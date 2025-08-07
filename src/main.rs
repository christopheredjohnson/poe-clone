use bevy::{color::palettes::css, prelude::*};
use bevy_ecs_tilemap::prelude::*;
use pathfinding::prelude::astar;
use bevy_prototype_lyon::prelude::*;


const TILE_SIZE: f32 = 32.0;

#[derive(Resource)]
pub struct WalkableMap {
    pub width: i32,
    pub height: i32,
    pub grid: Vec<Vec<bool>>,
}

impl WalkableMap {
    pub fn is_walkable(&self, pos: TilePos) -> bool {
        if pos.x >= self.width as u32 || pos.y >= self.height as u32 {
            return false;
        }
        self.grid[pos.y as usize][pos.x as usize]
    }
}

#[derive(Resource)]
pub struct TileMap {
    pub width: u32,
    pub height: u32,
    pub tiles: Vec<Vec<u32>>,
}

impl TileMap {
    pub fn from_csv(csv_content: &str) -> Result<Self, String> {
        let lines: Vec<&str> = csv_content.trim().lines().collect();
        if lines.is_empty() {
            return Err("Empty CSV file".to_string());
        }

        let height = lines.len() as u32;
        let mut tiles = Vec::new();
        let mut width = 0;

        for (row_idx, line) in lines.iter().enumerate() {
            let row: Result<Vec<u32>, _> =
                line.split(',').map(|s| s.trim().parse::<u32>()).collect();

            match row {
                Ok(row_data) => {
                    if row_idx == 0 {
                        width = row_data.len() as u32;
                    } else if row_data.len() != width as usize {
                        return Err(format!("Inconsistent row length at row {}", row_idx));
                    }
                    tiles.push(row_data);
                }
                Err(e) => return Err(format!("Parse error at row {}: {}", row_idx, e)),
            }
        }

        // Reverse the tiles since bevy_ecs_tilemap uses bottom-left origin
        tiles.reverse();

        Ok(TileMap {
            width,
            height,
            tiles,
        })
    }

    pub fn get_tile(&self, x: u32, y: u32) -> Option<u32> {
        if x < self.width && y < self.height {
            Some(self.tiles[y as usize][x as usize])
        } else {
            None
        }
    }

    pub fn is_walkable(&self, x: u32, y: u32) -> bool {
        match self.get_tile(x, y) {
            Some(tile_id) => tile_id != 1, // Assuming tile ID 1 is non-walkable
            None => false,
        }
    }
}

#[derive(Component)]
struct Player;

#[derive(Component, Default)]
pub struct PathQueue(pub Vec<Vec2>);

#[derive(Resource)]
struct TilemapTransform(Transform);

#[derive(Resource)]
struct MapLoaded(bool);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(ImagePlugin::default_nearest()))
        .add_plugins(TilemapPlugin)
        .insert_resource(ClearColor(Color::BLACK))
        .insert_resource(MapLoaded(false))
        .add_systems(Startup, setup_camera)
        .add_systems(
            Update,
            (
                load_map_from_csv,
                handle_mouse_click_to_move,
                move_along_path,
                camera_follow_player,
                draw_path_gizmos,
            ),
        )
        .run();
}

fn setup_camera(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}

fn draw_path_gizmos(
    mut gizmos: Gizmos,
    path_q: Query<&PathQueue, With<Player>>,
) {
    if let Ok(path) = path_q.get_single() {
        for point in &path.0 {
            gizmos.circle_2d(*point, 3.0, css::YELLOW);
        }
    }
}


fn load_map_from_csv(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    map_loaded: Res<MapLoaded>,
) {
    if map_loaded.0 {
        return;
    }

    // Example CSV data - in a real game, you'd load this from a file
    let csv_data = r#"0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"#;

    let tilemap = match TileMap::from_csv(csv_data) {
        Ok(map) => map,
        Err(e) => {
            error!("Failed to load CSV map: {}", e);
            return;
        }
    };

    let map_size = TilemapSize {
        x: tilemap.width,
        y: tilemap.height,
    };
    let tile_size = TilemapTileSize {
        x: TILE_SIZE,
        y: TILE_SIZE,
    };
    let grid_size = tile_size.into();
    let map_type = TilemapType::Square;

    let texture_handle: Handle<Image> = asset_server.load("tiles.png");
    let tilemap_transform = get_tilemap_center_transform(&map_size, &grid_size, &map_type, 0.0);

    commands.insert_resource(TilemapTransform(tilemap_transform));

    let tilemap_entity = commands.spawn_empty().id();
    let mut tile_storage = TileStorage::empty(map_size);
    let mut walkable_grid = vec![vec![true; tilemap.width as usize]; tilemap.height as usize];

    // Create tiles from CSV data
    for x in 0..tilemap.width {
        for y in 0..tilemap.height {
            let tile_pos = TilePos { x, y };
            let tile_id = tilemap.get_tile(x, y).unwrap_or(0);

            // Update walkable grid
            walkable_grid[y as usize][x as usize] = tilemap.is_walkable(x, y);

            let tile_entity = commands
                .spawn(TileBundle {
                    position: tile_pos,
                    tilemap_id: TilemapId(tilemap_entity),
                    texture_index: TileTextureIndex(tile_id),
                    ..default()
                })
                .id();

            tile_storage.set(&tile_pos, tile_entity);
        }
    }

    commands.entity(tilemap_entity).insert(TilemapBundle {
        grid_size,
        map_type,
        size: map_size,
        storage: tile_storage,
        texture: TilemapTexture::Single(texture_handle),
        tile_size,
        transform: tilemap_transform,
        ..default()
    });

    commands.insert_resource(WalkableMap {
        width: tilemap.width as i32,
        height: tilemap.height as i32,
        grid: walkable_grid,
    });

    // Spawn player at tile (0, 0)
    let start_tile = TilePos { x: 0, y: 0 };
    let tile_center_local = Vec2::new(TILE_SIZE / 2.0, TILE_SIZE / 2.0);
    let start_world = tile_center_local + tilemap_transform.translation.truncate();

    commands.spawn((
        // SpatialBundle {
        //     transform: Transform::from_translation(start_world.extend(1.0)),
        //     ..default()
        // },
        SpriteBundle {
            sprite: Sprite {
                custom_size: Some(Vec2::splat(TILE_SIZE - 2.0)),
                color: Color::rgb(0.2, 0.2, 1.0),
                ..default()
            },
            transform: Transform::from_translation(start_world.extend(1.0)),
            ..default()
        },
        Player,
        PathQueue::default(),
    ));

    commands.insert_resource(MapLoaded(true));
    info!(
        "Map loaded successfully: {}x{}",
        tilemap.width, tilemap.height
    );
}

fn handle_mouse_click_to_move(
    mouse: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    map: Option<Res<WalkableMap>>,
    tilemap_transform: Option<Res<TilemapTransform>>,
    mut commands: Commands,
    player_q: Query<(&Transform, Entity), With<Player>>,
) {
    // Early return if map isn't loaded yet
    let Some(map) = map else { return };
    let Some(tilemap_transform) = tilemap_transform else {
        return;
    };

    if !mouse.just_pressed(MouseButton::Left) {
        return;
    }

    let Ok(window) = windows.get_single() else {
        return;
    };
    let Ok((camera, cam_tf)) = camera_q.get_single() else {
        return;
    };
    let Some(cursor_pos) = window.cursor_position() else {
        return;
    };
    let Some(world_pos) = camera.viewport_to_world(cam_tf, cursor_pos) else {
        return;
    };

    let clicked_pos = world_pos.origin.truncate() - tilemap_transform.0.translation.truncate();
    let clicked_tile = world_to_tile_pos(clicked_pos);

    let Ok((player_tf, entity)) = player_q.get_single() else {
        return;
    };
    let player_pos = player_tf.translation.truncate() - tilemap_transform.0.translation.truncate();
    let player_tile = world_to_tile_pos(player_pos);

    if let Some(path) = find_path(player_tile, clicked_tile, &map) {
        let world_path: Vec<Vec2> = path
            .into_iter()
            .map(|tile_pos| tile_pos_to_world_centered(tile_pos, &tilemap_transform.0))
            .collect();

        commands.entity(entity).insert(PathQueue(world_path));
    }
}

fn move_along_path(
    time: Res<Time>,
    mut commands: Commands,
    mut q: Query<(Entity, &mut Transform, &mut PathQueue), With<Player>>,
) {
    let Ok((entity, mut tf, mut path)) = q.get_single_mut() else {
        return;
    };

    let speed = 100.0;

    if let Some(next_target) = path.0.first() {
        let pos = tf.translation.truncate();
        let dir = (*next_target - pos).normalize_or_zero();
        tf.translation += (dir * speed * time.delta_seconds()).extend(0.0);

        if pos.distance(*next_target) < 2.0 {
            path.0.remove(0);
        }
    } else if !path.0.is_empty() {
        commands.entity(entity).remove::<PathQueue>();
    }
}


fn camera_follow_player(
    mut cam_q: Query<&mut Transform, (With<Camera>, Without<Player>)>,
    player_q: Query<&Transform, With<Player>>,
) {
    if let (Ok(mut cam_tf), Ok(player_tf)) = (cam_q.get_single_mut(), player_q.get_single()) {
        cam_tf.translation.x = player_tf.translation.x;
        cam_tf.translation.y = player_tf.translation.y;
    }
}

// === Utility Functions ===

fn world_to_tile_pos(pos: Vec2) -> TilePos {
    TilePos {
        x: ((pos.x + TILE_SIZE / 2.0) / TILE_SIZE).floor().max(0.0) as u32,
        y: ((pos.y + TILE_SIZE / 2.0) / TILE_SIZE).floor().max(0.0) as u32,
    }
}

fn tile_pos_to_world_centered(tile: TilePos, tilemap_transform: &Transform) -> Vec2 {
    let local_pos = Vec2::new(tile.x as f32 * TILE_SIZE, tile.y as f32 * TILE_SIZE);
    local_pos + tilemap_transform.translation.truncate()
}

fn find_path(start: TilePos, end: TilePos, map: &WalkableMap) -> Option<Vec<TilePos>> {
    astar(
        &start,
        |p| {
            let mut neighbors = vec![];
            for (dx, dy) in [
                (-1, 0),
                (1, 0),
                (0, -1),
                (0, 1),
                (-1, -1),
                (1, -1),
                (-1, 1),
                (1, 1),
            ] {
                let new_x = p.x.wrapping_add_signed(dx);
                let new_y = p.y.wrapping_add_signed(dy);
                let next = TilePos { x: new_x, y: new_y };
                if map.is_walkable(next) {
                    neighbors.push((next, 1));
                }
            }
            neighbors
        },
        |p| (p.x.abs_diff(end.x) + p.y.abs_diff(end.y)),
        |p| *p == end,
    )
    .map(|(path, _)| path)
}

// === File Loading Helper (for when you want to load from actual files) ===

#[allow(dead_code)]
fn load_csv_from_file(file_path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(file_path)
}
