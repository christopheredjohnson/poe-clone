// main.rs
use bevy::{color::palettes::css, prelude::*};
use bevy_ecs_tilemap::helpers::geometry::get_tilemap_center_transform;
use bevy_ecs_tilemap::prelude::*;
use pathfinding::prelude::astar;

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

#[derive(Resource, Clone)]
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
        // Bottom-left origin for bevy_ecs_tilemap
        tiles.reverse();
        Ok(TileMap { width, height, tiles })
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
            Some(tile_id) => tile_id != 1, // 1 = wall (example)
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

#[derive(Component)]
struct HoveredTile;

#[derive(Component)]
struct ContextMenuRoot;

#[derive(Resource, Default)]
struct ContextMenuState {
    visible: bool,
}
#[derive(Resource, Default, Clone, Copy)]
struct LastInspectedTile(Option<TilePos>);

// Text tags (avoid multi-mutable query conflicts)
#[derive(Component)] struct TxtTile;
#[derive(Component)] struct TxtWalkable;
#[derive(Component)] struct TxtTileId;

#[derive(Component, Clone, Copy)]
enum ContextAction {
    MoveHere,
    Close,
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(ImagePlugin::default_nearest()))
        .add_plugins(TilemapPlugin)
        .insert_resource(ClearColor(Color::BLACK))
        .insert_resource(MapLoaded(false))
        .insert_resource(LastInspectedTile::default())
        .add_systems(Startup, (setup_camera, setup_context_menu))
        .add_systems(
            Update,
            (
                load_map_from_csv,
                handle_mouse_click_to_move,
                move_along_path,
                camera_follow_player,
                draw_path_gizmos,
                highlight_hovered_tile,
                show_context_menu_on_right_click,
                hide_context_menu_on_left_click_or_escape,
                context_menu_button_interactions,
            ),
        )
        .run();
}

fn setup_camera(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}

fn setup_context_menu(mut commands: Commands) {
    let panel_bg = Color::rgba(0.08, 0.08, 0.1, 0.9);
    commands
        .spawn((
            NodeBundle {
                style: Style {
                    position_type: PositionType::Absolute,
                    width: Val::Px(200.0),
                    min_height: Val::Px(120.0),
                    padding: UiRect::all(Val::Px(8.0)),
                    flex_direction: FlexDirection::Column,
                    row_gap: Val::Px(6.0),
                    ..default()
                },
                background_color: panel_bg.into(),
                visibility: Visibility::Hidden,
                ..default()
            },
            ContextMenuRoot,
        ))
        .with_children(|parent| {
            parent.spawn((
                TextBundle::from_section(
                    "Tile: -, -",
                    TextStyle { font_size: 16.0, color: Color::WHITE, ..default() },
                ),
                TxtTile,
            ));
            parent.spawn((
                TextBundle::from_section(
                    "Walkable: -",
                    TextStyle { font_size: 14.0, color: Color::rgb(0.9, 0.9, 0.9), ..default() },
                ),
                TxtWalkable,
            ));
            parent.spawn((
                TextBundle::from_section(
                    "Tile ID: -",
                    TextStyle { font_size: 14.0, color: Color::rgb(0.9, 0.9, 0.9), ..default() },
                ),
                TxtTileId,
            ));

            // Buttons container
            parent
                .spawn(NodeBundle {
                    style: Style {
                        margin: UiRect::top(Val::Px(8.0)),
                        flex_direction: FlexDirection::Column,
                        row_gap: Val::Px(6.0),
                        ..default()
                    },
                    ..default()
                })
                .with_children(|buttons| {
                    spawn_menu_button(buttons, "Move here", ContextAction::MoveHere);
                    spawn_menu_button(buttons, "Close", ContextAction::Close);
                });
        });

    commands.insert_resource(ContextMenuState::default());
}

fn spawn_menu_button(parent: &mut ChildBuilder, label: &str, action: ContextAction) {
    parent
        .spawn((
            ButtonBundle {
                style: Style {
                    width: Val::Percent(100.0),
                    padding: UiRect::all(Val::Px(6.0)),
                    ..default()
                },
                background_color: Color::rgba(0.2, 0.2, 0.25, 0.9).into(),
                ..default()
            },
            action,
        ))
        .with_children(|b| {
            b.spawn(TextBundle::from_section(
                label,
                TextStyle { font_size: 14.0, color: Color::WHITE, ..default() },
            ));
        });
}

fn draw_path_gizmos(mut gizmos: Gizmos, path_q: Query<&PathQueue, With<Player>>) {
    if let Ok(path) = path_q.get_single() {
        for point in &path.0 {
            gizmos.circle_2d(*point, 3.0, css::YELLOW);
        }
        for w in path.0.windows(2) {
            gizmos.line_2d(w[0], w[1], css::ORANGE);
        }
    }
}

fn highlight_hovered_tile(
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    tilemap_transform: Option<Res<TilemapTransform>>,
    map: Option<Res<WalkableMap>>,
    mut commands: Commands,
    mut hovered_q: Query<(&mut Transform, &mut Sprite), With<HoveredTile>>,
) {
    let Some(tilemap_transform) = tilemap_transform else { return; };
    let Some(map) = map else { return; };
    let Ok(window) = windows.get_single() else { return; };
    let Ok((camera, cam_tf)) = camera_q.get_single() else { return; };
    let Some(cursor_pos) = window.cursor_position() else { return; };
    let Some(world_pos) = camera.viewport_to_world(cam_tf, cursor_pos) else { return; };

    let hovered_pos = world_pos.origin.truncate() - tilemap_transform.0.translation.truncate();
    let hovered_tile = world_to_tile_pos(hovered_pos);
    let walkable = map.is_walkable(hovered_tile);
    let color = if walkable {
        Color::rgba(0.0, 1.0, 0.0, 0.35)
    } else {
        Color::rgba(1.0, 0.0, 0.0, 0.35)
    };

    let new_world_pos = tile_pos_to_world_centered(hovered_tile, &tilemap_transform.0).extend(2.0);

    if let Ok((mut tf, mut sprite)) = hovered_q.get_single_mut() {
        tf.translation = new_world_pos;
        sprite.color = color;
    } else {
        commands.spawn((
            SpriteBundle {
                sprite: Sprite {
                    color,
                    custom_size: Some(Vec2::splat(TILE_SIZE)),
                    ..default()
                },
                transform: Transform::from_translation(new_world_pos),
                ..default()
            },
            HoveredTile,
        ));
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

    // Example CSV (0 = floor, 1 = wall)
    let csv_data = r#"0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0
                    0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0
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

    // Create tiles
    for x in 0..tilemap.width {
        for y in 0..tilemap.height {
            let tile_pos = TilePos { x, y };
            let tile_id = tilemap.get_tile(x, y).unwrap_or(0);
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

    // Make tilemap available to the UI for inspection
    commands.insert_resource(tilemap.clone());

    // Spawn player at (0,0)
    let start_world = Vec2::new(TILE_SIZE / 2.0, TILE_SIZE / 2.0)
        + tilemap_transform.translation.truncate();

    commands.spawn((
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
    info!("Map loaded successfully: {}x{}", map_size.x, map_size.y);
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
    let Some(map) = map else { return };
    let Some(tilemap_transform) = tilemap_transform else { return };
    if !mouse.just_pressed(MouseButton::Left) { return; }

    let Ok(window) = windows.get_single() else { return };
    let Ok((camera, cam_tf)) = camera_q.get_single() else { return };
    let Some(cursor_pos) = window.cursor_position() else { return };
    let Some(world_pos) = camera.viewport_to_world(cam_tf, cursor_pos) else { return };

    let clicked_pos = world_pos.origin.truncate() - tilemap_transform.0.translation.truncate();
    let clicked_tile = world_to_tile_pos(clicked_pos);

    let Ok((player_tf, entity)) = player_q.get_single() else { return };
    let player_pos = player_tf.translation.truncate() - tilemap_transform.0.translation.truncate();
    let player_tile = world_to_tile_pos(player_pos);

    if let Some(path) = find_path(player_tile, clicked_tile, &map) {
        let world_path: Vec<Vec2> = path
            .into_iter()
            .map(|tp| tile_pos_to_world_centered(tp, &tilemap_transform.0))
            .collect();
        commands.entity(entity).insert(PathQueue(world_path));
    }
}

fn move_along_path(
    time: Res<Time>,
    mut commands: Commands,
    mut q: Query<(Entity, &mut Transform, &mut PathQueue), With<Player>>,
) {
    let Ok((entity, mut tf, mut path)) = q.get_single_mut() else { return };
    let speed = 100.0;

    if let Some(next_target) = path.0.first() {
        let pos = tf.translation.truncate();
        let dir = (*next_target - pos).normalize_or_zero();
        tf.translation += (dir * speed * time.delta_seconds()).extend(0.0);
        if pos.distance(*next_target) < 2.0 {
            path.0.remove(0);
        }
    } else {
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

fn show_context_menu_on_right_click(
    mouse: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    tilemap_tf: Option<Res<TilemapTransform>>,
    map_walk: Option<Res<WalkableMap>>,
    map_tiles: Option<Res<TileMap>>,
    mut menu_state: ResMut<ContextMenuState>,
    mut last_tile: ResMut<LastInspectedTile>,
    mut q_menu: Query<(&mut Style, &mut Visibility), With<ContextMenuRoot>>,
    mut text_sets: ParamSet<(
        Query<&mut Text, With<TxtTile>>,
        Query<&mut Text, With<TxtWalkable>>,
        Query<&mut Text, With<TxtTileId>>,
    )>,
) {
    if !mouse.just_pressed(MouseButton::Right) { return; }

    let Some(tilemap_tf) = tilemap_tf else { return; };
    let Ok(window) = windows.get_single() else { return; };
    let Ok((camera, cam_tf)) = camera_q.get_single() else { return; };
    let Some(cursor_pos) = window.cursor_position() else { return; };
    let Some(world) = camera.viewport_to_world(cam_tf, cursor_pos) else { return; };

    let local = world.origin.truncate() - tilemap_tf.0.translation.truncate();
    let tile = world_to_tile_pos(local);
    last_tile.0 = Some(tile);

    let walkable = map_walk.as_ref().map(|m| m.is_walkable(tile)).unwrap_or(false);
    let tile_id = map_tiles.as_ref().and_then(|t| t.get_tile(tile.x, tile.y)).unwrap_or(u32::MAX);

    // Clamp menu pos
    let menu_w = 200.0;
    let menu_h = 140.0;
    let mut left = cursor_pos.x;
    let mut top = window.height() - cursor_pos.y;
    if left + menu_w > window.width() { left = window.width() - menu_w; }
    if top + menu_h > window.height() { top = window.height() - menu_h; }
    if left < 0.0 { left = 0.0; }
    if top < 0.0 { top = 0.0; }

    if let Ok((mut style, mut vis)) = q_menu.get_single_mut() {
        style.left = Val::Px(left);
        style.top = Val::Px(top);

        if let Ok(mut t) = text_sets.p0().get_single_mut() {
            t.sections[0].value = format!("Tile: {}, {}", tile.x, tile.y);
        }
        if let Ok(mut t) = text_sets.p1().get_single_mut() {
            t.sections[0].value = format!("Walkable: {}", walkable);
        }
        if let Ok(mut t) = text_sets.p2().get_single_mut() {
            t.sections[0].value = format!("Tile ID: {}", tile_id);
        }

        *vis = Visibility::Visible;
        menu_state.visible = true;
    }
}

fn hide_context_menu_on_left_click_or_escape(
    mouse: Res<ButtonInput<MouseButton>>,
    keys: Res<ButtonInput<KeyCode>>,
    mut menu_state: ResMut<ContextMenuState>,
    mut q_menu: Query<&mut Visibility, With<ContextMenuRoot>>,
) {
    if !(mouse.just_pressed(MouseButton::Left) || keys.just_pressed(KeyCode::Escape)) {
        return;
    }
    if let Ok(mut vis) = q_menu.get_single_mut() {
        *vis = Visibility::Hidden;
        menu_state.visible = false;
    }
}

fn context_menu_button_interactions(
    mut q_buttons: Query<
        (&Interaction, &mut BackgroundColor, &ContextAction),
        (Changed<Interaction>, With<Button>)
    >,
    mut menu_state: ResMut<ContextMenuState>,
    last_tile: Res<LastInspectedTile>,
    map: Option<Res<WalkableMap>>,
    tilemap_tf: Option<Res<TilemapTransform>>,
    mut player_q: Query<(&Transform, Entity), With<Player>>,
    mut q_menu: Query<&mut Visibility, With<ContextMenuRoot>>,
    mut commands: Commands,
) {
    for (interaction, mut bg, action) in &mut q_buttons {
        match *interaction {
            Interaction::Pressed => {
                *bg = Color::rgba(0.35, 0.35, 0.45, 0.95).into();

                match action {
                    ContextAction::MoveHere => {
                        if let (Some(map), Some(tilemap_tf), Ok((player_tf, entity))) =
                            (map.as_ref(), tilemap_tf.as_ref(), player_q.get_single_mut())
                        {
                            if let Some(target) = last_tile.0 {
                                let player_pos = player_tf.translation.truncate()
                                    - tilemap_tf.0.translation.truncate();
                                let player_tile = world_to_tile_pos(player_pos);
                                if let Some(path) = find_path(player_tile, target, map) {
                                    let world_path: Vec<Vec2> = path
                                        .into_iter()
                                        .map(|tp| tile_pos_to_world_centered(tp, &tilemap_tf.0))
                                        .collect();
                                    commands.entity(entity).insert(PathQueue(world_path));
                                }
                            }
                        }
                    }
                    ContextAction::Close => { /* nothing */ }
                }

                if let Ok(mut vis) = q_menu.get_single_mut() {
                    *vis = Visibility::Hidden;
                    menu_state.visible = false;
                }
            }
            Interaction::Hovered => {
                *bg = Color::rgba(0.28, 0.28, 0.36, 0.95).into();
            }
            Interaction::None => {
                *bg = Color::rgba(0.2, 0.2, 0.25, 0.9).into();
            }
        }
    }
}

// === Utility ===
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

                // Block diagonal corner-cutting
                if dx != 0 && dy != 0 {
                    let adj1 = TilePos { x: p.x.wrapping_add_signed(dx), y: p.y };
                    let adj2 = TilePos { x: p.x, y: p.y.wrapping_add_signed(dy) };
                    if !map.is_walkable(adj1) || !map.is_walkable(adj2) {
                        continue;
                    }
                }

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

// Optional file loading helper
#[allow(dead_code)]
fn load_csv_from_file(file_path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(file_path)
}
