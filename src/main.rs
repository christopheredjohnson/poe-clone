// Bevy 2D PoE‑like — Click‑to‑Move with bevy_ecs_tilemap walls + path smoothing
// Bevy 0.16.x, bevy_ecs_tilemap 0.13.x
// Controls:
//   LMB = move (A* over tilemap; smoothed by greedy line‑of‑sight)
//   RMB = cast Fireball toward cursor (LMP support demo)

use bevy::{color::palettes::css, prelude::*, render::{render_asset::RenderAssetUsages, render_resource::{Extent3d, TextureDimension, TextureFormat}, texture::ImageSampler}};
use bevy_ecs_tilemap::prelude::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "PoE‑like 2D — C2M + Tilemap Obstacles".into(),
                resolution: (1280., 720.).into(),
                resizable: true,
                ..default()
            }),
            ..default()
        }))
        .add_plugins(TilemapPlugin)
        .insert_resource(ClearColor(Color::linear_rgb(0.06, 0.06, 0.07)))
        .insert_resource(PlayerLoadout::default())
        .add_event::<DamageEvent>()
        .add_systems(Startup, (setup_cameras, setup_tilemap_and_nav, spawn_player, spawn_dummy_pack))
        .add_systems(
            Update,
            (
                click_to_move,           // LEFT click → set path
                follow_path,             // move along A* path
                player_cast_fireball,    // RIGHT click → cast
                projectile_motion,
                projectile_hit_detection,
                death_and_cleanup,
                ui_health_text,
            ),
        )
        .run();
}

// === Core data ===
#[derive(Component)]
struct Player;

#[derive(Component)]
struct Enemy;

#[derive(Component)]
struct Health { hp: f32, max: f32 }

#[derive(Event, Debug, Clone, Copy)]
struct DamageEvent { target: Entity, amount: f32, is_crit: bool }

#[derive(Component)]
struct Velocity(Vec2);

#[derive(Component)]
struct Projectile { life_secs: f32 }

#[derive(Component)]
struct Hitbox { radius: f32 }

#[derive(Component)]
struct Faction(FactionKind);

#[derive(Clone, Copy, PartialEq)]
enum FactionKind { Player, Enemy }

#[derive(Resource)]
struct PlayerLoadout {
    fireball_lmp: Option<Lmp>,
    crit_chance: f32,  // 0..1
    crit_multi: f32,   // 1.5 = +50%
    spell_power: f32,  // generic scaler
}

impl Default for PlayerLoadout {
    fn default() -> Self {
        Self {
            fireball_lmp: Some(Lmp { extra: 2, angle_deg: 15.0 }),
            crit_chance: 0.08,
            crit_multi: 1.5,
            spell_power: 1.0,
        }
    }
}

#[derive(Clone, Copy)]
struct Lmp { extra: u32, angle_deg: f32 }

// === UI ===
#[derive(Component)]
struct UiHealth;


#[derive(Component)]
pub struct Path {
    pub points: Vec<Vec2>,  // sequence of waypoints in world-space
    pub index: usize,       // current target point index
    pub speed: f32,         // units per second
}

impl Default for Path {
    fn default() -> Self {
        Self {
            points: Vec::new(),
            index: 0,
            speed: 240.0,
        }
    }
}

fn ui_health_text(mut q_text: Query<&mut Text, With<UiHealth>>, q_player: Query<&Health, With<Player>>) {
    if let (Ok(mut text), Ok(h)) = (q_text.get_single_mut(), q_player.get_single()) {
        text.sections[0].value = format!("HP: {}/{}  |  LMB=Move  RMB=Fireball", h.hp as i32, h.max as i32);
    }
}

// === Setup ===
fn setup_cameras(mut commands: Commands) { commands.spawn(Camera2dBundle::default()); }

fn spawn_player(mut commands: Commands, assets: Res<AssetServer>) {
    let size = Vec2::new(24., 24.);
    commands
        .spawn(SpriteBundle {
            sprite: Sprite { color: Color::rgba(0.8, 0.9, 1.0, 1.0), custom_size: Some(size), ..default() },
            transform: Transform::from_xyz(0., 0., 10.),
            ..default()
        })
        .insert(Player)
        .insert(Health { hp: 100., max: 100. })
        .insert(Hitbox { radius: 12. })
        .insert(Faction(FactionKind::Player))
        .insert(Path { points: vec![], index: 0, speed: 240.0 });

    // UI health text
    commands.spawn((TextBundle::from_section(
        "HP: 100/100",
        TextStyle { font_size: 20.0, color: Color::WHITE, ..default() },
    )
    .with_style(Style { position_type: PositionType::Absolute, top: Val::Px(8.0), left: Val::Px(10.0), ..default() }), UiHealth));
}

fn spawn_dummy_pack(mut commands: Commands) {
    for i in 0..6 {
        let x = -160. + i as f32 * 50.0;
        let y = 140. + (i % 2) as f32 * 30.0;
        commands
            .spawn(SpriteBundle {
                sprite: Sprite { color: Color::rgba(1.0, 0.45, 0.45, 1.0), custom_size: Some(Vec2::splat(20.0)), ..default() },
                transform: Transform::from_xyz(x, y, 5.),
                ..default()
            })
            .insert(Enemy)
            .insert(Health { hp: 30., max: 30. })
            .insert(Hitbox { radius: 10. })
            .insert(Faction(FactionKind::Enemy));
    }
}

// === Tilemap + NavGrid ===
#[derive(Resource, Clone)]
struct NavGrid {
    origin: Vec2, // world‑space reference (top‑left of grid)
    cols: i32,
    rows: i32,
    cell: f32,    // cell size in pixels
    walkable: Vec<bool>, // cols*rows
}

impl NavGrid {
    fn in_bounds(&self, c: IVec2) -> bool { c.x >= 0 && c.y >= 0 && c.x < self.cols && c.y < self.rows }
    fn idx(&self, c: IVec2) -> usize { (c.y * self.cols + c.x) as usize }
    fn is_walkable(&self, c: IVec2) -> bool { self.in_bounds(c) && self.walkable[self.idx(c)] }
    fn pos_to_cell(&self, p: Vec2) -> IVec2 {
        let rel = p - self.origin; IVec2::new((rel.x / self.cell).floor() as i32, (rel.y / self.cell).floor() as i32)
    }
    fn cell_center(&self, c: IVec2) -> Vec2 { self.origin + (c.as_vec2() + Vec2::splat(0.5)) * self.cell }
    fn neighbors4(&self, c: IVec2) -> impl Iterator<Item = IVec2> + '_ {
        [IVec2::new(1,0), IVec2::new(-1,0), IVec2::new(0,1), IVec2::new(0,-1)]
            .into_iter().map(move |d| c + d).filter(|&n| self.is_walkable(n))
    }
}

fn setup_tilemap_and_nav(mut commands: Commands, mut images: ResMut<Assets<Image>>) {
    // Grid config
    let cols = 40; let rows = 24; let cell = 32.0;
    let map_size = TilemapSize { x: cols as u32, y: rows as u32 };
    let grid_size = TilemapGridSize { x: cell, y: cell };
    let map_type = TilemapType::Square;

   // 1x1 white texture for tiles
    let mut img = Image::new_fill(
        Extent3d { width: 1, height: 1, depth_or_array_layers: 1 },
        TextureDimension::D2,
        &[255, 255, 255, 255],
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::default(),
    );
    img.sampler = ImageSampler::nearest();
    
    let texture_handle = images.add(img);

    // Create map entity FIRST so tiles can reference its TilemapId
    let tile_storage = TileStorage::empty(map_size);
    let map_entity = commands
        .spawn(TilemapBundle {
            grid_size,
            size: map_size,
            map_type,
            storage: tile_storage.clone(),
            // texture: TilemapTexture::Single(texture_handle.clone()),
            transform: Transform::from_xyz(-(cols as f32 * cell) * 0.5, -(rows as f32 * cell) * 0.5, 0.0),
            ..default()
        })
        .id();

    // Build walkability + tiles: borders are walls; add a few pillars.
    let mut walkable = vec![true; (cols * rows) as usize];
    let mut storage = tile_storage; // mutable copy to fill
    let mut spawn_tile = |x: u32, y: u32, wall: bool, commands: &mut Commands| {
        let tile_pos = TilePos { x, y };
        let color = if wall { Color::srgb(0.10, 0.10, 0.12) } else { Color::srgb(0.18, 0.18, 0.22) };
        let tile_entity = commands
            .spawn(TileBundle {
                position: tile_pos,
                tilemap_id: TilemapId(map_entity),
                texture_index: TileTextureIndex(0),
                color: TileColor(color), // set via bundle (avoid duplicate component)
                ..default()
            })
            .id();
        if wall { walkable[(y as i32 * cols + x as i32) as usize] = false; }
        storage.set(&tile_pos, tile_entity);
    };

    // Fill map
    for y in 0..rows as u32 {
        for x in 0..cols as u32 {
            let border = x == 0 || y == 0 || x == (cols as u32 - 1) || y == (rows as u32 - 1);
            spawn_tile(x, y, border, &mut commands);
        }
    }
    for (x, y) in [(10,8), (10,9), (10,10), (20,12), (21,12), (22,12), (28,6), (28,7), (28,8)] {
        spawn_tile(x, y, true, &mut commands);
    }

    // Write storage back into map entity (since we mutated it)
    commands.entity(map_entity).insert(storage);

    // NavGrid mirrors tilemap
    commands.insert_resource(NavGrid {
        origin: Vec2::new(-(cols as f32 * cell) * 0.5, -(rows as f32 * cell) * 0.5),
        cols, rows, cell, walkable,
    });
}

// === A* + smoothing ===
fn a_star(nav: &NavGrid, start: IVec2, goal: IVec2) -> Option<Vec<IVec2>> {
    use std::cmp::Ordering; use std::collections::{BinaryHeap, HashMap};
    #[derive(Copy, Clone, Eq, PartialEq)] struct Node { c: IVec2, f: i32 }
    impl Ord for Node { fn cmp(&self, other: &Self) -> Ordering { other.f.cmp(&self.f) } }
    impl PartialOrd for Node { fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) } }
    let h = |a: IVec2, b: IVec2| (a.x - b.x).abs() + (a.y - b.y).abs();
    let mut open = BinaryHeap::new();
    let mut came: HashMap<IVec2, IVec2> = HashMap::new();
    let mut g: HashMap<IVec2, i32> = HashMap::new();
    g.insert(start, 0); open.push(Node { c: start, f: h(start, goal) });
    while let Some(Node { c, .. }) = open.pop() {
        if c == goal {
            let mut path = vec![c]; let mut cur = c;
            while let Some(&p) = came.get(&cur) { path.push(p); cur = p; }
            path.reverse(); return Some(path);
        }
        let gc = *g.get(&c).unwrap_or(&i32::MAX);
        for n in nav.neighbors4(c) {
            let tentative = gc.saturating_add(1);
            if tentative < *g.get(&n).unwrap_or(&i32::MAX) {
                came.insert(n, c); g.insert(n, tentative);
                let f = tentative + h(n, goal); open.push(Node { c: n, f });
            }
        }
    }
    None
}

fn line_of_sight(nav: &NavGrid, a: IVec2, b: IVec2) -> bool {
    // Grid Bresenham between centers; all traversed cells must be walkable
    let mut x0 = a.x; let mut y0 = a.y; let x1 = b.x; let y1 = b.y;
    let dx = (x1 - x0).abs(); let sx = if x0 < x1 { 1 } else { -1 };
    let dy = -(y1 - y0).abs(); let sy = if y0 < y1 { 1 } else { -1 };
    let mut err = dx + dy; // error value e_xy
    loop {
        if !nav.is_walkable(IVec2::new(x0, y0)) { return false; }
        if x0 == x1 && y0 == y1 { break; }
        let e2 = 2 * err;
        if e2 >= dy { err += dy; x0 += sx; }
        if e2 <= dx { err += dx; y0 += sy; }
    }
    true
}

fn smooth_cells(nav: &NavGrid, cells: &[IVec2]) -> Vec<IVec2> {
    if cells.is_empty() { return vec![]; }
    let mut out = vec![cells[0]]; let mut i = 0; let mut j = 1;
    while j < cells.len() {
        // extend j as far as LOS from i holds
        while j + 1 < cells.len() && line_of_sight(nav, cells[i], cells[j + 1]) { j += 1; }
        out.push(cells[j]); i = j; j += 1;
    }
    out
}

#[derive(Component)]
struct PathMarker;

fn click_to_move(
    buttons: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    nav: Res<NavGrid>,
    mut q_player: Query<(&Transform, &mut Path), With<Player>>,
    mut commands: Commands,
    existing_markers: Query<Entity, With<PathMarker>>,
) {
    if !buttons.just_pressed(MouseButton::Left) { return; }
    let Ok((player_t, mut path)) = q_player.get_single_mut() else { return; };
    let (camera, cam_xform) = camera_q.single();
    let window = windows.single();
    if let Some(cursor_pos) = window.cursor_position() {
        if let Some(ray) = camera.viewport_to_world(cam_xform, cursor_pos) {
            let target_world = ray.origin.truncate();
            let start_c = nav.pos_to_cell(player_t.translation.truncate());
            let goal_c  = nav.pos_to_cell(target_world);
            if !nav.is_walkable(goal_c) { return; }
            if let Some(cells) = a_star(&nav, start_c, goal_c) {
                let cells = smooth_cells(&nav, &cells);
                let pts: Vec<Vec2> = cells.into_iter().map(|c| nav.cell_center(c)).collect();
                path.points = pts.clone();
                path.index = 0;

                // === Path preview ===
                // Clear previous markers
                for e in &existing_markers { commands.entity(e).despawn_recursive(); }
                // Spawn small dots at each waypoint (skip very first since it's on the player)
                for (i, p) in path.points.iter().enumerate() {
                    if i == 0 { continue; }
                    commands.spawn((
                        SpriteBundle {
                            sprite: Sprite { color: Color::srgb(0.35, 0.75, 1.0), custom_size: Some(Vec2::splat(6.0)), ..default() },
                            transform: Transform::from_xyz(p.x, p.y, 7.0),
                            ..default()
                        },
                        PathMarker,
                    ));
                }
            }
        }
    }
}

fn follow_path(
    time: Res<Time>,
    mut q: Query<(&mut Transform, &mut Path), With<Player>>,
) {
    let Ok((mut t, mut p)) = q.get_single_mut() else { return; };
    if p.index >= p.points.len() { return; }

    let target = p.points[p.index];
    let cur = t.translation.truncate();
    let to = target - cur;
    let dist = to.length();
    let step = p.speed * time.delta_seconds();

    if dist <= step {
        t.translation.x = target.x;
        t.translation.y = target.y;
        p.index += 1;
    } else {
        let dir = to / dist;
        t.translation.x += dir.x * step;
        t.translation.y += dir.y * step;
    }
}

// === Combat & Skills ===
#[derive(Resource, Default)]
struct Cooldowns { fireball: f32 }

fn player_cast_fireball(
    buttons: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    time: Res<Time>,
    mut cooldowns: Local<Cooldowns>,
    loadout: Res<PlayerLoadout>,
    mut commands: Commands,
    q_player: Query<&Transform, With<Player>>,
) {
    cooldowns.fireball -= time.delta_seconds();
    if cooldowns.fireball > 0.0 { return; }
    if !buttons.just_pressed(MouseButton::Right) { return; }

    let Ok(player_t) = q_player.get_single() else { return; };
    let (camera, cam_xform) = camera_q.single();
    let window = windows.single();

    if let Some(cursor_pos) = window.cursor_position() {
        if let Some(ray) = camera.viewport_to_world(cam_xform, cursor_pos) {
            let target = ray.origin.truncate();
            let origin = player_t.translation.truncate();
            let dir = (target - origin).normalize_or_zero();

            let mut angles = vec![0.0_f32];
            if let Some(lmp) = loadout.fireball_lmp { if lmp.extra >= 2 { angles.push(lmp.angle_deg.to_radians()); angles.push(-lmp.angle_deg.to_radians()); } }
            for a in angles { let v = Mat2::from_angle(a).mul_vec2(dir) * 420.0; spawn_fireball(&mut commands, origin, v, loadout.spell_power, loadout.crit_chance, loadout.crit_multi); }
            cooldowns.fireball = 0.35;
        }
    }
}

fn spawn_fireball(commands: &mut Commands, origin: Vec2, velocity: Vec2, spell_power: f32, crit_chance: f32, crit_multi: f32) {
    commands
        .spawn(SpriteBundle {
            sprite: Sprite { color: css::ORANGE_RED.into(), custom_size: Some(Vec2::splat(12.0)), ..default() },
            transform: Transform::from_xyz(origin.x, origin.y, 8.0),
            ..default()
        })
        .insert(Projectile { life_secs: 1.2 })
        .insert(Velocity(velocity))
        .insert(Hitbox { radius: 8.0 })
        .insert(Faction(FactionKind::Player))
        .insert(Fireball { base: 22.0 * spell_power, crit_chance, crit_multi });
}

#[derive(Component)]
struct Fireball { base: f32, crit_chance: f32, crit_multi: f32 }

fn projectile_motion(time: Res<Time>, mut commands: Commands, mut q: Query<(Entity, &mut Transform, &mut Projectile, &Velocity)>) {
    for (e, mut t, mut proj, v) in &mut q {
        t.translation.x += v.0.x * time.delta_seconds();
        t.translation.y += v.0.y * time.delta_seconds();
        proj.life_secs -= time.delta_seconds();
        if proj.life_secs <= 0.0 { commands.entity(e).despawn_recursive(); }
    }
}

fn projectile_hit_detection(
    mut commands: Commands,
    mut ev_damage: EventWriter<DamageEvent>,
    q_proj: Query<(Entity, &Transform, &Hitbox, &Faction, Option<&Fireball>)>,
    mut q_targets: Query<(Entity, &Transform, &Hitbox, &Faction), With<Health>>,
) {
    for (pe, pt, ph, pf, fball) in &q_proj {
        for (te, tt, th, tf) in &mut q_targets {
            if pf.0 == tf.0 { continue; }
            let d2 = pt.translation.truncate().distance_squared(tt.translation.truncate());
            let r = ph.radius + th.radius;
            if d2 <= r * r {
                if let Some(fb) = fball {
                    let is_crit = rand::random::<f32>() < fb.crit_chance;
                    let mut dmg = fb.base; if is_crit { dmg *= fb.crit_multi; }
                    ev_damage.send(DamageEvent { target: te, amount: dmg, is_crit });
                }
                commands.entity(pe).despawn_recursive();
                break;
            }
        }
    }
}

fn death_and_cleanup(mut commands: Commands, mut evr: EventReader<DamageEvent>, mut q_hp: Query<(Entity, &mut Health)>) {
    for ev in evr.read() {
        if let Ok((e, mut hp)) = q_hp.get_mut(ev.target) {
            hp.hp -= ev.amount; if hp.hp <= 0.0 { commands.entity(e).despawn_recursive(); }
        }
    }
}

// === Notes ===
// - Add in Cargo.toml:
//   bevy = { version = "0.16", features = ["dynamic_linking"] }   # optional feature
//   bevy_ecs_tilemap = "0.13"
// - Tile colors are driven by TileColor over a 1x1 white texture. Swap to an atlas when you have art.
// - NavGrid is built from the tilemap (walls = unwalkable). Hook your real level loader here.
// - Smoothing uses greedy LOS; upgrade to funnel algorithm for navmesh later if you move off-grid.
