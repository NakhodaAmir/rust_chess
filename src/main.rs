use core::error;
use std::collections::{HashMap, HashSet};
use std::io::Cursor;
use std::ops::Deref;

use map_macro::hash_set;

use image::imageops::FilterType;
use image::{GenericImageView, ImageBuffer, Pixel, Rgba};
use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, Size};
use winit::error::EventLoopError;
use winit::event::{ElementState, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::window::{CursorIcon, Window, WindowAttributes, WindowButtons, WindowId};

use pixels::{self, Pixels, SurfaceTexture};

const TITLE: &str = "Rust Chess";
const WINDOW_WIDTH: u32 = 1000;
const WINDOW_HEIGHT: u32 = 1000;

const BOARD_WIDTH: u32 = 1000;
const SQUARE_SIZE: u32 = BOARD_WIDTH / 8;
const SCALE_FACTOR: u32 = 4;

const VALID_MOVE_COLOR: [u8; 4] = [173, 216, 230, 255];
const VALID_TAKE_COLOR: [u8; 4] = [0, 0, 139, 255];
const CHECK_COLOR: [u8; 4] = [220, 20, 60, 255];
const VALID_CIRCLE_RADIUS: u32 = 20;

enum Pieces<'a> {
    None,
    Pawn(&'a Color),
    Rook(&'a Color),
    Knight(&'a Color),
    Bishop(&'a Color),
    Queen(&'a Color),
    King(&'a Color),
}

enum DrawImageType {
    None,
    RowCol((u32, u32)),
    Pos((f64, f64)),
}

#[derive(PartialEq, Debug)]
enum Color {
    White,
    Black,
}

enum DrawValidShape {
    Square,
    Circle,
}

enum CastlingRights {
    Some {
        kingside_white: bool,
        queenside_white: bool,
        kingside_black: bool,
        queenside_black: bool,
    },
    None,
}

struct FEN {
    board: Vec<Vec<char>>,
    active_color: Color,
    castling: CastlingRights,
    en_passant: Option<String>, // None if `-`, Some(square) if valid
    halfmove_clock: u32,
    fullmove_number: u32,
}

struct Prefab<'a> {
    image_data: Vec<u8>,
    image_width: u32,
    image_height: u32,
    piece_type: Pieces<'a>,
}

struct App<'a> {
    window: Option<Window>,
    pixels: Option<Pixels>,
    white_tile: Prefab<'a>,
    black_tile: Prefab<'a>,
    pieces: Vec<Vec<Option<Prefab<'a>>>>,
    mouse_position: (f64, f64),
    dragging_piece_pos: Option<(u8, u8)>,
    valid_moves_pos: Option<(HashSet<(u8, u8)>, HashSet<(u8, u8)>)>,
    move_color: Color,
    white_king: ((u8, u8), bool),
    black_king: ((u8, u8), bool),
    en_passant: Option<(u8, u8)>,
    white_long_castle: bool,
    white_short_castle: bool,
    black_long_castle: bool,
    black_short_castle: bool,
}

impl<'a> ApplicationHandler for App<'a> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        self.window = {
            let window_attributes = Window::default_attributes()
                .with_title(TITLE)
                .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT))
                .with_enabled_buttons(WindowButtons::all() - WindowButtons::MAXIMIZE)
                .with_resizable(false);
            Some(event_loop.create_window(window_attributes).unwrap())
        };

        self.pixels = {
            let surface_texture =
                SurfaceTexture::new(WINDOW_WIDTH, WINDOW_HEIGHT, self.window.as_ref().unwrap());
            Some(Pixels::new(WINDOW_WIDTH, WINDOW_HEIGHT, surface_texture).unwrap())
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                let frame = self.pixels.as_mut().unwrap().frame_mut();
                frame.fill(0);

                //Draw Board
                for row in 0..8 {
                    for col in 0..8 {
                        if (col + row) % 2 == 0 {
                            self.black_tile
                                .draw_image(frame, DrawImageType::RowCol((row + 1, col + 1)));
                        } else {
                            self.white_tile
                                .draw_image(frame, DrawImageType::RowCol((row + 1, col + 1)));
                        }
                    }
                }

                //Draw Valid Moves + Takes
                if let Some((valid_paths, valid_takes)) = &self.valid_moves_pos {
                    for path in valid_paths {
                        draw_valid_moves(
                            frame,
                            path.0 + 1,
                            path.1 + 1,
                            VALID_MOVE_COLOR,
                            DrawValidShape::Circle,
                        );
                    }
                    for takes in valid_takes {
                        draw_valid_moves(
                            frame,
                            takes.0 + 1,
                            takes.1 + 1,
                            VALID_TAKE_COLOR,
                            DrawValidShape::Square,
                        );
                    }
                }

                if self.black_king.1 {
                    draw_valid_moves(
                        frame,
                        self.black_king.0 .0 + 1,
                        self.black_king.0 .1 + 1,
                        CHECK_COLOR,
                        DrawValidShape::Square,
                    );
                }
                if self.white_king.1 {
                    draw_valid_moves(
                        frame,
                        self.white_king.0 .0 + 1,
                        self.white_king.0 .1 + 1,
                        CHECK_COLOR,
                        DrawValidShape::Square,
                    );
                }

                //Draw Pieces
                for row in 0..8 {
                    for col in 0..8 {
                        if let Some(piece) = &self.pieces[row as usize][col as usize] {
                            if self.dragging_piece_pos == Some((row, col)) {
                                continue;
                            } else {
                                piece.draw_image(
                                    frame,
                                    DrawImageType::RowCol((row as u32 + 1, col as u32 + 1)),
                                );
                            }
                        }
                    }
                }

                //Draw Dragged Peice
                if let Some(row_col) = &self.dragging_piece_pos {
                    if let Some(piece) = &self.pieces[row_col.0 as usize][row_col.1 as usize] {
                        piece.draw_image(frame, DrawImageType::Pos(self.mouse_position));
                    }
                }

                if self.dragging_piece_pos.is_some() {
                    self.window.as_ref().unwrap().request_redraw();
                }

                self.pixels
                    .as_ref()
                    .unwrap()
                    .render()
                    .expect("wgpu::Surface::get_current_texture failed");
            }
            WindowEvent::CursorMoved { position, .. } => {
                self.mouse_position = (position.x, position.y);
                if self.dragging_piece_pos.is_none() {
                    if let Some((row, col)) = screen_to_board_pos(self.mouse_position) {
                        match &self.pieces[row as usize][col as usize] {
                            Some(piece) => {
                                if piece.piece_type.color() == &self.move_color {
                                    self.window
                                        .as_ref()
                                        .unwrap()
                                        .set_cursor(CursorIcon::Pointer);
                                }
                            }
                            None => self
                                .window
                                .as_ref()
                                .unwrap()
                                .set_cursor(CursorIcon::Default),
                        }
                    }
                }
            }
            WindowEvent::MouseInput { state, button, .. } => match (state, button) {
                (ElementState::Pressed, MouseButton::Left) => {
                    if let Some((row, col)) = screen_to_board_pos(self.mouse_position) {
                        if let Some(piece) = &self.pieces[row as usize][col as usize] {
                            if piece.piece_type.color() == &self.move_color {
                                self.dragging_piece_pos = Some((row, col));
                                self.window
                                    .as_ref()
                                    .unwrap()
                                    .set_cursor(CursorIcon::Pointer);
                                self.valid_moves_pos = Some(piece.piece_type.valid_paths(
                                    &self.pieces,
                                    (row, col),
                                    &self.en_passant,
                                ));
                                self.window.as_ref().unwrap().request_redraw();
                            }
                        }
                    }
                }
                (ElementState::Released, MouseButton::Left) => {
                    if let Some(((row, col), (drag_piece_row, drag_piece_col))) =
                        screen_to_board_pos(self.mouse_position).zip(self.dragging_piece_pos)
                    {
                        if let Some(piece) =
                            self.pieces[drag_piece_row as usize][drag_piece_col as usize].take()
                        {
                            let king_pos = match self.move_color {
                                Color::White => self.white_king.0,
                                Color::Black => self.black_king.0,
                            };
                            if piece.is_valid_move(
                                &self.pieces,
                                (drag_piece_row, drag_piece_col),
                                (row, col),
                                &self.en_passant,
                            ) {
                                if let Some(en_passant) = &self.en_passant {
                                    match piece.piece_type {
                                        Pieces::Pawn(color) => match color {
                                            Color::White => {
                                                if row == en_passant.0 + 1 && col == en_passant.1 {
                                                    self.pieces[en_passant.0 as usize]
                                                        [en_passant.1 as usize] = None;
                                                }
                                            }
                                            Color::Black => {
                                                if row == en_passant.0 - 1 && col == en_passant.1 {
                                                    self.pieces[en_passant.0 as usize]
                                                        [en_passant.1 as usize] = None;
                                                }
                                            }
                                        },
                                        _ => (),
                                    }
                                }
                                let taken_piece = self.pieces[row as usize][col as usize].take();
                                self.pieces[row as usize][col as usize] = Some(piece);
                                let king_pos = {
                                    if king_pos == (drag_piece_row, drag_piece_col) {
                                        (row, col)
                                    } else {
                                        king_pos
                                    }
                                };
                                if !is_check(&self.pieces, king_pos, &self.move_color) {
                                    match self.move_color {
                                        Color::White => {
                                            self.white_king.0 = king_pos;
                                            self.white_king.1 = false;
                                            self.black_king.1 = is_check(
                                                &self.pieces,
                                                self.black_king.0,
                                                &Color::Black,
                                            );
                                        }
                                        Color::Black => {
                                            self.black_king.0 = king_pos;
                                            self.black_king.1 = false;
                                            self.white_king.1 = is_check(
                                                &self.pieces,
                                                self.white_king.0,
                                                &Color::White,
                                            );
                                        }
                                    }
                                    let piece_type = &self.pieces[row as usize][col as usize]
                                        .as_ref()
                                        .unwrap()
                                        .piece_type;

                                    match piece_type {
                                        Pieces::Pawn(_) => {
                                            if row.abs_diff(drag_piece_row) == 2 {
                                                self.en_passant = Some((row, col))
                                            } else {
                                                self.en_passant = None
                                            }
                                        }
                                        _ => self.en_passant = None,
                                    }
                                    self.move_color.inverse();
                                } else {
                                    self.pieces[drag_piece_row as usize][drag_piece_col as usize] =
                                        self.pieces[row as usize][col as usize].take();
                                    self.pieces[row as usize][col as usize] = taken_piece;
                                }
                            } else {
                                self.pieces[drag_piece_row as usize][drag_piece_col as usize] =
                                    Some(piece);
                            }
                        }
                    }
                    self.valid_moves_pos = None;
                    self.dragging_piece_pos = None;
                    self.window
                        .as_ref()
                        .unwrap()
                        .set_cursor(CursorIcon::Default);
                    self.window.as_ref().unwrap().request_redraw();
                }
                _ => (),
            },
            _ => (),
        }
    }
}

impl<'a> Default for App<'a> {
    fn default() -> Self {
        Self {
            window: Default::default(),
            pixels: Default::default(),
            black_tile: Prefab {
                image_data: image::open("images/black_tile.png")
                    .unwrap()
                    .resize(SQUARE_SIZE, SQUARE_SIZE, FilterType::Lanczos3)
                    .to_rgba8()
                    .into_raw(),
                image_width: SQUARE_SIZE,
                image_height: SQUARE_SIZE,
                piece_type: Pieces::None,
            },
            white_tile: Prefab {
                image_data: image::open("images/white_tile.png")
                    .unwrap()
                    .resize(SQUARE_SIZE, SQUARE_SIZE, FilterType::Lanczos3)
                    .to_rgba8()
                    .into_raw(),
                image_width: SQUARE_SIZE,
                image_height: SQUARE_SIZE,
                piece_type: Pieces::None,
            },
            pieces: vec![
                vec![
                    Some(Prefab::new(Pieces::Rook(&Color::White))),
                    Some(Prefab::new(Pieces::Knight(&Color::White))),
                    Some(Prefab::new(Pieces::Bishop(&Color::White))),
                    Some(Prefab::new(Pieces::Queen(&Color::White))),
                    Some(Prefab::new(Pieces::King(&Color::White))),
                    Some(Prefab::new(Pieces::Bishop(&Color::White))),
                    Some(Prefab::new(Pieces::Knight(&Color::White))),
                    Some(Prefab::new(Pieces::Rook(&Color::White))),
                ],
                vec![
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                    Some(Prefab::new(Pieces::Pawn(&Color::White))),
                ],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                    Some(Prefab::new(Pieces::Pawn(&Color::Black))),
                ],
                vec![
                    Some(Prefab::new(Pieces::Rook(&Color::Black))),
                    Some(Prefab::new(Pieces::Knight(&Color::Black))),
                    Some(Prefab::new(Pieces::Bishop(&Color::Black))),
                    Some(Prefab::new(Pieces::Queen(&Color::Black))),
                    Some(Prefab::new(Pieces::King(&Color::Black))),
                    Some(Prefab::new(Pieces::Bishop(&Color::Black))),
                    Some(Prefab::new(Pieces::Knight(&Color::Black))),
                    Some(Prefab::new(Pieces::Rook(&Color::Black))),
                ],
            ],
            mouse_position: (0.0, 0.0),
            dragging_piece_pos: None,
            valid_moves_pos: None,
            move_color: Color::White,
            white_king: ((0, 4), false),
            black_king: ((7, 4), false),
            en_passant: None,
            white_long_castle: true,
            white_short_castle: true,
            black_long_castle: true,
            black_short_castle: true,
        }
    }
}

impl<'a> Prefab<'a> {
    fn new(piece: Pieces<'a>) -> Self {
        let path = {
            match piece {
                Pieces::Pawn(Color::White) => "images/white_pawn.png",
                Pieces::Pawn(Color::Black) => "images/black_pawn.png",
                Pieces::Rook(Color::White) => "images/white_rook.png",
                Pieces::Rook(Color::Black) => "images/black_rook.png",
                Pieces::Knight(Color::White) => "images/white_knight.png",
                Pieces::Knight(Color::Black) => "images/black_knight.png",
                Pieces::Bishop(Color::White) => "images/white_bishop.png",
                Pieces::Bishop(Color::Black) => "images/black_bishop.png",
                Pieces::Queen(Color::White) => "images/white_queen.png",
                Pieces::Queen(Color::Black) => "images/black_queen.png",
                Pieces::King(Color::White) => "images/white_king.png",
                Pieces::King(Color::Black) => "images/black_king.png",
                _ => panic!("Invalid"),
            }
        };
        let image_data = image::open(path).expect(&path);
        let (width, height): (u32, u32) = (
            image_data.dimensions().0 / SCALE_FACTOR,
            image_data.dimensions().1 / SCALE_FACTOR,
        );
        Self {
            image_data: image_data
                .resize(width, height, FilterType::Lanczos3)
                .to_rgba8()
                .into_raw(),
            image_width: width,
            image_height: height,
            piece_type: piece,
        }
    }

    fn draw_image(&self, frame: &mut [u8], draw_image_type: DrawImageType) {
        let (x_offset, y_offset) = match draw_image_type {
            DrawImageType::None => (0, 0),
            DrawImageType::RowCol((row, col)) => {
                let x_offset = ((col * SQUARE_SIZE * 2) - self.image_width) / 2 - SQUARE_SIZE / 2;
                let y_offset =
                    (((9 - row) * SQUARE_SIZE * 2) - self.image_height) / 2 - SQUARE_SIZE / 2;
                (x_offset as usize, y_offset as usize)
            }
            DrawImageType::Pos((x, y)) => {
                let x_offset = (((x + (SQUARE_SIZE / 2) as f64) * 2.0) - self.image_width as f64)
                    / 2.0
                    - (BOARD_WIDTH / 16) as f64;
                let y_offset = (((y + (SQUARE_SIZE / 2) as f64) * 2.0) - self.image_height as f64)
                    / 2.0
                    - (BOARD_WIDTH / 16) as f64;
                (x_offset as usize, y_offset as usize)
            }
        };

        for y in 0..self.image_height {
            for x in 0..self.image_width {
                let frame_x = x as usize + x_offset;
                let frame_y = y as usize + y_offset;

                if frame_x < WINDOW_WIDTH as usize && frame_y < WINDOW_HEIGHT as usize {
                    let frame_index = (frame_y * WINDOW_WIDTH as usize + frame_x) * 4;
                    let image_index = (y * self.image_width + x) * 4;

                    if frame_index + 4 <= frame.len()
                        && image_index + 4 <= self.image_data.len() as u32
                    {
                        // Extract RGBA values
                        let image_r = self.image_data[image_index as usize] as f32;
                        let image_g = self.image_data[image_index as usize + 1] as f32;
                        let image_b = self.image_data[image_index as usize + 2] as f32;
                        let image_a = self.image_data[image_index as usize + 3] as f32 / 255.0;

                        let frame_r = frame[frame_index] as f32;
                        let frame_g = frame[frame_index + 1] as f32;
                        let frame_b = frame[frame_index + 2] as f32;
                        let frame_a = frame[frame_index + 3] as f32 / 255.0;

                        let out_a = image_a + frame_a * (1.0 - image_a);

                        //Alpha blending formula
                        if out_a > 0.0 {
                            frame[frame_index] = ((image_r * image_a
                                + frame_r * frame_a * (1.0 - image_a))
                                / out_a) as u8;
                            frame[frame_index + 1] = ((image_g * image_a
                                + frame_g * frame_a * (1.0 - image_a))
                                / out_a) as u8;
                            frame[frame_index + 2] = ((image_b * image_a
                                + frame_b * frame_a * (1.0 - image_a))
                                / out_a) as u8;
                            frame[frame_index + 3] = (out_a * 255.0) as u8;
                        }
                    }
                }
            }
        }
    }

    fn is_valid_move(
        &self,
        pieces: &Vec<Vec<Option<Prefab<'a>>>>,
        current_pos: (u8, u8),
        target_pos: (u8, u8),
        en_passant: &Option<(u8, u8)>,
    ) -> bool {
        let valid_move = self
            .piece_type
            .valid_paths(&pieces, current_pos, en_passant)
            .0
            .contains(&target_pos)
            || self
                .piece_type
                .valid_paths(&pieces, current_pos, en_passant)
                .1
                .contains(&target_pos);

        valid_move
    }
}

impl Color {
    fn inverse(&mut self) {
        *self = match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }
}

impl<'a> Pieces<'a> {
    fn color(&self) -> &Color {
        match self {
            Pieces::None => &Color::White,
            Pieces::Pawn(color) => color,
            Pieces::Rook(color) => color,
            Pieces::Knight(color) => color,
            Pieces::Bishop(color) => color,
            Pieces::Queen(color) => color,
            Pieces::King(color) => color,
        }
    }

    fn valid_paths(
        &self,
        pieces: &Vec<Vec<Option<Prefab>>>,
        current_pos: (u8, u8),
        en_passant: &Option<(u8, u8)>,
    ) -> (HashSet<(u8, u8)>, HashSet<(u8, u8)>) {
        let calc_pos = |dir: &[(i8, i8)],
                        max_offset: i8,
                        color: &Color,
                        capacity: usize|
         -> (HashSet<(u8, u8)>, HashSet<(u8, u8)>) {
            let mut valid_paths: HashSet<(u8, u8)> = HashSet::with_capacity(capacity);
            let mut valid_takes: HashSet<(u8, u8)> = HashSet::new();
            for dir in dir {
                for offset in 1..max_offset {
                    let calc_pos: (i8, i8) = (
                        current_pos.0 as i8 + dir.0 * offset,
                        current_pos.1 as i8 + dir.1 * offset,
                    );

                    if calc_pos.0 > 7 || calc_pos.0 < 0 || calc_pos.1 > 7 || calc_pos.1 < 0 {
                        break;
                    }

                    if let Some(piece) = &pieces[calc_pos.0 as usize][calc_pos.1 as usize] {
                        if piece.piece_type.color() != color {
                            valid_takes.insert((calc_pos.0 as u8, calc_pos.1 as u8));
                        }
                        break;
                    } else {
                        valid_paths.insert((calc_pos.0 as u8, calc_pos.1 as u8));
                    }
                }
            }
            (valid_paths, valid_takes)
        };

        match self {
            Pieces::None => (HashSet::new(), HashSet::new()),
            Pieces::Pawn(color) => {
                let mut valid_paths = HashSet::with_capacity(4);
                let mut valid_takes: HashSet<(u8, u8)> = HashSet::new();
                match color {
                    Color::White => {
                        if current_pos.0 == 7 {
                            return (HashSet::new(), HashSet::new());
                        }

                        if pieces[(current_pos.0 + 1).min(7) as usize][current_pos.1 as usize]
                            .is_none()
                        {
                            valid_paths.insert(((current_pos.0 + 1).min(7), current_pos.1));

                            if current_pos.0 == 1
                                && pieces[current_pos.0 as usize + 2][current_pos.1 as usize]
                                    .is_none()
                            {
                                valid_paths.insert((current_pos.0 + 2, current_pos.1));
                            }
                        }

                        for &offset in &[1, -1] {
                            let next_row = (current_pos.0 as usize + 1).min(7);
                            let next_col = (current_pos.1 as isize + offset).clamp(0, 7) as usize;

                            if let Some(piece) = &pieces[next_row][next_col] {
                                if piece.piece_type.color() != *color {
                                    valid_takes.insert((next_row as u8, next_col as u8));
                                }
                            }
                        }

                        if let Some(en_passant) = en_passant {
                            if let Some(piece) =
                                &pieces[en_passant.0 as usize][en_passant.1 as usize]
                            {
                                if (current_pos.1 as i8 - 1 == en_passant.1 as i8
                                    || current_pos.1 + 1 == en_passant.1)
                                    && current_pos.0 == en_passant.0
                                    && piece.piece_type.color() != *color
                                {
                                    valid_paths.insert((en_passant.0 + 1, en_passant.1));
                                }
                            }
                        }

                        (valid_paths, valid_takes)
                    }
                    Color::Black => {
                        if current_pos.0 == 0 {
                            return (HashSet::new(), HashSet::new());
                        }

                        if pieces[(current_pos.0 - 1).max(0) as usize][current_pos.1 as usize]
                            .is_none()
                        {
                            valid_paths.insert(((current_pos.0 - 1).max(0), current_pos.1));

                            if current_pos.0 == 6
                                && pieces[current_pos.0 as usize - 2][current_pos.1 as usize]
                                    .is_none()
                            {
                                valid_paths.insert((current_pos.0 - 2, current_pos.1));
                            }
                        }

                        for &offset in &[1, -1] {
                            let next_row = (current_pos.0 as usize - 1).max(0);
                            let next_col = (current_pos.1 as isize + offset).clamp(0, 7) as usize;

                            if let Some(piece) = &pieces[next_row][next_col] {
                                if piece.piece_type.color() != *color {
                                    valid_takes.insert((next_row as u8, next_col as u8));
                                }
                            }
                        }

                        if let Some(en_passant) = en_passant {
                            if let Some(piece) =
                                &pieces[en_passant.0 as usize][en_passant.1 as usize]
                            {
                                if (current_pos.1 as i8 - 1 == en_passant.1 as i8
                                    || current_pos.1 + 1 == en_passant.1)
                                    && current_pos.0 == en_passant.0
                                    && piece.piece_type.color() != *color
                                {
                                    valid_paths.insert((en_passant.0 - 1, en_passant.1));
                                }
                            }
                        }

                        (valid_paths, valid_takes)
                    }
                    _ => (HashSet::new(), HashSet::new()),
                }
            }
            Pieces::Rook(color) => calc_pos(&[(1, 0), (-1, 0), (0, 1), (0, -1)], 8, color, 14),
            Pieces::Knight(color) => calc_pos(
                &[
                    (2, 1),
                    (2, -1),
                    (-2, 1),
                    (-2, -1),
                    (1, 2),
                    (1, -2),
                    (-1, 2),
                    (-1, -2),
                ],
                2,
                color,
                8,
            ),
            Pieces::Bishop(color) => calc_pos(&[(1, 1), (1, -1), (-1, 1), (-1, -1)], 8, color, 14),
            Pieces::Queen(color) => calc_pos(
                &[
                    (1, 1),
                    (1, -1),
                    (-1, 1),
                    (-1, -1),
                    (1, 0),
                    (-1, 0),
                    (0, 1),
                    (0, -1),
                ],
                8,
                color,
                24,
            ),
            Pieces::King(color) => calc_pos(
                &[
                    (1, 1),
                    (1, -1),
                    (-1, 1),
                    (-1, -1),
                    (1, 0),
                    (-1, 0),
                    (0, 1),
                    (0, -1),
                ],
                2,
                color,
                8,
            ),
        }
    }
}

/* impl FEN {
    fn parse_fen(fen: &str) -> Result<FEN, String>{
        let parts: Vec<&str> = fen.split_whitespace().collect();
        if parts.len() != 6 { return Err("Invalid FEN: Incorrect number of fields".to_string())};

        let board: Vec<String> = parts[0].split('/').map(|s| s.to_string()).collect();
        if board.len() != 8 { return Err("Invalid FEN: board does not have 8 ranks".to_string())};

        let board: Vec<Vec<char>> = board.iter().map(|s| s.chars().collect()).collect();

        let active_color = match parts[1] {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return Err("Invalid FEN: active color must be 'w' or 'b'".to_string())
        };

        let castling = if parts[2] == "-" {
            CastlingRights::None
        } else {
            CastlingRights::Some {
                kingside_white: parts[2].contains('K'),
                queenside_white: parts[2].contains('Q'),
                kingside_black: parts[2].contains('k'),
                queenside_black: parts[2].contains('q'),
            }
        };

        let en_passant = if parts[3] == "-" {
            None
        } else {
            Some(parts[3].to_string())
        };

        let halfmove_clock = parts[4].parse::<u32>().map_err(|_| "Invalid FEN: halfmove clock is not a number")?;

        let fullmove_number = parts[5].parse::<u32>().map_err(|_| "Invalid FEN: fullmove number is not a number")?;

        Ok( FEN {
            board,
            active_color,
            castling,
            en_passant,
            halfmove_clock,
            fullmove_number
        })
    }

    fn draw_board(&self, frame: &mut [u8], black_rook: &Prefab, black_knight: &Prefab, black_bishop: &Prefab, black_queen: &Prefab, black_king: &Prefab, black_pawn: &Prefab, white_rook: &Prefab, white_knight: &Prefab, white_bishop: &Prefab, white_queen: &Prefab, white_king: &Prefab, white_pawn: &Prefab) {
        for row in (1..9).rev() {
            let mut col = 1;
            for elem in &self.board[row - 1] {
                match elem {
                    'r' => black_rook.draw_image(frame, Some((row as u32, col as u32))),
                    'n' => black_knight.draw_image(frame, Some((row as u32, col as u32))),
                    'b' => black_bishop.draw_image(frame, Some((row as u32, col as u32))),
                    'q' => black_queen.draw_image(frame, Some((row as u32, col as u32))),
                    'k' => black_king.draw_image(frame, Some((row as u32, col as u32))),
                    'p' => black_pawn.draw_image(frame, Some((row as u32, col as u32))),
                    'R' => white_rook.draw_image(frame, Some((row as u32, col as u32))),
                    'N' => white_knight.draw_image(frame, Some((row as u32, col as u32))),
                    'B' => white_bishop.draw_image(frame, Some((row as u32, col as u32))),
                    'Q' => white_queen.draw_image(frame, Some((row as u32, col as u32))),
                    'K' => white_king.draw_image(frame, Some((row as u32, col as u32))),
                    'P' => white_pawn.draw_image(frame, Some((row as u32, col as u32))),
                    _ => {
                        col += elem.to_digit(10).unwrap() as usize;
                        continue;
                    }
                }
                col += 1;
            }
        }
    }
} */

fn screen_to_board_pos(mouse_pos: (f64, f64)) -> Option<(u8, u8)> {
    let col = (mouse_pos.0 as u32 / SQUARE_SIZE) as u8;
    let row = ((BOARD_WIDTH - mouse_pos.1.min(BOARD_WIDTH as f64) as u32) / SQUARE_SIZE) as u8;

    //0-8
    if row < 8 && col < 8 {
        Some((row, col))
    } else {
        None
    }
}

fn is_check(pieces: &Vec<Vec<Option<Prefab>>>, king_pos: (u8, u8), color: &Color) -> bool {
    let mut combined_takes: HashSet<(u8, u8)> = HashSet::new();
    combined_takes.extend(&Pieces::Knight(color).valid_paths(pieces, king_pos, &None).1);
    combined_takes.extend(&Pieces::Bishop(color).valid_paths(pieces, king_pos, &None).1);
    combined_takes.extend(&Pieces::Rook(color).valid_paths(pieces, king_pos, &None).1);
    combined_takes.extend(&Pieces::Queen(color).valid_paths(pieces, king_pos, &None).1);
    combined_takes.extend(&Pieces::King(color).valid_paths(pieces, king_pos, &None).1);

    for takes in combined_takes {
        if let Some(piece) = &pieces[takes.0 as usize][takes.1 as usize] {
            let potential_check = piece.piece_type.valid_paths(pieces, takes, &None).1;
            if potential_check.contains(&king_pos) {
                return true;
            }
        }
    }
    false
}

fn draw_valid_moves(frame: &mut [u8], row: u8, col: u8, color: [u8; 4], shape: DrawValidShape) {
    let x_pos = ((col as u32 * SQUARE_SIZE * 2) - SQUARE_SIZE) / 2 - SQUARE_SIZE / 2;
    let y_pos = (((9 - row as u32) * SQUARE_SIZE * 2) - SQUARE_SIZE) / 2 - SQUARE_SIZE / 2;

    let mut set_frame = |y: u32, x: u32| {
        if x < WINDOW_WIDTH && y < WINDOW_HEIGHT {
            let index = ((y * WINDOW_WIDTH + x) * 4) as usize;

            frame[index..index + 4].copy_from_slice(&color);
        }
    };

    match shape {
        DrawValidShape::Square => {
            for y in y_pos..(y_pos + SQUARE_SIZE) {
                for x in x_pos..(x_pos + SQUARE_SIZE) {
                    set_frame(y, x);
                }
            }
        }
        DrawValidShape::Circle => {
            let x_pos = x_pos + SQUARE_SIZE / 2;
            let y_pos = y_pos + SQUARE_SIZE / 2;
            let sqaured_radius = (VALID_CIRCLE_RADIUS * VALID_CIRCLE_RADIUS) as i32;

            for y in (y_pos.saturating_sub(VALID_CIRCLE_RADIUS))..=(y_pos + VALID_CIRCLE_RADIUS) {
                for x in (x_pos.saturating_sub(VALID_CIRCLE_RADIUS))..=(x_pos + VALID_CIRCLE_RADIUS)
                {
                    let dx = x as i32 - x_pos as i32;
                    let dy = y as i32 - y_pos as i32;

                    if dx * dx + dy * dy <= sqaured_radius {
                        set_frame(y, x);
                    }
                }
            }
        }
    }
}

fn main() -> Result<(), EventLoopError> {
    let event_loop = EventLoop::new().unwrap();

    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = App::default();
    event_loop.run_app(&mut app)
}
