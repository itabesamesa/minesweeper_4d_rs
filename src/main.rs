use std::cmp::Ordering;
use color_eyre::Result;
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}
};
use ratatui::{
    DefaultTerminal, Frame,
    style::{Stylize, Style, Color},
    text::Line,
    widgets::{Widget},
    layout::{Constraint, Layout, Rect},
    prelude::Buffer,
};
use mersenne_twister::*;
use rand::{Rng, SeedableRng, rngs::ThreadRng};
use std::process;

enum MinesweeperChar {
    Bomb,
    Flag,
}

impl MinesweeperChar {
    fn as_str(&self) -> &'static str {
        match self {
            MinesweeperChar::Bomb => "💣",
            MinesweeperChar::Flag => "🏴"
        }
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct Point {
    x: i16,
    y: i16,
    z: i16,
    w: i16
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z && self.w == other.w
    }
}

impl Eq for Point {}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering { //ordering is very iffy cause obviously... best just
                                              //work around and don't touch!
        if self.eq(&other) {
            Ordering::Equal
        } else if self.x < other.x && self.y < other.y && self.z < other.z && self.w < other.w {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Point {
    fn random_range(&mut self, rng: &mut ThreadRng, max: Point) {
        self.x = rng.random_range(self.x..max.x);
        self.y = rng.random_range(self.y..max.y);
        self.z = rng.random_range(self.z..max.z);
        self.w = rng.random_range(self.w..max.w);
    }

    fn to_1d(self, dim: Point) -> usize {
        (((self.w * dim.z + self.z) * dim.y + self.y) * dim.x + self.x).try_into().unwrap()
    }

    fn calc_area(self) -> usize {
        return (self.x*self.y*self.z*self.w).try_into().unwrap();
    }

    fn offset(self, p: Point) -> Point {
        Point {
            x: self.x+p.x,
            y: self.y+p.y,
            z: self.z+p.z,
            w: self.w+p.w
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct MinesweeperCell {
    coord: Point,
    is_bomb: bool,
    state: u8,
    abs: u8,
    rel: i8,
    is_active: bool,
    in_active_neighbourhood: bool
}

impl Widget for MinesweeperCell {
    fn render(self, area: Rect, buf: &mut Buffer) {
        //let bg = if (self.coord.x + self.coord.y)%2 == 0 {Color::Rgb(0xc6, 0xc6, 0xc6)} else {Color::Rgb(0xb8, 0xb8, 0xb8)}; //Light
        let bg = if self.is_active {
            Color::Rgb(255, 42, 255)
        } else if self.in_active_neighbourhood {
            if (self.coord.x + self.coord.y)%2 == 0 {Color::Rgb(128, 115, 128)} else {Color::Rgb(89, 80, 89)} //Covered
        } else {
            if (self.coord.x + self.coord.y)%2 == 0 {Color::Rgb(0x66, 0x66, 0x66)} else {Color::Rgb(0x3b, 0x3b, 0x3b)} //Covered
        };
        if self.is_bomb {
            buf.set_string(area.left(), area.top(), MinesweeperChar::Bomb.as_str(), Style::default().bg(bg));
        } else if self.abs > 0 {
            buf.set_string(area.left(), area.top(), format!("{:2}", self.abs), Style::default().fg(Color::White).bg(bg));
        } else {
            buf.set_string(area.left(), area.top(), "  ", Style::default().bg(bg));
        }
    }
}

impl MinesweeperCell {
    fn set_bomb(&mut self, t: bool) {
        self.is_bomb = t;
    }

    fn set_coord(&mut self, p: Point) {
        self.coord.x = p.x;
        self.coord.y = p.y;
        self.coord.z = p.z;
        self.coord.w = p.w;
    }

    fn set_active(&mut self, t: bool) {
        self.is_active = t;
    }

    fn set_active_neighbourhood(&mut self, t: bool) {
        self.in_active_neighbourhood = t;
    }

    fn inc_all(&mut self) {
        self.abs += 1;
        self.rel += 1;
    }
}

#[derive(Clone, Debug, Default)]
struct MinesweeperField {
    dim: Point,
    rng: ThreadRng,
    seed: u64,
    mines: u16,
    loc: Point,
    area: usize,
    uncovered_cells: u16,
    is_rel: bool,
    field: Vec<MinesweeperCell>
}

impl Widget for MinesweeperField {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let z_constraints = (0..self.dim.z).map(|_| Constraint::Length((2 * self.dim.x).try_into().unwrap()));
        let w_constraints = (0..self.dim.w).map(|_| Constraint::Length(self.dim.y.try_into().unwrap()));
        let z_horizontal = Layout::horizontal(z_constraints).spacing(2);
        let w_vertical = Layout::vertical(w_constraints).spacing(1);

        let grid_rows = w_vertical.split(area);
        let grids = grid_rows.iter().flat_map(|&grid_row| z_horizontal.split(grid_row).to_vec());

        for (i, grid) in grids.enumerate() {
            let w = (i as i16)/self.dim.w;
            let z = (i as i16)%self.dim.z;
            let x_constraints = (0..self.dim.x).map(|_| Constraint::Length(2));
            let y_constraints = (0..self.dim.y).map(|_| Constraint::Length(1));
            let x_horizontal = Layout::horizontal(x_constraints).spacing(0);
            let y_vertical = Layout::vertical(y_constraints).spacing(0);

            let rows = y_vertical.split(grid);
            let cells = rows.iter().flat_map(|&row| x_horizontal.split(row).to_vec());
            for (j, cell) in cells.enumerate() {
                let y = (j as i16)/self.dim.y;
                let x = (j as i16)%self.dim.x;
                self.field[Point {x: x, y: y, z: z, w: w}.to_1d(self.dim)].render(cell, buf)
            }
        }
    }
}

impl MinesweeperField {
    fn init(&mut self) {
        self.dim = Point {x: 4, y: 4, z: 4, w: 4};
        self.seed = 0;
        self.rng = rand::rng();
        self.mines = 20;
        self.loc = Point {x: 0, y: 0, z: 0, w: 0};
        self.uncovered_cells = 0;
        self.is_rel = true;
        self.area = self.dim.calc_area();
        self.field = vec![
            MinesweeperCell {
                coord: Point {x: 0, y: 0, z: 0, w: 0},
                is_bomb: false,
                state: 0,
                abs: 0,
                rel: 0,
                is_active: false,
                in_active_neighbourhood: false
            }; self.area
        ];
        for w in 0..self.dim.w {
            for z in 0..self.dim.z {
                for y in 0..self.dim.y {
                    for x in 0..self.dim.x {
                        let coord = Point {x: x, y: y, z: z, w: w};
                        let cell: &mut MinesweeperCell = self.cell_at(coord).expect("cell coord is not in dim when generating cell coords");
                        cell.set_coord(coord);
                    }
                }
            }
        }
        self.set_active_cell(true);
    }

    fn set_active_cell(&mut self, t: bool) {
        self.cell_at(self.loc).unwrap().set_active(t);
        self.do_in_neighbourhood(self.loc, |c| c.set_active_neighbourhood(t));
    }

    fn dim_to_str(&self) -> String {
        format!("{} {} {} {}", self.dim.x, self.dim.y, self.dim.z, self.dim.w)
    }

    fn cell_at(&mut self, p: Point) -> Option<&mut MinesweeperCell> {
        if p < self.dim {
            return Some(&mut self.field[p.to_1d(self.dim)]);
        }
        return None;
    }

    fn do_in_neighbourhood(&mut self, p: Point, f: impl Fn(&mut MinesweeperCell)) {
        for w in -1..=1 {
            for z in -1..=1 {
                for y in -1..=1 {
                    for x in -1..=1 {
                        let coord = p.offset(Point {x: x, y: y, z: z, w: w});
                        if (Point {x: -1, y: -1, z: -1, w: -1}) < coord && coord < self.dim {
                            //this comp is on purpose cause it kinda funky...
                            f(self.cell_at(coord).unwrap());
                        }
                    }
                }
            }
        }
    }

    fn place_mines(&mut self) {
        for _ in 0..self.mines {
            loop {
                let mut coord = Point {x: 0, y: 0, z: 0, w: 0};
                coord.random_range(&mut self.rng, self.dim);
                let cell: &mut MinesweeperCell = self.cell_at(coord).expect("cell coord is not in dim");
                if !cell.is_bomb {
                    cell.set_bomb(true);
                    self.do_in_neighbourhood(coord, |c| c.inc_all());
                    break;
                }
            }
        }
    }

    fn move_right_x(&mut self) {
        if self.loc.x+1 < self.dim.x {
            self.set_active_cell(false);
            self.loc.x += 1;
            self.set_active_cell(true);
        }
    }

    fn move_right_z(&mut self) {
        if self.loc.z+1 < self.dim.z {
            self.set_active_cell(false);
            self.loc.z += 1;
            self.set_active_cell(true);
        }
    }

    fn move_left_x(&mut self) {
        if self.loc.x-1 >= 0 {
            self.set_active_cell(false);
            self.loc.x -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_left_z(&mut self) {
        if self.loc.z-1 >= 0 {
            self.set_active_cell(false);
            self.loc.z -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_down_y(&mut self) {
        if self.loc.y+1 < self.dim.y {
            self.set_active_cell(false);
            self.loc.y += 1;
            self.set_active_cell(true);
        }
    }

    fn move_down_w(&mut self) {
        if self.loc.w+1 < self.dim.w {
            self.set_active_cell(false);
            self.loc.w += 1;
            self.set_active_cell(true);
        }
    }

    fn move_up_y(&mut self) {
        if self.loc.y-1 >= 0 {
            self.set_active_cell(false);
            self.loc.y -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_up_w(&mut self) {
        if self.loc.w-1 >= 0 {
            self.set_active_cell(false);
            self.loc.w -= 1;
            self.set_active_cell(true);
        }
    }
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = App::new().run(terminal);
    ratatui::restore();
    result
}

/// The main application which holds the state and logic of the application.
#[derive(Debug, Default)]
pub struct App {
    /// Is the application running?
    running: bool,
    field: MinesweeperField,
}

impl App {
    /// Construct a new instance of [`App`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Run the application's main loop.
    pub fn run(mut self, mut terminal: DefaultTerminal) -> Result<()> {
        self.running = true;
        self.field.init();
        self.field.place_mines();
        while self.running {
            terminal.draw(|frame| self.render(frame))?;
            self.handle_crossterm_events()?;
        }
        Ok(())
    }

    /// Renders the user interface.
    ///
    /// This is where you add new widgets. See the following resources for more information:
    ///
    /// - <https://docs.rs/ratatui/latest/ratatui/widgets/index.html>
    /// - <https://github.com/ratatui/ratatui/tree/main/ratatui-widgets/examples>
    fn render(&mut self, frame: &mut Frame) {
        let title = Line::from("4D Minesweeper")
            .bold()
            .blue()
            .centered();
        let mut area = frame.area();
        area.x += 2;
        area.y += 1;
        area.width -= 4;
        area.height -= 2;
        frame.render_widget(
            title,
            frame.area()
        );
        frame.render_widget(
            self.field.clone(),
            area,
        );
    }

    /// Reads the crossterm events and updates the state of [`App`].
    ///
    /// If your application needs to perform work in between handling events, you can use the
    /// [`event::poll`] function to check if there are any events available with a timeout.
    fn handle_crossterm_events(&mut self) -> Result<()> {
        match event::read()? {
            // it's important to check KeyEventKind::Press to avoid handling key release events
            Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key),
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
            _ => {}
        }
        Ok(())
    }

    /// Handles the key events and updates the state of [`App`].
    fn on_key_event(&mut self, key: KeyEvent) {
        match (key.modifiers, key.code) {
            (_, KeyCode::Esc | KeyCode::Char('q'))
            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
            // Add other key handlers here.
            (_, KeyCode::Char('a')) => self.field.move_left_z(),
            (_, KeyCode::Char('d')) => self.field.move_right_z(),
            (_, KeyCode::Char('w')) => self.field.move_up_w(),
            (_, KeyCode::Char('s')) => self.field.move_down_w(),
            (_, KeyCode::Left) => self.field.move_left_x(),
            (_, KeyCode::Right) => self.field.move_right_x(),
            (_, KeyCode::Up) => self.field.move_up_y(),
            (_, KeyCode::Down) => self.field.move_down_y(),
            _ => {}
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }
}
