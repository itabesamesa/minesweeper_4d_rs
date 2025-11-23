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

#[derive(Copy, Clone, Debug, Default)]
struct Point {
    x: usize,
    y: usize,
    z: usize,
    w: usize
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z && self.w == other.w
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if self.x < other.x && self.y < other.y && self.z < other.z && self.w < other.w {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}

impl Point {
    fn random_range(&mut self, mut rng: ThreadRng, max: Point) {
        self.x = rng.random_range(self.x..max.x);
        self.y = rng.random_range(self.y..max.y);
        self.z = rng.random_range(self.z..max.z);
        self.w = rng.random_range(self.w..max.w);
    }

    fn to_1d(self, dim: Point) -> usize {
        ((self.w * dim.z + self.z) * dim.y + self.y) * dim.x + self.x
    }

    fn calc_area(self) -> usize {
        return self.x*self.y*self.z*self.w;
    }
}

#[derive(Copy, Clone, Debug)]
struct MinesweeperCell {
    is_bomb: bool,
    state: u8,
    abs: u8,
    rel: i8
}

impl Widget for MinesweeperCell {
    fn render(self, area: Rect, buf: &mut Buffer) {
        buf.set_string(area.left(), area.top(), format!("{:2}", self.abs), Style::default().fg(Color::Green));
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
            let w = i/self.dim.w;
            let z = i%self.dim.z;
            let x_constraints = (0..self.dim.x).map(|_| Constraint::Length(2));
            let y_constraints = (0..self.dim.y).map(|_| Constraint::Length(1));
            let x_horizontal = Layout::horizontal(x_constraints).spacing(0);
            let y_vertical = Layout::vertical(y_constraints).spacing(0);

            let rows = y_vertical.split(grid);
            let cells = rows.iter().flat_map(|&row| x_horizontal.split(row).to_vec());
            for (j, cell) in cells.enumerate() {
                let y = j/self.dim.y;
                let x = j%self.dim.x;
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
                is_bomb: false,
                state: 0,
                abs: 0,
                rel: 0
            }; self.area + 1
        ];
    }

    fn dim_to_str(&self) -> String {
        format!("{} {} {} {}", self.dim.x, self.dim.y, self.dim.z, self.dim.w)
    }

    fn cell_at(&self, p: Point) -> MinesweeperCell {
        if p < self.dim {
            self.field[p.to_1d()]
        }
    }

    fn place_mines(&self) {
        for b in 0..self.mines {
            while true {
                coord = Point {x: 0, y: 0, z: 0, w: 0}.random_range(self.rng, self.dim);
            }
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
            _ => {}
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }
}
