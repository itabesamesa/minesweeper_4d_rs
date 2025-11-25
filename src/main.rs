use std::cmp::Ordering;
use std::process;
use std::cell::RefCell;
use color_eyre::Result;
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}
};
use ratatui::{
    DefaultTerminal, Frame,
    style::{Stylize, Style, Color},
    text::{Line, Span},
    widgets::{Widget, Paragraph, Wrap, Block},
    layout::{Constraint, Layout, Rect, Alignment, Flex},
    prelude::Buffer,
};
use mersenne_twister::*;
use rand::{Rng, SeedableRng, rngs::ThreadRng};

#[derive(Clone, Debug, Default)]
enum MinesweeperFieldState {
    #[default] New,
    Running,
    Paused,
    ClickedMine,
    GaveUp,
    RevealField,
    Won
}

impl MinesweeperFieldState {
    fn as_str(&self) -> &'static str {
        match self {
            MinesweeperFieldState::New => "New Game",
            MinesweeperFieldState::Running => "Game Running",
            MinesweeperFieldState::Paused => "Game Paused",
            MinesweeperFieldState::ClickedMine => "Game Over",
            MinesweeperFieldState::GaveUp => "Game Capitulated",
            MinesweeperFieldState::RevealField => "Revealed Game",
            MinesweeperFieldState::Won => "Game Won",
        }
    }
}

#[derive(Clone, Debug, Default)]
enum MinesweeperGameState {
    #[default] Running,
    Controls,
    Settings,
    TooSmall,
}

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
    is_covered: bool,
    is_flagged: bool,
    abs: u8,
    rel: i8,
    is_active: bool,
    in_active_neighbourhood: bool,
    print_zero: bool
}

impl Widget for MinesweeperCell {
    fn render(self, area: Rect, buf: &mut Buffer) {
        buf.set_string(area.left(), area.top(), self.get_string(), Style::default().fg(Color::Black).bg(self.get_color()));
    }
}

impl MinesweeperCell {
    fn set_bomb(&mut self, t: bool) {
        self.is_bomb = t;
    }

    fn set_covered(&mut self, t: bool) {
        self.is_covered = t;
    }

    fn toggle_flagged(&mut self) {
        self.is_flagged = !self.is_flagged;
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

    fn set_print_zero(&mut self, t: bool) {
        self.print_zero = t;
    }

    fn inc_all(&mut self) {
        self.abs += 1;
        self.rel += 1;
    }

    fn get_light_dark_color(&self, light: Color, dark: Color) -> Color {
        if (self.coord.x + self.coord.y)%2 == 0 {
            light
        } else {
            dark
        }
    }

    fn get_color(&self) -> Color {
        if self.is_active {
            Color::Rgb(255, 42, 255)
        } else {
            if self.is_covered {
                if self.in_active_neighbourhood {
                    self.get_light_dark_color(Color::Rgb(128, 115, 128), Color::Rgb(89, 80, 89))
                } else {
                    self.get_light_dark_color(Color::Rgb(0x66, 0x66, 0x66), Color::Rgb(0x3b, 0x3b, 0x3b))
                }
            } else {
                if self.in_active_neighbourhood {
                    self.get_light_dark_color(Color::Rgb(179, 161, 179), Color::Rgb(166, 149, 166))
                } else {
                    self.get_light_dark_color(Color::Rgb(0xc6, 0xc6, 0xc6), Color::Rgb(0xb8, 0xb8, 0xb8))
                }
            }
        }
    }

    fn get_string(&self) -> String {
        if self.is_flagged {
            String::from(MinesweeperChar::Flag.as_str())
        } else if self.is_covered {
            "  ".to_string()
        } else if self.is_bomb {
            String::from(MinesweeperChar::Bomb.as_str())
        } else {
            if self.abs < 10 {
                char::from_u32(0xff10+(self.abs as u32)).unwrap().to_string()
            } else {
                format!("{:2}", self.abs)
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
struct MinesweeperField {
    state: MinesweeperFieldState,
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
        self.state = MinesweeperFieldState::New;
        self.dim = Point {x: 4, y: 4, z: 4, w: 4};
        self.seed = 0;
        self.rng = rand::rng();
        self.mines = 10;
        self.loc = Point {x: 0, y: 0, z: 0, w: 0};
        self.uncovered_cells = 0;
        self.is_rel = true;
        self.area = self.dim.calc_area();
        self.field = vec![
            MinesweeperCell {
                coord: Point {x: 0, y: 0, z: 0, w: 0},
                is_bomb: false,
                is_covered: true,
                is_flagged: false,
                abs: 0,
                rel: 0,
                is_active: false,
                in_active_neighbourhood: false,
                print_zero: false,
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
        self.do_in_neighbourhood(self.loc, |s, p| s.cell_at(p).unwrap().set_active_neighbourhood(t));
    }

    fn dim_to_str(self) -> String {
        format!("{} {} {} {}", self.dim.x, self.dim.y, self.dim.z, self.dim.w)
    }

    fn loc_to_str(self) -> String {
        format!("{} {} {} {}", self.loc.x, self.loc.y, self.loc.z, self.loc.w)
    }

    fn cell_at(&mut self, p: Point) -> Option<&mut MinesweeperCell> {
        if p < self.dim {
            return Some(&mut self.field[p.to_1d(self.dim)]);
        }
        return None;
    }

    fn do_in_neighbourhood(&mut self, p: Point, f: impl Fn(&mut MinesweeperField, Point)) {
        for w in -1..=1 {
            for z in -1..=1 {
                for y in -1..=1 {
                    for x in -1..=1 {
                        let coord = p.offset(Point {x: x, y: y, z: z, w: w});
                        if (Point {x: -1, y: -1, z: -1, w: -1}) < coord && coord < self.dim {
                            //this comp is on purpose cause it kinda funky...
                            f(self, coord);
                        }
                    }
                }
            }
        }
    }

    fn check_in_neighbourhood(&mut self, p: Point, f: impl Fn(&mut MinesweeperField, Point) -> bool) -> bool {
        let mut t: bool = true;
        for w in -1..=1 {
            for z in -1..=1 {
                for y in -1..=1 {
                    for x in -1..=1 {
                        let coord = p.offset(Point {x: x, y: y, z: z, w: w});
                        if (Point {x: -1, y: -1, z: -1, w: -1}) < coord && coord < self.dim {
                            //this comp is on purpose cause it kinda funky...
                            t &= f(self, coord);
                        }
                    }
                }
            }
        }
        t
    }

    fn place_mines(&mut self) {
        for _ in 0..self.mines {
            loop {
                let mut coord = Point {x: 0, y: 0, z: 0, w: 0};
                coord.random_range(&mut self.rng, self.dim);
                let cell: &mut MinesweeperCell = self.cell_at(coord).expect("cell coord is not in dim");
                if !cell.is_bomb {
                    cell.set_bomb(true);
                    self.do_in_neighbourhood(coord, |s, p| s.cell_at(p).unwrap().inc_all());
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

    fn uncover_cell(&mut self, p: Point) {
        let cell: &mut MinesweeperCell = self.cell_at(p).unwrap();
        if cell.is_bomb && cell.is_covered {
            cell.set_covered(false);
            self.state = MinesweeperFieldState::ClickedMine;
        } else if !cell.is_flagged {
            cell.set_covered(false);
            if cell.abs == 0 {
                self.do_in_neighbourhood(p, |s, p| if s.cell_at(p).unwrap().is_covered {s.uncover_cell(p);})
            }
            /*if cell.rel == 0 {
                cell.set_print_zero(self.check_in_neighbourhood(p, |s, p| s.cell_at(p).unwrap().is_covered))
            }*/
        }
    }
}

#[derive(Clone, Debug, Default)]
struct MinesweeperGame {
    field: MinesweeperField,
    state: MinesweeperGameState,
    show_info: bool,
    delta_mode: bool,
    obfuscate_on_pause: bool,
    disable_action_on_reveal: bool,
    disable_movement_on_reveal: bool,
}

fn centered_rect(area: Rect, width: u16, height: u16) -> Rect {
    let horizontal = Layout::horizontal([width]).flex(Flex::Center);
    let vertical = Layout::vertical([height]).flex(Flex::Center);
    let [area] = vertical.areas(area);
    let [area] = horizontal.areas(area);
    area
}

fn center_horizontal(area: Rect, width: u16) -> Rect {
    let [area] = Layout::horizontal([Constraint::Length(width)])
        .flex(Flex::Center)
        .areas(area);
    area
}

fn center_vertical(area: Rect, height: u16) -> Rect {
    let [area] = Layout::vertical([Constraint::Length(height)])
        .flex(Flex::Center)
        .areas(area);
    area
}

impl Widget for MinesweeperGame {
    fn render(self, area: Rect, buf: &mut Buffer) {
        match self.state {
            MinesweeperGameState::Running => {
                let fieldWidth = (self.field.dim.y+1)*2*self.field.dim.w-1;
                let fieldHeight = (self.field.dim.y+1)*self.field.dim.w-1;
                let layout = Layout::horizontal([
                    Constraint::Length(fieldWidth.try_into().unwrap()),
                    Constraint::Max(if self.show_info {50} else {0})
                ].into_iter()).flex(Flex::Center).split(area);
                //let vertical = min(((self.field.dim.y+1)*2*self.field.dim.w-1).try_into().unwrap(), 5);
                /*let [fieldArea] = Layout::vertical([
                        ((self.field.dim.y+1)*2*self.field.dim.w-1).try_into().unwrap()
                    ]).flex(Flex::Center).areas(layout[0]);*/
                /*let fieldVertical = Layout::vertical([
                    Constraint::Length(((self.field.dim.y+1)*2*self.field.dim.w-1).try_into().unwrap())
                ]).flex(Flex::Center).areas(layout[0]);
                eprintln!("{:?}", fieldVertical);*/
                let fieldArea = center_vertical(layout[0], fieldHeight.try_into().unwrap());
                eprintln!("{:?}", layout);

                self.field.render(
                    fieldArea,
                    buf
                );

                if self.show_info {
                    let text = vec![
                        format!("Seed:").into(),
                        //format!("Fields uncovered: {}/{}", self.field.uncovered_cells, (self.field.area as u16)-self.field.mines).into(),
                        format!("Mines Flagged:").into(),
                        //format!("Dimensions:       {}", self.field.dim_to_str()).into(),
                        //format!("Location:         {}", self.field.loc_to_str()).into(),
                        format!("Started at:").into(),
                        //self.field.state.as_str().into(),
                    ];
                    //let [textArea] = Layout::vertical([5]).flex(Flex::Center).areas(layout[1]);
                    Paragraph::new(text)
                        .block(Block::bordered().title("Game Info"))
                        .style(Style::new().white())
                        .alignment(Alignment::Left)
                        .wrap(Wrap { trim: true })
                        .render(
                            center_vertical(layout[1], 5),
                            buf
                        );
                }
            },
            MinesweeperGameState::Controls => {
                let text = vec![
                    "Quit:                          ^C/q/ESC".into(),
                    "Controls:                      c".into(),
                    "Settings:                      o".into(),
                    "Move left in x:                Leftarrow".into(),
                    "Move right in x:               Rightarrow".into(),
                    "Move up in y:                  Uparrow".into(),
                    "Move down in y:                Downarrow".into(),
                    "Move left in z:                a".into(),
                    "Move right in z:               d".into(),
                    "Move up in w:                  w".into(),
                    "Move down in w:                s".into(),
                    "New game:                      n".into(),
                    "Find free cell:                f".into(),
                    "Uncover cell:                  SPACE".into(),
                    "Flag cell:                     m/e".into(),
                    "Pause game:                    p".into(),
                    "Toggle info:                   i".into(),
                ];
                Paragraph::new(text)
                    .block(Block::bordered().title("Game Controls"))
                    .style(Style::new().white())
                    .alignment(Alignment::Left)
                    .wrap(Wrap { trim: true })
                    .render(
                        area,
                        buf
                    );
            },
            MinesweeperGameState::Settings => {
                let text = vec![
                    "Size".into(),
                    "   x:".into(),
                    "   y:".into(),
                    "   z:".into(),
                    "   w:".into(),
                    "Use random seed:".into(),
                    "   Seed:".into(),
                    "Mines:".into(),
                    "Show info:".into(),
                    "Delta mode:".into(),
                ];
                Paragraph::new(text)
                    .block(Block::bordered().title("Game Settings"))
                    .style(Style::new().white())
                    .alignment(Alignment::Left)
                    .wrap(Wrap { trim: true })
                    .render(
                        area,
                        buf
                    );
            },
            MinesweeperGameState::TooSmall => {
                let fieldWidth = (self.field.dim.y+1)*self.field.dim.w+1;
                let fieldHeight = (self.field.dim.y+1)*self.field.dim.w+1;
                let text = vec![
                    "Your terminal is too small".into(),
                    "Recomened minimum size:".into(),
                    format!("{}x{}", fieldWidth, fieldHeight).into(),
                    "With info panel:".into(),
                    format!("{}x{}", fieldWidth, fieldHeight+50).into(),
                ];
                Paragraph::new(text)
                    .block(Block::bordered().title("Game Settings"))
                    .style(Style::new().white())
                    .alignment(Alignment::Left)
                    .wrap(Wrap { trim: true })
                    .render(
                        area,
                        buf
                    );
            }
        }
    }
}

impl MinesweeperGame {
    fn init(&mut self) {
        self.field.init();
        self.field.place_mines();
        self.state = MinesweeperGameState::Running;
        self.show_info = true;
        self.delta_mode = false;
        self.obfuscate_on_pause = false;
        self.disable_action_on_reveal = false;
        self.disable_movement_on_reveal = false;
    }

    fn toggle_show_info(&mut self) {
        self.show_info = !self.show_info;
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
    game: MinesweeperGame,
}

impl App {
    /// Construct a new instance of [`App`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Run the application's main loop.
    pub fn run(mut self, mut terminal: DefaultTerminal) -> Result<()> {
        self.running = true;
        self.game.init();
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
            self.game.clone(),
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
        match self.game.state {
            MinesweeperGameState::Running => {
                match self.game.field.state {
                    MinesweeperFieldState::New => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                            (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                            (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                            (_, KeyCode::Char('i')) => self.game.toggle_show_info(),
                            (_, KeyCode::Char('a')) => self.game.field.move_left_z(),
                            (_, KeyCode::Char('d')) => self.game.field.move_right_z(),
                            (_, KeyCode::Char('w')) => self.game.field.move_up_w(),
                            (_, KeyCode::Char('s')) => self.game.field.move_down_w(),
                            (_, KeyCode::Left) => self.game.field.move_left_x(),
                            (_, KeyCode::Right) => self.game.field.move_right_x(),
                            (_, KeyCode::Up) => self.game.field.move_up_y(),
                            (_, KeyCode::Down) => self.game.field.move_down_y(),
                            (_, KeyCode::Char('f')) => todo!(),
                            (_, KeyCode::Char(' ')) => {
                                self.game.field.uncover_cell(self.game.field.loc);
                                self.game.field.state = MinesweeperFieldState::Running;
                            },
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::Running | MinesweeperFieldState::RevealField => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                            (_, KeyCode::Char('c')) => {
                                self.game.state = MinesweeperGameState::Controls;
                                self.game.field.state = MinesweeperFieldState::Paused;
                            },
                            (_, KeyCode::Char('o')) => {
                                self.game.state = MinesweeperGameState::Settings;
                                self.game.field.state = MinesweeperFieldState::Paused;
                            },
                            (_, KeyCode::Char('i')) => self.game.toggle_show_info(),
                            (_, KeyCode::Char('a')) => self.game.field.move_left_z(),
                            (_, KeyCode::Char('d')) => self.game.field.move_right_z(),
                            (_, KeyCode::Char('w')) => self.game.field.move_up_w(),
                            (_, KeyCode::Char('s')) => self.game.field.move_down_w(),
                            (_, KeyCode::Left) => self.game.field.move_left_x(),
                            (_, KeyCode::Right) => self.game.field.move_right_x(),
                            (_, KeyCode::Up) => self.game.field.move_up_y(),
                            (_, KeyCode::Down) => self.game.field.move_down_y(),
                            (_, KeyCode::Char('p')) => self.game.field.state = MinesweeperFieldState::Paused,
                            (_, KeyCode::Char(' ')) => self.game.field.uncover_cell(self.game.field.loc),
                            (_, KeyCode::Char('m') | KeyCode::Char('e')) => self.game.field.cell_at(self.game.field.loc).unwrap().toggle_flagged(),
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::ClickedMine | MinesweeperFieldState::GaveUp => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.field.state = MinesweeperFieldState::RevealField,
                            (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                            (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                            (_, KeyCode::Char('i')) => self.game.toggle_show_info(),
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::Paused => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q')) | (_, KeyCode::Esc | KeyCode::Char('p'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.field.state = MinesweeperFieldState::Running,
                            (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                            (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                            (_, KeyCode::Char('i')) => self.game.toggle_show_info(),
                            _ => {}
                        }
                    },
                    _ => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                            (_, KeyCode::Char('i')) => self.game.toggle_show_info(),
                            _ => {}
                        }
                    }
                }
            },
            MinesweeperGameState::Settings => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q') | KeyCode::Char('o'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.state = MinesweeperGameState::Running,
                    (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                    _ => {}
                }
            },
            MinesweeperGameState::Controls => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q') | KeyCode::Char('c'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.state = MinesweeperGameState::Running,
                    (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                    _ => {}
                }
            },
            MinesweeperGameState::TooSmall => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                    _ => {}
                }
            }
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }
}
