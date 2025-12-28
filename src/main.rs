use std::{
    cmp::Ordering,
    fmt,
    env,
    str,
    time::{Duration, Instant},
};
use color_eyre::Result;
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}
};
use ratatui::{
    DefaultTerminal, Frame,
    style::{Stylize, Style, Color, Modifier},
    text::Line,
    widgets::{Widget, Paragraph, Wrap, Block},
    layout::{Constraint, Layout, Rect, Alignment, Flex},
    prelude::Buffer,
};
use rand::{
    Rng, //SeedableRng,
    rngs::ThreadRng
};
use chrono::{DateTime, Local};
use len_trait::Len;
//use rand_chacha::ChaCha20Rng;
//ChaCha20Rng doesn't implement Default, gonna have to find somthing else

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

#[derive(Debug, Default, Clone)]
enum SettingsOptionTypes {
    #[default] None,
    Bool,
    Int,
}

#[derive(Copy, Clone, Debug, Default)]
pub struct Point {
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
    fn new() -> Point {
        Point {x: 0, y: 0, z: 0, w: 0}
    }

    fn random_range(&mut self, rng: &mut ThreadRng, max: Point) {
        self.x = rng.random_range(self.x..max.x);
        self.y = rng.random_range(self.y..max.y);
        self.z = rng.random_range(self.z..max.z);
        self.w = rng.random_range(self.w..max.w);
    }

    fn to_1d(self, dim: Point) -> usize {
        (((self.w * dim.z + self.z) * dim.y + self.y) * dim.x + self.x).try_into().unwrap()
        //(((self.x * dim.y + self.y) * dim.z + self.z) * dim.w + self.w).try_into().unwrap()
    }

    fn calc_area(self) -> usize {
        (self.x*self.y*self.z*self.w).try_into().unwrap()
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

#[derive(Clone, Debug, Default)]
struct KeyValueList<T> {//T must implement to_string() method!
    pos: usize,
    highlight: bool,
    title: String,
    constraint_len: u16,
    array: Vec<(String, T)>
}

impl<T: std::fmt::Display> Widget for KeyValueList<T> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let block = Block::bordered()
            .border_style(Style::default().fg(Color::White))
            .style(Style::default().fg(Color::White))
            .title(self.title);
        let layout = Layout::horizontal([
            Constraint::Length(self.constraint_len),
            Constraint::Fill(1)
        ]).split(block.inner(area));
        block.render(area, buf);
        Paragraph::new(
            self.array.iter().enumerate().map(|x| {// i don't like this, but too much of a rust
                                                   // noob to fix...
                if self.highlight && x.0 == self.pos {
                    Line::raw(x.1.0.clone()).style(Modifier::UNDERLINED)
                } else {
                    Line::raw(x.1.0.clone())
                }
            }).collect::<Vec<ratatui::prelude::Line>>())
            .style(Style::new().white())
            .alignment(Alignment::Left)
            .wrap(Wrap { trim: true })
            .render(
                layout[0],
                buf
            );
        Paragraph::new(
            self.array.iter().enumerate().map(|x| {// i don't like this, but too much of a rust
                                                   // noob to fix...
                if self.highlight && x.0 == self.pos {
                    Line::raw(format!("{}", x.1.1)).style(Modifier::UNDERLINED)
                } else {
                    Line::raw(format!("{}", x.1.1))
                }
            }).collect::<Vec<ratatui::prelude::Line>>())
            .style(Style::new().white())
            .alignment(Alignment::Right)
            .wrap(Wrap { trim: true })
            .render(
                layout[1],
                buf
            );
    }
}

impl<T: Len> KeyValueList<T> {
    fn new(highlight: bool, title: String, array: Vec<(String, T)>) -> KeyValueList<T> {
        let cl = array.iter().reduce(|a, b| if a.0.len() < b.0.len() {b} else {a}).unwrap().0.len() as u16;
        KeyValueList {
            pos: 0,
            highlight: highlight,
            title: title,
            constraint_len: cl,
            array: array
        }
    }

    fn inc_pos(&mut self) {
        self.pos = (self.pos+1)%self.array.len();
    }

    fn dec_pos(&mut self) {
        self.pos = (self.array.len()+self.pos-1)%self.array.len();
    }

    fn get_tuple(&mut self) -> &mut (String, T) {
        &mut self.array[self.pos]
    }
}

#[derive(Clone, Debug, Default)]
struct SettingsOption {
    enabled: bool,
    option_type: SettingsOptionTypes,
    value: u16
}

impl fmt::Display for SettingsOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.enabled {
            match self.option_type {
                SettingsOptionTypes::Bool => write!(f, "{}", if self.value == 0 {"Off"} else {"On"}),
                SettingsOptionTypes::Int => write!(f, "{}", self.value),
                _ => write!(f, ""),
            }
        } else {
            write!(f, "")
        }
    }
}

impl len_trait::Empty for SettingsOption {
    fn is_empty(&self) -> bool {
        if matches!(self.option_type, SettingsOptionTypes::None) {
            true
        } else {
            false
        }
    }
}

impl Len for SettingsOption {
    fn len(&self) -> usize {
        match self.option_type {
            SettingsOptionTypes::Bool => if self.value == 0 {3} else {2},
            SettingsOptionTypes::Int => (self.value.checked_ilog10().unwrap_or(0) + 1).try_into().unwrap(),
            _ => 0,
        }
    }
}

impl SettingsOption {
    fn set_bool(&mut self, v: bool) {
        if matches!(self.option_type, SettingsOptionTypes::Bool) {
            if v {
                self.value = 1;
            } else {
                self.value = 0;
            }
        }
    }

    fn toggle_bool(&mut self) {//no ! cause 000 -> 111
        if self.value == 0 {
            self.value = 1;
        } else {
            self.value = 0;
        }
    }

    fn inc(&mut self) {
        match self.option_type {
            SettingsOptionTypes::Bool => self.toggle_bool(),
            SettingsOptionTypes::Int => self.value += 1,
            _ => {},
        }
    }

    fn dec(&mut self) {
        match self.option_type {
            SettingsOptionTypes::Bool => self.toggle_bool(),
            SettingsOptionTypes::Int => if self.value != 0 {self.value -= 1;},
            _ => {},
        }
    }

    fn append(&mut self, v: u16) {
        match self.option_type {
            SettingsOptionTypes::Bool => if v != 0 {self.value = 1;} else {self.value = 0;},
            SettingsOptionTypes::Int => self.value = self.value.wrapping_mul(10).wrapping_add(v),
            //could do better, but too lazy
            _ => {},
        }
    }
 
    fn del(&mut self) {
        match self.option_type {
            SettingsOptionTypes::Bool => self.toggle_bool(),
            SettingsOptionTypes::Int => self.value = self.value/10,
            _ => {},
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

impl MinesweeperCell {
    fn new() -> MinesweeperCell {
        MinesweeperCell {
            coord: Point::new(),
            is_bomb: false,
            is_covered: true,
            is_flagged: false,
            abs: 0,
            rel: 0,
            is_active: false,
            in_active_neighbourhood: false,
            print_zero: false
        }
    }

    fn render_rel(self, area: Rect, buf: &mut Buffer) {
        buf.set_string(area.left(), area.top(), self.get_rel_string(), Style::default().fg(Color::Black).bg(self.get_color()));
    }

    fn render_abs(self, area: Rect, buf: &mut Buffer) {
        buf.set_string(area.left(), area.top(), self.get_abs_string(), Style::default().fg(Color::Black).bg(self.get_color()));
    }

    fn set_bomb(&mut self, t: bool) {
        self.is_bomb = t;
    }

    fn set_covered(&mut self, t: bool) {
        self.is_covered = t;
    }

    fn set_coord(&mut self, p: Point) {
        self.coord = p;
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

    fn inc_rel(&mut self) {
        self.rel += 1;
    }

    fn dec_all(&mut self) {
        self.abs -= 1;
        self.rel -= 1;
    }

    fn dec_rel(&mut self) {
        self.rel -= 1;
    }

    fn dec_abs(&mut self) {
        self.abs -= 1;
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

    fn get_rel_string(&self) -> String {
        if self.is_flagged {
            String::from(MinesweeperChar::Flag.as_str())
        } else if self.is_covered {
            "  ".to_string()
        } else if self.is_bomb {
            String::from(MinesweeperChar::Bomb.as_str())
        } else if self.print_zero {
            char::from_u32(0xff10).unwrap().to_string()
        } else if self.rel != 0 {
            if 0 < self.rel && self.rel < 10 {
                char::from_u32(0xff10+(self.rel as u32)).unwrap().to_string()
            } else {
                format!("{:2}", self.rel)
            }
        } else {
            "  ".to_string()
        }
    }

    fn get_abs_string(&self) -> String {
        if self.is_flagged {
            String::from(MinesweeperChar::Flag.as_str())
        } else if self.is_covered {
            "  ".to_string()
        } else if self.is_bomb {
            String::from(MinesweeperChar::Bomb.as_str())
        } else if self.abs != 0 {
            if 0 < self.abs && self.abs < 10 {
                char::from_u32(0xff10+(self.abs as u32)).unwrap().to_string()
            } else {
                format!("{:2}", self.abs)
            }
        } else {
            "  ".to_string()
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
    flagged_mines: u16,
    delta_mode: bool,
    sweep_mode: bool,
    started: DateTime<Local>,
    duration: Duration,
    //ended: DateTime<Local>,
    field: Vec<MinesweeperCell>
}

impl Widget for MinesweeperField {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let z_constraints = (0..self.dim.z).map(|_| Constraint::Length((2 * self.dim.x).try_into().unwrap()));
        let w_constraints = (0..self.dim.w).map(|_| Constraint::Length(self.dim.y.try_into().unwrap()));
        let z_horizontal = Layout::horizontal(z_constraints).spacing(2);
        let w_vertical = Layout::vertical(w_constraints).spacing(1);

        let grid_rows = w_vertical.split(area);
        let grids: Vec<Rect> = grid_rows.iter().flat_map(|&grid_row| z_horizontal.split(grid_row).to_vec()).collect();

        if matches!(self.state, MinesweeperFieldState::Paused) {
            Block::new()
                .style(Style::default().bg(Color::Rgb(38, 38, 38)))
                .title_top(Line::from(self.state.as_str()).centered().fg(Color::White))
                .render(area, buf);
        } else {
            for w in 0..self.dim.w {
                for z in 0..self.dim.z {
                    let x_constraints = (0..self.dim.x).map(|_| Constraint::Length(2));
                    let y_constraints = (0..self.dim.y).map(|_| Constraint::Length(1));
                    let x_horizontal = Layout::horizontal(x_constraints).spacing(0);
                    let y_vertical = Layout::vertical(y_constraints).spacing(0);

                    let rows = y_vertical.split(grids[(w*self.dim.z+z) as usize]);
                    let cells: Vec<Rect> = rows.iter().flat_map(|&row| x_horizontal.split(row).to_vec()).collect();
                    for y in 0..self.dim.y {
                        for x in 0..self.dim.x {
                            let cell_area = cells[(y*self.dim.x+x) as usize];
                            if self.delta_mode {
                                self.field[Point {x: x, y: y, z: z, w: w}.to_1d(self.dim)].render_rel(cell_area, buf);
                            } else {
                                self.field[Point {x: x, y: y, z: z, w: w}.to_1d(self.dim)].render_abs(cell_area, buf);
                            }
                        }
                    }
                }
            }
            if matches!(self.state, MinesweeperFieldState::ClickedMine)
                || matches!(self.state, MinesweeperFieldState::GaveUp)
                || matches!(self.state, MinesweeperFieldState::Won) {
                let mut message_box = centered_rect(area, 18, 3);
                message_box.x -= 1;
                let text = vec![
                    self.state.as_str().into()
                ];
                Paragraph::new(text)
                    .block(Block::bordered())
                    .style(Style::new().white().bg(Color::Rgb(38, 38, 38)))
                    .alignment(Alignment::Center)
                    .wrap(Wrap { trim: true })
                    .render(
                        message_box,
                        buf
                    );
            }
        }
    }
}

impl MinesweeperField {
    fn init(&mut self, dim: Point, mines: u16, delta_mode: bool, sweep_mode: bool) {
        self.state = MinesweeperFieldState::New;
        self.dim = dim;
        self.seed = 0;
        self.rng = rand::rng();
        self.mines = mines;
        self.loc = Point::new();
        self.uncovered_cells = 0;
        self.flagged_mines = 0;
        self.delta_mode = delta_mode;
        self.sweep_mode = sweep_mode;
        self.area = self.dim.calc_area();
        self.started = Local::now();
        self.duration = Duration::ZERO;
        //self.ended = self.started;
        self.fill_field();
        self.set_active_cell(true);
        self.place_mines();
    }

    fn fill_field(&mut self) {
        self.field = vec![MinesweeperCell::new(); self.area];
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
    }

    fn regenerate(&mut self) {
        self.field.clear();
        self.fill_field();
        self.set_active_cell(true);
        self.place_mines();
        self.state = MinesweeperFieldState::New;
        self.uncovered_cells = 0;
        self.flagged_mines = 0;
        self.duration = Duration::ZERO;
    }

    fn set_active_cell(&mut self, t: bool) {
        self.cell_at(self.loc).unwrap().set_active(t);
        self.do_in_neighbourhood(self.loc, |s, p| s.cell_at(p).unwrap().set_active_neighbourhood(t));
    }

    fn cell_at(&mut self, p: Point) -> Option<&mut MinesweeperCell> {
        if p < self.dim {
            return Some(&mut self.field[p.to_1d(self.dim)]);
        }
        return None;
    }

    fn do_everywhere(&mut self, f: impl Fn(&mut MinesweeperField, Point)) {
        for i in 0..self.area {
            let p = self.field[i].coord;
            f(self, p);
        }
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
        let mut t: bool = false;
        for w in -1..=1 {
            for z in -1..=1 {
                for y in -1..=1 {
                    for x in -1..=1 {
                        let coord = p.offset(Point {x: x, y: y, z: z, w: w});
                        if (Point {x: -1, y: -1, z: -1, w: -1}) < coord && coord < self.dim {
                            //this comp is on purpose cause it kinda funky...
                            t |= f(self, coord);
                        }
                    }
                }
            }
        }
        t
    }

    fn place_mines(&mut self) {
        if self.mines > (self.area as u16) {
            self.mines = self.area as u16; //should warn and break instead
        }
        if self.mines > (self.area as u16)/2 {
            for i in 0..self.area {
                let cell: &mut MinesweeperCell = &mut self.field[i];
                cell.set_bomb(true);
                let p = cell.coord;
                self.do_in_neighbourhood(p, |s, p| s.cell_at(p).unwrap().inc_all());
            }
            if self.mines != (self.area as u16) {
                for _ in self.mines..(self.area as u16) {
                    loop {
                        let mut coord = Point::new();
                        coord.random_range(&mut self.rng, self.dim);
                        let cell = self.cell_at(coord).unwrap();
                        if cell.is_bomb {
                            cell.set_bomb(false);
                            self.do_in_neighbourhood(coord, |s, p| s.cell_at(p).unwrap().dec_all());
                            break;
                        }
                    }
                }
            }
        } else {
            for _ in 0..self.mines {
                loop {
                    let mut coord = Point::new();
                    coord.random_range(&mut self.rng, self.dim);
                    let cell: &mut MinesweeperCell = self.cell_at(coord).unwrap();
                    if !cell.is_bomb {
                        cell.set_bomb(true);
                        self.do_in_neighbourhood(coord, |s, p| s.cell_at(p).unwrap().inc_all());
                        break;
                    }
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

    fn uncover_rel_cell(&mut self, p: Point) {
        self.do_in_neighbourhood(p, |s, p| {
            let cell = s.cell_at(p).unwrap();
            if cell.is_covered || cell.print_zero {
                cell.set_print_zero(false);
                s.uncover_cell(p);
            }
        });
    }

    fn set_print_zero(&mut self, p: Point) {
        self.do_in_neighbourhood(p, |s, p| {
            if !s.cell_at(p).unwrap().is_covered {
                let t = s.check_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    cell.is_covered && !cell.is_flagged
                });
                let cell = s.cell_at(p).unwrap();
                if cell.rel == 0 {
                    cell.set_print_zero(t);
                } else {
                    cell.set_print_zero(false);
                }
            }
        });
    }

    fn uncover_cell(&mut self, p: Point) {
        let cell: &mut MinesweeperCell = self.cell_at(p).unwrap();
        if !cell.is_flagged {
            if cell.is_covered {
                cell.set_covered(false);
                cell.set_print_zero(false);
                if cell.is_bomb {
                    if !matches!(self.state, MinesweeperFieldState::RevealField) {
                        self.state = MinesweeperFieldState::ClickedMine;
                    }
                } else {
                    if cell.abs == 0 {
                        self.do_in_neighbourhood(p, |s, p| s.uncover_cell(p));
                    } else if cell.rel == 0 {
                        if self.delta_mode {
                            self.uncover_rel_cell(p);
                        } //should still set print_zero when not in delta_mode to avoid graphical bugs when
                        //swapping between modes
                    }
                    self.uncovered_cells += 1;
                    if !self.sweep_mode && usize::from(self.uncovered_cells+self.mines) == self.area && !matches!(self.state, MinesweeperFieldState::RevealField) {
                        self.state = MinesweeperFieldState::Won;
                    }
                }
            } else if cell.rel == 0 {
                if self.delta_mode {
                    self.uncover_rel_cell(p);
                    //should still set print_zero when not in delta_mode to avoid graphical bugs when
                    //swapping between modes
                } else {
                    self.do_in_neighbourhood(p, |s, p| {
                        let cell = s.cell_at(p).unwrap();
                        if !cell.is_flagged {
                            cell.set_covered(false);
                            if cell.is_bomb {
                                if !matches!(s.state, MinesweeperFieldState::RevealField) {
                                    s.state = MinesweeperFieldState::ClickedMine;
                                }
                            }
                        }
                    });
                }
            }
        }
    }

    fn toggle_flagged(&mut self, p: Point) {
        if self.sweep_mode {
            let cell: &mut MinesweeperCell = self.cell_at(p).unwrap();
            if cell.is_covered {
                if cell.is_bomb {
                    cell.set_bomb(false);
                    self.uncover_cell(self.loc);
                    self.do_in_neighbourhood(p, |s, p| {
                        let cell = s.cell_at(p).unwrap();
                        if cell.rel == cell.abs.try_into().unwrap() {
                            cell.dec_all();
                        } else {
                            cell.dec_abs();
                        }
                        if cell.abs == 0 {
                            let loc = cell.coord;
                            s.uncover_cell(loc);
                        }
                    });
                    self.flagged_mines += 1;
                    self.uncovered_cells -= 1;
                    if usize::from(self.uncovered_cells+self.mines) == self.area && !matches!(self.state, MinesweeperFieldState::RevealField) {
                        self.state = MinesweeperFieldState::Won;
                    }
                } else {
                    self.state = MinesweeperFieldState::ClickedMine;
                }
            }
        } else {
            let cell: &mut MinesweeperCell = self.cell_at(p).unwrap();
            cell.is_flagged = !cell.is_flagged;
            if cell.is_flagged {
                self.do_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    cell.dec_rel();
                });
                self.flagged_mines += 1;
            } else {
                self.do_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    cell.inc_rel();
                });
                self.flagged_mines -= 1;
            }
            self.set_print_zero(p);
        }
    }

    fn toggle_flagged_chording(&mut self, p: Point) {
        self.do_in_neighbourhood(p, |s, p| {
            if s.cell_at(p).unwrap().is_covered {
                s.toggle_flagged(p);
            }
        })
    }

    fn find_free_cell(&mut self) {
        let mut p = Point::new();
        let mut found: bool = false;
        for cell in &self.field {
            if cell.abs == 0 {
                found = true;
                p = cell.coord;
                break;
            }
        }
        if found {
            self.set_active_cell(false);
            self.loc = p;
            self.set_active_cell(true);
            self.uncover_cell(p);
        }
    }

    fn get_display_width(&self) -> u16 {
        ((self.dim.x+1)*2*self.dim.z-1).try_into().unwrap()
    }

    fn get_display_height(&self) -> u16 {
        ((self.dim.y+1)*self.dim.w-1).try_into().unwrap()
    }

    fn delta_mode_str(&self) -> String {
        (if self.delta_mode {"On"} else {"Off"}).to_string()
    }

    fn sweep_mode_str(&self) -> String {
        (if self.sweep_mode {"On"} else {"Off"}).to_string()
    }

    /*fn seed_str(&self) -> String {
        format!("{}", self.seed)
    }*/

    fn cells_uncovered_str(&self) -> String {
        format!("{}/{}", self.uncovered_cells, (self.area as u16)-self.mines)
    }

    fn mines_flagged_str(&self) -> String {
        format!("{}/{}", self.flagged_mines, self.mines)
    }

    fn dim_str(&self) -> String {
        format!("{} {} {} {}", self.dim.x, self.dim.y, self.dim.z, self.dim.w)
    }

    fn loc_str(&self) -> String {
        format!("{} {} {} {}", self.loc.x, self.loc.y, self.loc.z, self.loc.w)
    }

    fn started_str(&self) -> String {
        self.started.format("%Y-%m-%d %H:%M:%S").to_string()
    }

    fn time_elapsed_str(&self) -> String {
        format!("{:.3}", self.duration.as_secs_f32())
    }

    fn state_str(&self) -> String {
        self.state.as_str().to_string()
    }
}

#[derive(Clone, Debug, Default)]
struct MinesweeperGame {
    field: MinesweeperField,
    state: MinesweeperGameState,
    show_info: bool,
    info: KeyValueList<String>,
    controls: KeyValueList<String>,
    settings: (KeyValueList<SettingsOption>, KeyValueList<String>),
    info_panel_min_width: u16,
    info_panel_max_width: u16,
    /*obfuscate_on_pause: bool,
    disable_action_on_reveal: bool,
    disable_movement_on_reveal: bool,*/
}

fn centered_rect(area: Rect, width: u16, height: u16) -> Rect {
    let horizontal = Layout::horizontal([width]).flex(Flex::Center);
    let vertical = Layout::vertical([height]).flex(Flex::Center);
    let [area] = vertical.areas(area);
    let [area] = horizontal.areas(area);
    area
}

/*fn center_horizontal(area: Rect, width: u16) -> Rect {
    let [area] = Layout::horizontal([Constraint::Length(width)])
        .flex(Flex::Center)
        .areas(area);
    area
}*/

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
                let layout = Layout::horizontal([
                    Constraint::Length(self.field.get_display_width().try_into().unwrap()),
                    Constraint::Max(if self.show_info {self.info_panel_max_width} else {0})
                ].into_iter()).flex(Flex::Center).split(area);
                let field_area = center_vertical(
                    layout[0],
                    self.field.get_display_height().try_into().unwrap()
                );

                self.field.render(
                    field_area,
                    buf
                );

                if self.show_info {
                    let height = self.info.array.len()+2;
                    self.info.render(center_vertical(layout[1], height.try_into().unwrap()), buf);
                }
            },
            MinesweeperGameState::Controls => {
                let height = self.controls.array.len()+2;
                self.controls.render(centered_rect(area, 50, height.try_into().unwrap()), buf);
            },
            MinesweeperGameState::Settings => {
                let height = self.settings.0.array.len()+2;
                let height2 = self.settings.1.array.len()+2;
                let layout = Layout::horizontal([
                    Constraint::Length(40),
                    Constraint::Max(50)
                ].into_iter()).flex(Flex::Center).split(area);
                self.settings.0.render(center_vertical(layout[0], height.try_into().unwrap()), buf);
                self.settings.1.render(center_vertical(layout[1], height2.try_into().unwrap()), buf);
            },
            MinesweeperGameState::TooSmall => {
                let field_width = self.field.get_display_width()+2;
                let field_height = self.field.get_display_height()+2;
                let text = vec![
                    "Your terminal is too small".into(),
                    "Recommened minimum size:".into(),
                    format!("{}x{}", field_width, field_height).into(),
                    "With info panel:".into(),
                    format!("{}x{}", field_width+self.info_panel_min_width, field_height).into(),
                ];
                Paragraph::new(text)
                    .block(Block::bordered().title("Too small!!!").title_alignment(Alignment::Center))
                    .style(Style::new().white())
                    .alignment(Alignment::Center)
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
    fn init(&mut self, dim: Point, mines: u16, show_info: bool, delta_mode: bool, sweep_mode: bool) {
        self.field.init(dim, mines, delta_mode, sweep_mode);
        self.state = MinesweeperGameState::Running;
        self.show_info = show_info;
        self.info = KeyValueList::new(false, "Game Info".to_string(), vec![
            ("Delta mode:".to_string(),      self.field.delta_mode_str()),
            ("Sweep mode:".to_string(),      self.field.sweep_mode_str()),
            //("Seed:".to_string(),            self.field.seed_str()),
            ("Cells uncovered:".to_string(), self.field.cells_uncovered_str()),
            ("Mines Flagged:".to_string(),   self.field.mines_flagged_str()),
            ("Dimensions:".to_string(),      self.field.dim_str()),
            ("Location:".to_string(),        self.field.loc_str()),
            ("Started at:".to_string(),      self.field.started_str()),
            ("Time elapsed:".to_string(),    self.field.time_elapsed_str()),
            ("Game state:".to_string(),      self.field.state_str())
        ]);
        self.controls = MinesweeperGame::get_controls();
        self.settings = (
            KeyValueList::new(true, "Game Settings".to_string(), vec![
                ("Size".to_string(),             SettingsOption {enabled: false, option_type: SettingsOptionTypes::None, value: 0}),
                ("├─ x:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.x as u16}),
                ("├─ y:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.y as u16}),
                ("├─ z:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.z as u16}),
                ("└─ w:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.w as u16}),
                ("Mines:".to_string(),           SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.mines}),
                ("Show info:".to_string(),       SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.show_info {1} else {0}}),
                ("Delta mode:".to_string(),      SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.field.delta_mode {1} else {0}}),
                ("Sweep mode:".to_string(),      SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.field.sweep_mode {1} else {0}}),
                //("Use random seed:".to_string(), SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: 1}),
                //("└─ Seed:".to_string(),         SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: 0}),
            ]),
            KeyValueList::new(false, "Settings Controls".to_string(), vec![
                ("Exit".to_string(),             "ctrl+C, q, ESC".to_string()),
                ("Controls:".to_string(),        "c".to_string()),
                ("Save and exit".to_string(),    "o".to_string()),
                ("Move up:".to_string(),         "Uparrow, k, w, ctrl+k".to_string()),
                ("Move down:".to_string(),       "Downarrow, j, s, ctrl+j".to_string()),
                ("Increment value:".to_string(), "Rightarrow, l, d, ctrl+l".to_string()),
                ("Decrement value:".to_string(), "Leftarrow, h, a, ctrl+h".to_string()),
                ("0..9:".to_string(),            "edit value".to_string()),
                ("Toggle on".to_string(),        "y, t".to_string()),
                ("Toggle off".to_string(),       "n, f".to_string()),
        ]));
        self.info_panel_min_width = 25;
        self.info_panel_max_width = 50;
    }

    fn get_controls() -> KeyValueList<String> {
        KeyValueList::new(false, "Game Controls".to_string(), vec![
            ("Quit:".to_string(),                 "ctrl+C, q, ESC".to_string()),
            ("Controls:".to_string(),             "c".to_string()),
            ("Settings:".to_string(),             "o".to_string()),
            ("Move left in x:".to_string(),       "Leftarrow, h".to_string()),
            ("Move right in x:".to_string(),      "Rightarrow, l".to_string()),
            ("Move up in y:".to_string(),         "Uparrow, k".to_string()),
            ("Move down in y:".to_string(),       "Downarrow, j".to_string()),
            ("Move left in z:".to_string(),       "a, ctrl+h".to_string()),
            ("Move right in z:".to_string(),      "d, ctrl+l".to_string()),
            ("Move up in w:".to_string(),         "w, ctrl+k".to_string()),
            ("Move down in w:".to_string(),       "s, ctrl+j".to_string()),
            ("New game:".to_string(),             "n".to_string()),
            ("Find free cell:".to_string(),       "f".to_string()),
            ("Uncover cell:".to_string(),         "SPACE".to_string()),
            ("Give up/reveal field:".to_string(), "g".to_string()),
            ("Flag cell:".to_string(),            "m, e".to_string()),
            ("Flag cell chording:".to_string(),   "M, E".to_string()),
            ("Pause game:".to_string(),           "p".to_string()),
            ("Toggle info:".to_string(),          "i".to_string()),
            ("Toggle delta mode:".to_string(),    "u".to_string()),
            ("Toggle sweep mode:".to_string(),    "U".to_string()),
        ])
    }

    fn toggle_delta_mode(&mut self) {
        self.field.delta_mode = !self.field.delta_mode;
        self.info.array[0].1 = self.field.delta_mode_str();
    }

    fn toggle_sweep_mode(&mut self) {
        self.field.sweep_mode = !self.field.sweep_mode;
        self.info.array[1].1 = self.field.sweep_mode_str();
        if self.field.sweep_mode {
            for cell in self.field.field.clone() {
                if cell.is_flagged {
                    self.field.cell_at(cell.coord).unwrap().is_flagged = false;
                    self.field.toggle_flagged(cell.coord);
                }
            }
        }
    }

    fn update_info_cells_uncovered(&mut self) {
        self.info.array[2].1 = self.field.cells_uncovered_str();
    }

    fn update_info_mines_flagged(&mut self) {
        self.info.array[3].1 = self.field.mines_flagged_str();
    }

    fn update_info_loc(&mut self) {
        self.info.array[5].1 = self.field.loc_str();
    }

    fn update_info_started(&mut self) {
        self.info.array[6].1 = self.field.started_str();
    }

    fn update_info_time_elapsed(&mut self) {
        self.info.array[7].1 = self.field.time_elapsed_str();
    }

    fn update_info_state(&mut self) {
        self.info.array[8].1 = self.field.state_str();
    }

    fn move_in_field(&mut self, f: impl Fn(&mut MinesweeperField)) {
        f(&mut self.field);
        self.update_info_loc();
    }

    fn regenerate_field(&mut self) {
        self.field.regenerate();
        self.info.array[0].1 = self.field.delta_mode_str();
        self.info.array[1].1 = self.field.sweep_mode_str();
        //self.info.array[1].1 = self.field.seed_str();
        self.info.array[2].1 = self.field.cells_uncovered_str();
        self.info.array[3].1 = self.field.mines_flagged_str();
        self.info.array[4].1 = self.field.dim_str();
        self.info.array[5].1 = self.field.loc_str();
        self.info.array[6].1 = self.field.started_str();
        self.info.array[7].1 = self.field.time_elapsed_str();
        self.info.array[8].1 = self.field.state_str();
    }

    fn apply_settings(&mut self) {
        self.field.dim = Point {
            x: self.settings.0.array[1].1.value as i16,
            y: self.settings.0.array[2].1.value as i16,
            z: self.settings.0.array[3].1.value as i16,
            w: self.settings.0.array[4].1.value as i16,
        };
        self.field.area = self.field.dim.calc_area();
        self.field.mines = self.settings.0.array[5].1.value;
        self.show_info = if self.settings.0.array[6].1.value == 1 {true} else {false};
        self.field.delta_mode = if self.settings.0.array[7].1.value == 1 {true} else {false};
        /*if self.settings.array[8].1.value == 1 {
            //self.field.rng = ;
            eprintln!("do random seed");
        } else {
            eprintln!("no!");
        }*/
    }
}

fn main() -> color_eyre::Result<()> {
    let mut dim = Point {x: 4, y: 4, z: 4, w: 4};
    let mut mines: u16 = 20;
    let mut show_info = true;
    let mut delta_mode = true;
    let mut sweep_mode = false;
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "-?" | "--help" => {
                let controls = MinesweeperGame::get_controls();
                println!("{}", controls.title);
                let width = (controls.constraint_len+1) as usize;
                for c in controls.array {
                    println!("  {}{}", format!("{:width$}", c.0), c.1);
                }
                println!("Commandline arguments");
                println!("  -h, -?, --help            Show this menu");
                println!("  -d, --dim, --dimension    Change field dimensions. An array of unsigned integers e.g.: -d 4 4 4 4");
                println!("  -m, --mines               Change amount of mines. An unsigned integer");
                println!("  -i, --show_info           Toggle info box. A boolean value t/f or true/false or y/n or yes/no (any capitalisation)");
                println!("  -u, --delta_mode          Toggle delta mode. A boolean value t/f or true/false or y/n or yes/no (any capitalisation)");
                println!("  -U, --sweep_mode          Toggle sweep mode. A boolean value t/f or true/false or y/n or yes/no (any capitalisation)");
                println!("Default settings as a command");
                println!("  minesweeper_4d -d 4 4 4 4 -m 20 -i t -u t");
                return Ok(())
            },
            "-d" | "--dim" | "--dimension" => {
                let x = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.x = x.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", x, arg).as_str());
                let y = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.y = y.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", y, arg).as_str());
                let z = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.z = z.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", z, arg).as_str());
                let w = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.w = w.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", w, arg).as_str());
            },
            "-m" | "--mines" => {
                let value = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                mines = value.parse::<u16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", value, arg).as_str());
            },
            "-i" | "--show_info" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" => show_info = true,
                    "f" | "n" | "false" | "no" => show_info = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-u" | "--delta_mode" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" => delta_mode = true,
                    "f" | "n" | "false" | "no" => delta_mode = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-U" | "--sweep_mode" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" => sweep_mode = true,
                    "f" | "n" | "false" | "no" => sweep_mode = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            &_ => {
                panic!("Unrecognized option \"{arg}\"");
            }
        }
    }
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = App::new().run(terminal, dim, mines, show_info, delta_mode, sweep_mode);
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
    pub fn run(mut self, mut terminal: DefaultTerminal, dim: Point, mines: u16, show_info: bool, delta_mode: bool, sweep_mode: bool) -> Result<()> {
        self.running = true;
        self.game.init(dim, mines, show_info, delta_mode, sweep_mode);
        while self.running {
            terminal.draw(|frame| self.render(frame))?;
            let start = Instant::now();
            self.handle_crossterm_events()?;
            if matches!(self.game.field.state, MinesweeperFieldState::Running) {
                self.game.field.duration += start.elapsed();
                self.game.update_info_time_elapsed();
            }
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
        area.y += 1;
        area.height -= 1;
        frame.render_widget(
            title,
            frame.area()
        );
        self.check_size(area.width, area.height, self.game.state.clone());
        frame.render_widget(
            self.game.clone(),
            area,
        );
    }

    fn check_size(&mut self, width: u16, height: u16, default: MinesweeperGameState) {
        if self.game.field.get_display_height() > height ||
            (
                self.game.field.get_display_width()+
                (if self.game.show_info {self.game.info_panel_min_width} else {0})
            ) > width {
            self.game.state = MinesweeperGameState::TooSmall;
        } else {
            self.game.state = default;
        }
    }

    /// Reads the crossterm events and updates the state of [`App`].
    ///
    /// If your application needs to perform work in between handling events, you can use the
    /// [`event::poll`] function to check if there are any events available with a timeout.
    fn handle_crossterm_events(&mut self) -> Result<()> {
        if event::poll(Duration::from_millis(250))? {
            match event::read()? {
                // it's important to check KeyEventKind::Press to avoid handling key release events
                Event::Key(key) if key.kind == KeyEventKind::Press => self.on_key_event(key),
                Event::Mouse(_) => {}
                Event::Resize(width, height) => self.check_size(width, height, MinesweeperGameState::Running),
                _ => {}
            }
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
                            (_, KeyCode::Char('i')) => self.game.show_info = !self.game.show_info,
                            (_, KeyCode::Char('u')) => self.game.toggle_delta_mode(),
                            (_, KeyCode::Char('U')) => {
                                self.game.toggle_sweep_mode();
                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                            },
                            (_, KeyCode::Char('a')) | (KeyModifiers::CONTROL, KeyCode::Char('h')) => self.game.move_in_field(|f| f.move_left_z()),
                            (_, KeyCode::Char('d')) | (KeyModifiers::CONTROL, KeyCode::Char('l')) => self.game.move_in_field(|f| f.move_right_z()),
                            (_, KeyCode::Char('w')) | (KeyModifiers::CONTROL, KeyCode::Char('k')) => self.game.move_in_field(|f| f.move_up_w()),
                            (_, KeyCode::Char('s')) | (KeyModifiers::CONTROL, KeyCode::Char('j')) => self.game.move_in_field(|f| f.move_down_w()),
                            (_, KeyCode::Left | KeyCode::Char('h')) => self.game.move_in_field(|f| f.move_left_x()),
                            (_, KeyCode::Right | KeyCode::Char('l')) => self.game.move_in_field(|f| f.move_right_x()),
                            (_, KeyCode::Up | KeyCode::Char('k')) => self.game.move_in_field(|f| f.move_up_y()),
                            (_, KeyCode::Down | KeyCode::Char('j')) => self.game.move_in_field(|f| f.move_down_y()),
                            (_, KeyCode::Char('f')) => {
                                self.game.field.state = MinesweeperFieldState::Running;
                                self.game.field.find_free_cell();
                                self.game.field.started = Local::now();
                                self.game.update_info_cells_uncovered();
                                self.game.update_info_started();
                            },
                            (_, KeyCode::Char(' ')) => {
                                self.game.field.state = MinesweeperFieldState::Running;
                                self.game.field.uncover_cell(self.game.field.loc);
                                self.game.field.started = Local::now();
                                self.game.update_info_cells_uncovered();
                                self.game.update_info_started();
                            },
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::Running | MinesweeperFieldState::RevealField => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                            (_, KeyCode::Char('c')) => {
                                self.game.field.state = MinesweeperFieldState::Paused;
                                self.game.state = MinesweeperGameState::Controls;
                            },
                            (_, KeyCode::Char('o')) => {
                                self.game.field.state = MinesweeperFieldState::Paused;
                                self.game.state = MinesweeperGameState::Settings;
                            },
                            (_, KeyCode::Char('n')) => self.game.regenerate_field(),
                            (_, KeyCode::Char('i')) => self.game.show_info = !self.game.show_info,
                            (_, KeyCode::Char('u')) => self.game.toggle_delta_mode(),
                            (_, KeyCode::Char('U')) => {
                                self.game.toggle_sweep_mode();
                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                            },
                            (_, KeyCode::Char('a')) | (KeyModifiers::CONTROL, KeyCode::Char('h')) => self.game.move_in_field(|f| f.move_left_z()),
                            (_, KeyCode::Char('d')) | (KeyModifiers::CONTROL, KeyCode::Char('l')) => self.game.move_in_field(|f| f.move_right_z()),
                            (_, KeyCode::Char('w')) | (KeyModifiers::CONTROL, KeyCode::Char('k')) => self.game.move_in_field(|f| f.move_up_w()),
                            (_, KeyCode::Char('s')) | (KeyModifiers::CONTROL, KeyCode::Char('j')) => self.game.move_in_field(|f| f.move_down_w()),
                            (_, KeyCode::Left | KeyCode::Char('h')) => self.game.move_in_field(|f| f.move_left_x()),
                            (_, KeyCode::Right | KeyCode::Char('l')) => self.game.move_in_field(|f| f.move_right_x()),
                            (_, KeyCode::Up | KeyCode::Char('k')) => self.game.move_in_field(|f| f.move_up_y()),
                            (_, KeyCode::Down | KeyCode::Char('j')) => self.game.move_in_field(|f| f.move_down_y()),
                            (_, KeyCode::Char('p')) => self.game.field.state = MinesweeperFieldState::Paused,
                            (_, KeyCode::Char(' ')) => {
                                self.game.field.uncover_cell(self.game.field.loc);
                                self.game.update_info_cells_uncovered();
                            },
                            (_, KeyCode::Char('m') | KeyCode::Char('e')) => {
                                self.game.field.toggle_flagged(self.game.field.loc);
                                self.game.update_info_mines_flagged();
                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                            },
                            (_, KeyCode::Char('M') | KeyCode::Char('E')) => {
                                self.game.field.toggle_flagged_chording(self.game.field.loc);
                                self.game.update_info_mines_flagged();
                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                            },
                            (_, KeyCode::Char('g')) => {
                                if !matches!(self.game.field.state, MinesweeperFieldState::RevealField) {
                                    self.game.field.state = MinesweeperFieldState::GaveUp;
                                } else {
                                    self.game.field.do_everywhere(|f, p| f.cell_at(p).unwrap().set_covered(false));
                                    self.game.field.uncovered_cells = self.game.field.area as u16;
                                    self.game.update_info_cells_uncovered();
                                }
                            },
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::ClickedMine | MinesweeperFieldState::GaveUp | MinesweeperFieldState::Won => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q') | KeyCode::Char('g'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.field.state = MinesweeperFieldState::RevealField,
                            (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                            (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                            (_, KeyCode::Char('n')) => self.game.regenerate_field(),
                            (_, KeyCode::Char('i')) => self.game.show_info = !self.game.show_info,
                            (_, KeyCode::Char('u')) => self.game.toggle_delta_mode(),
                            (_, KeyCode::Char('U')) => {
                                self.game.toggle_sweep_mode();
                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                            },
                            _ => {}
                        }
                    },
                    MinesweeperFieldState::Paused => {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Esc | KeyCode::Char('q') | KeyCode::Char('p'))
                            | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.field.state = MinesweeperFieldState::Running,
                            (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                            (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                            (_, KeyCode::Char('n')) => self.game.regenerate_field(),
                            (_, KeyCode::Char('i')) => self.game.show_info = !self.game.show_info,
                            _ => {}
                        }
                    },
                }
                self.game.update_info_state(); //maybe change this, so it doesn't get called too
                                               //often
            },
            MinesweeperGameState::Controls => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q') | KeyCode::Char('c'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('C')) => self.game.state = MinesweeperGameState::Running,
                    (_, KeyCode::Char('o')) => self.game.state = MinesweeperGameState::Settings,
                    _ => {}
                }
            },
            MinesweeperGameState::Settings => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.game.state = MinesweeperGameState::Running,
                    (_, KeyCode::Char('c')) => self.game.state = MinesweeperGameState::Controls,
                    (_, KeyCode::Char('o') | KeyCode::Enter) => {
                        self.game.apply_settings();
                        self.game.regenerate_field();
                        self.game.state = MinesweeperGameState::Running;
                    },
                    (_, KeyCode::Left | KeyCode::Char('h') | KeyCode::Char('a')) => self.game.settings.0.get_tuple().1.dec(),
                    (_, KeyCode::Right | KeyCode::Char('l') | KeyCode::Char('d')) => self.game.settings.0.get_tuple().1.inc(),
                    (_, KeyCode::Up | KeyCode::Char('k') | KeyCode::Char('w')) => self.game.settings.0.dec_pos(),
                    (_, KeyCode::Down | KeyCode::Char('j') | KeyCode::Char('s')) => self.game.settings.0.inc_pos(),
                    (_, KeyCode::Char('0')) => self.game.settings.0.get_tuple().1.append(0),
                    (_, KeyCode::Char('1')) => self.game.settings.0.get_tuple().1.append(1),
                    (_, KeyCode::Char('2')) => self.game.settings.0.get_tuple().1.append(2),
                    (_, KeyCode::Char('3')) => self.game.settings.0.get_tuple().1.append(3),
                    (_, KeyCode::Char('4')) => self.game.settings.0.get_tuple().1.append(4),
                    (_, KeyCode::Char('5')) => self.game.settings.0.get_tuple().1.append(5),
                    (_, KeyCode::Char('6')) => self.game.settings.0.get_tuple().1.append(6),
                    (_, KeyCode::Char('7')) => self.game.settings.0.get_tuple().1.append(7),
                    (_, KeyCode::Char('8')) => self.game.settings.0.get_tuple().1.append(8),
                    (_, KeyCode::Char('9')) => self.game.settings.0.get_tuple().1.append(9),
                    //idk if i could've done that better...
                    (_, KeyCode::Char('y') | KeyCode::Char('Y') | KeyCode::Char('t') | KeyCode::Char('T')) => self.game.settings.0.get_tuple().1.set_bool(true),
                    (_, KeyCode::Char('n') | KeyCode::Char('N') | KeyCode::Char('f') | KeyCode::Char('F')) => self.game.settings.0.get_tuple().1.set_bool(false),
                    (_, KeyCode::Delete | KeyCode::Backspace) => self.game.settings.0.get_tuple().1.del(),
                    _ => {}
                }
            },
            MinesweeperGameState::TooSmall => {
                match (key.modifiers, key.code) {
                    (_, KeyCode::Esc | KeyCode::Char('q'))
                    | (KeyModifiers::CONTROL, KeyCode::Char('c') | KeyCode::Char('C')) => self.quit(),
                    (_, KeyCode::Char('i')) => self.game.show_info = !self.game.show_info,
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
