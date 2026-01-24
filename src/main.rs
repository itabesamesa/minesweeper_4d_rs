use std::{
    cmp::Ordering,
    fmt,
    env,
    str,
    time::{Duration, Instant},
    collections::HashMap,
    fs::{File, read_to_string},
    io::Write,
    path::{Path, PathBuf},
    str::FromStr,
};
use color_eyre::Result;
use crossterm::{
    event::{
        self, Event,
        KeyCode, KeyEvent, KeyEventKind, KeyModifiers,
        EnableBracketedPaste, EnableFocusChange, EnableMouseCapture, DisableBracketedPaste, DisableFocusChange, DisableMouseCapture,
        MouseEvent, MouseEventKind, MouseButton,
    },
    execute,
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
    Rng, SeedableRng, RngCore,
    rngs::StdRng,
};
use chrono::{DateTime, Local, FixedOffset};
use len_trait::Len;
use directories::UserDirs;
use config::{Config, Value, ValueKind, Map};

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

    fn from_str(s: &str) -> Option<MinesweeperFieldState> {
        match s {
            "New Game" => Some(MinesweeperFieldState::New),
            "Game Running" => Some(MinesweeperFieldState::Running),
            "Game Paused" => Some(MinesweeperFieldState::ClickedMine),
            "Game Capitulated" => Some(MinesweeperFieldState::GaveUp),
            "Revealed Game" => Some(MinesweeperFieldState::RevealField),
            "Won" => Some(MinesweeperFieldState::Won),
            _ => None,
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

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
enum Dimension {
    #[default] X,
    Y,
    Z,
    W,
}

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
enum Movement {
    #[default] Left,
    Right,
    Up,
    Down,
}

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
enum GameFunction {
    #[default] New,
    Retry,
    Free,
    Uncover,
    Capitulate,
    Flag,
    FlagChording,
    Pause,
    Save,
    ToggleInfo,
    ToggleDelta,
    ToggleSweep,
    MarkSet,
    MarkUncover,
    MarkFlag,
    MarkClear,
}

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
enum GlobalFunction {
    #[default] QuitAll,
    Quit,
    Controls,
    Settings,
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

    fn random_range<T: SeedableRng + RngCore>(&mut self, rng: &mut T, max: Point) {
        self.x = (rng.next_u32()%max.x as u32) as i16;
        self.y = (rng.next_u32()%max.y as u32) as i16;
        self.z = (rng.next_u32()%max.z as u32) as i16;
        self.w = (rng.next_u32()%max.w as u32) as i16;
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
    scroll_buffer: usize,
    highlight: bool,
    title: String,
    constraint_len_key: u16,
    constraint_len_value: u16,
    move_direction: bool,
    alignment: (Alignment, Alignment),
    array: Vec<(String, T)>,
}

impl<T: std::fmt::Display> Widget for KeyValueList<T> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let block = Block::bordered()
            .border_style(Style::default().fg(Color::White))
            .style(Style::default().fg(Color::White))
            .title(self.title);
        let layout = Layout::horizontal([
            Constraint::Length(self.constraint_len_key),
            Constraint::Length(self.constraint_len_value),
        ]).flex(Flex::SpaceBetween).split(block.inner(area));
        let mut start = 0;
        let mut end = self.array.len();
        let offset = self.pos+self.scroll_buffer;
        let height: usize = layout[0].height.into();
        if self.array.len() > height {
            end = if offset > self.array.len() {self.array.len()} else {offset};
            if self.move_direction {
                if height < end {
                    start = end-height;
                } else {
                    end = height;
                }
            } else {
                if end < height {
                    end = height;
                } else {
                    start = end-height;
                }
            }
        }
        let tmp = self.array[start..end].into_iter().enumerate();
        let (keys, values): (Vec<ratatui::prelude::Line>, Vec<ratatui::prelude::Line>) = if self.highlight {
            tmp.map(|x| {
                if x.0 == self.pos-start {
                    (Line::raw(x.1.0.clone()).style(Modifier::UNDERLINED), Line::raw(format!("{}", x.1.1)).style(Modifier::UNDERLINED))
                } else {
                    (Line::raw(x.1.0.clone()), Line::raw(format!("{}", x.1.1)))
                }
            }).unzip()} else {tmp.map(|x| (Line::raw(x.1.0.clone()), Line::raw(format!("{}", x.1.1)))).unzip()};
        block.render(area, buf);
        Paragraph::new(keys)
            .style(Style::new().white())
            .alignment(self.alignment.0)
            .wrap(Wrap { trim: true })
            .render(
                layout[0],
                buf
            );
        Paragraph::new(values)
            .style(Style::new().white())
            .alignment(self.alignment.1)
            .wrap(Wrap { trim: true })
            .render(
                layout[1],
                buf
            );
        if start != 0 {
            buf.set_string(area.left(), area.top()+1, "🮧", Style::new().white());
        }
        if end != self.array.len() {
            buf.set_string(area.left(), area.bottom()-2, "🮦", Style::new().white());
        }
    }
}

impl<T: Len + std::fmt::Display> KeyValueList<T> {
    fn new(highlight: bool, title: String, alignment: (Alignment, Alignment), array: Vec<(String, T)>) -> KeyValueList<T> {
        KeyValueList {
            pos: 0,
            scroll_buffer: array.len()/2+1,
            highlight: highlight,
            title: title, // got frustrated... don't question the constraint_len stuff
            constraint_len_key: array.iter().reduce(|a, b| {if a.0.len() < b.0.len() {b} else {a}}).unwrap().0.len() as u16 +1,
            constraint_len_value: array.iter().reduce(|a, b| {if a.1.len() < b.1.len() {b} else {a}}).unwrap().1.len() as u16 +1,
            move_direction: true, //+1 cause of borders
            alignment: alignment,
            array: array
        }
    }

    fn inc_pos_wrap(&mut self) {
        self.pos = (self.pos+1)%self.array.len();
        self.move_direction = true;
    }

    fn dec_pos_wrap(&mut self) {
        self.pos = (self.array.len()+self.pos-1)%self.array.len();
        self.move_direction = false;
    }

    fn inc_pos(&mut self) {
        if self.pos+1 != self.array.len() {self.pos += 1;}
        self.move_direction = true;
    }

    fn dec_pos(&mut self) {
        if self.pos != 0 {self.pos -= 1;}
        self.move_direction = false;
    }

    fn get_tuple(&mut self) -> &mut (String, T) {
        &mut self.array[self.pos]
    }

    fn as_str(&self, padding: usize) -> String {
        let width = (self.constraint_len_key+1) as usize;
        let mut s = String::new();
        for c in &self.array {
            s.push_str(&format!("{: <padding$}{:width$}{}\n", "", c.0, c.1));
        }
        s
    }

    fn recalc_constraint_len(&mut self) { //+1 cause of borders
        self.constraint_len_key = self.array.iter().reduce(|a, b| {if a.0.len() < b.0.len() {b} else {a}}).unwrap().0.len() as u16 +1;
        self.constraint_len_value = self.array.iter().reduce(|a, b| {if a.1.len() < b.1.len() {b} else {a}}).unwrap().1.len() as u16 +1;
    }
}

#[derive(Clone, Debug, Default)]
struct SettingsOption {
    enabled: bool,
    option_type: SettingsOptionTypes,
    value: u64,
    min: u64,
    max: u64,
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
            SettingsOptionTypes::Int => if self.value < self.max {self.value += 1},
            _ => {},
        }
    }

    fn dec(&mut self) {
        match self.option_type {
            SettingsOptionTypes::Bool => self.toggle_bool(),
            SettingsOptionTypes::Int => {
                if self.value > self.min {
                    self.value -= 1;
                }
            },
            _ => {},
        }
    }

    fn append(&mut self, v: u64) {
        match self.option_type {
            SettingsOptionTypes::Bool => if v != 0 {self.value = 1;} else {self.value = 0;},
            SettingsOptionTypes::Int => {
                let tmp = self.value.saturating_mul(10).saturating_add(v);
                if tmp > self.max {
                    self.value = self.max;
                } else {
                    self.value = tmp;
                }
            },
            _ => {},
        }
    }
 
    fn del(&mut self) {
        match self.option_type {
            SettingsOptionTypes::Bool => self.toggle_bool(),
            SettingsOptionTypes::Int => {
                let tmp = self.value/10;
                if tmp < self.min {
                    self.value = self.min;
                } else {
                    self.value = tmp;
                }
            },
            _ => {},
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Mark {
    color: Color,
    total: usize,
    amount: usize,
    mines: usize,
    origin: Point,
}

impl Mark {
    fn new() -> Mark {
        Mark {
            color: Color::Black,
            total: 0,
            amount: 0,
            mines: 0,
            origin: Point::new(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct MinesweeperCellColors {
    cursor: Color,
    wrong: Color,
    text: Color,
    light_covered_default: Color,
    light_uncovered_default: Color,
    light_covered_neighbour: Color,
    light_uncovered_neighbour: Color,
    dark_covered_default: Color,
    dark_uncovered_default: Color,
    dark_covered_neighbour: Color,
    dark_uncovered_neighbour: Color,
}

impl Default for MinesweeperCellColors {
    fn default() -> MinesweeperCellColors {
        MinesweeperCellColors {
            cursor: Color::Rgb(0xff, 0x2a, 0xff),
            wrong: Color::Red,
            text: Color::Black,
            light_covered_default: Color::Rgb(0x66, 0x66, 0x66),
            light_uncovered_default: Color::Rgb(0xc6, 0xc6, 0xc6),
            light_covered_neighbour: Color::Rgb(0x80, 0x73, 0x80),
            light_uncovered_neighbour: Color::Rgb(0xb3, 0xa1, 0xb3),
            dark_covered_default: Color::Rgb(0x3b, 0x3b, 0x3b),
            dark_uncovered_default: Color::Rgb(0xb8, 0xb8, 0xb8),
            dark_covered_neighbour: Color::Rgb(0x59, 0x50, 0x59),
            dark_uncovered_neighbour: Color::Rgb(0xa6, 0x95, 0xa6),
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
    print_zero: bool,
    is_marked: bool,
    mark: Mark,
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
            print_zero: false,
            is_marked: false,
            mark: Mark::new(),
        }
    }

    fn render_mark(self, area: Rect, buf: &mut Buffer) {
        let fg = match self.mark.color {
            Color::Rgb(r, g, b) => {
                if g > 180 || (r as u16)+(g as u16)+(b as u16) > 450 { // yoinked from https://ux.stackexchange.com/a/151290
                    Color::Black
                } else {
                    Color::White
                }
            },
            _ => Color::White,
        };
        buf.set_string(area.left(), area.top(),
            if self.mark.mines < 10 {
                char::from_u32(0xff10+(self.mark.mines as u32)).unwrap().to_string()
            } else {
                format!("{:2}", self.mark.mines)
            }, Style::default().fg(fg).bg(if self.is_active {Color::Rgb(255, 42, 255)} else {self.mark.color}))
    }

    fn render_rel(self, area: Rect, buf: &mut Buffer, colors: MinesweeperCellColors) {
        if self.is_marked && !self.is_flagged {
            self.render_mark(area, buf);
        } else {
            buf.set_string(area.left(), area.top(), self.get_rel_string(), Style::default().fg(colors.text).bg(self.get_color(colors)));
        }
    }

    fn render_abs(self, area: Rect, buf: &mut Buffer, colors: MinesweeperCellColors) {
        if self.is_marked && !self.is_flagged {
            self.render_mark(area, buf);
        } else {
            buf.set_string(area.left(), area.top(), self.get_abs_string(), Style::default().fg(colors.text).bg(self.get_color(colors)));
        }
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

    fn get_color(&self, colors: MinesweeperCellColors) -> Color {
        if self.is_active {
            colors.cursor
        } else {
            if self.is_covered {
                if self.in_active_neighbourhood {
                    self.get_light_dark_color(colors.light_covered_neighbour, colors.dark_covered_neighbour)
                } else {
                    self.get_light_dark_color(colors.light_covered_default, colors.dark_covered_default)
                }
            } else {
                if self.is_flagged && !self.is_bomb {
                    colors.wrong
                } else if self.in_active_neighbourhood {
                    self.get_light_dark_color(colors.light_uncovered_neighbour, colors.dark_uncovered_neighbour)
                } else {
                    self.get_light_dark_color(colors.light_uncovered_default, colors.dark_uncovered_default)
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

    fn rm_mark(&mut self) {
        self.is_marked = false;
        self.mark.color = Color::Black;
        self.mark.mines = 0;
    }

    fn as_str(&self) -> String {
        format!("({:03} {:03} {:03})", ((((((
                            if self.print_zero {1} else {0}) << 1) +
                            if self.is_bomb {1} else {0}) << 1) +
                            if self.is_covered {1} else {0}) << 1) +
                            if self.is_flagged {1} else {0},
                self.abs,
                self.rel)
    }
}

#[derive(Clone, Debug)]
struct MinesweeperField {
    state: MinesweeperFieldState,
    dim: Point,
    rng: StdRng,
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
    field: Vec<MinesweeperCell>,
    marks: HashMap<Color, Mark>,
    colors: MinesweeperCellColors,
}

impl Default for MinesweeperField {
    fn default() -> MinesweeperField {
        MinesweeperField {
            state: MinesweeperFieldState::default(),
            dim: Point::default(),
            rng: SeedableRng::seed_from_u64(0),
            seed: 0,
            mines: 0,
            loc: Point::default(),
            area: 0,
            uncovered_cells: 0,
            flagged_mines: 0,
            delta_mode: true,
            sweep_mode: false,
            started: Local::now(),
            duration: Duration::ZERO,
            field: Vec::default(),
            marks: HashMap::new(),
            colors: MinesweeperCellColors::default(),
            //..Default::default() // this causes a stack overflow, so tedious way it is?
        }
    }
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
                                self.field[Point {x: x, y: y, z: z, w: w}.to_1d(self.dim)].render_rel(cell_area, buf, self.colors);
                            } else {
                                self.field[Point {x: x, y: y, z: z, w: w}.to_1d(self.dim)].render_abs(cell_area, buf, self.colors);
                            }
                        }
                    }
                }
            }
            if matches!(self.state, MinesweeperFieldState::ClickedMine)
                || matches!(self.state, MinesweeperFieldState::GaveUp)
                || matches!(self.state, MinesweeperFieldState::Won) {
                let mut message_box = centered_rect(area, 18, 3);
                buf.set_string(message_box.left(), message_box.top(), "                 ", Style::new());
                buf.set_string(message_box.left(), message_box.top()+1, "                 ", Style::new());
                buf.set_string(message_box.left(), message_box.top()+2, "                 ", Style::new());
                // to prevent a display bug, where numbers are rendered ontop of the message_box
                if (message_box.left()-area.left())%2 == 1 {message_box.x -= 1;}
                Paragraph::new(format!("{}", self.state.as_str()))
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
    fn init(&mut self, dim: Point, mines: u16, delta_mode: bool, sweep_mode: bool, seed: u64) {
        self.dim = dim;
        self.seed = seed;
        self.rng = SeedableRng::seed_from_u64(self.seed);
        self.mines = mines;
        self.delta_mode = delta_mode;
        self.sweep_mode = sweep_mode;
        self.area = self.dim.calc_area();
        self.started = Local::now();
        self.duration = Duration::ZERO;
        self.marks = HashMap::new();
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

    fn regenerate(&mut self, regen_seed: bool) {
        self.field.clear();
        self.marks.clear();
        //self.seed = self.rng.next_u64(); // this is a terible idea but :3
        if regen_seed {self.seed = rand::random::<u64>();}
        self.rng = StdRng::seed_from_u64(self.seed);
        self.fill_field();
        self.set_active_cell(true);
        self.place_mines();
        self.state = MinesweeperFieldState::New;
        self.uncovered_cells = 0;
        self.flagged_mines = 0;
        self.started = Local::now();
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

    fn find_in_neighbourhood<T>(&mut self, p: Point, f: impl Fn(&mut MinesweeperField, Point) -> Option<T>) -> Vec<T> {
        let mut v: Vec<T> = Vec::new();
        for w in -1..=1 {
            for z in -1..=1 {
                for y in -1..=1 {
                    for x in -1..=1 {
                        let coord = p.offset(Point {x: x, y: y, z: z, w: w});
                        if (Point {x: -1, y: -1, z: -1, w: -1}) < coord && coord < self.dim {
                            //this comp is on purpose cause it kinda funky...
                            match f(self, coord) {
                                Some(r) => v.push(r),
                                None => (),
                            }
                        }
                    }
                }
            }
        }
        v
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

    fn move_right_end_x(&mut self) {
        self.set_active_cell(false);
        self.loc.x = self.dim.x-1;
        self.set_active_cell(true);
    }

    fn move_right_z(&mut self) {
        if self.loc.z+1 < self.dim.z {
            self.set_active_cell(false);
            self.loc.z += 1;
            self.set_active_cell(true);
        }
    }

    fn move_right_end_z(&mut self) {
        self.set_active_cell(false);
        self.loc.z = self.dim.z-1;
        self.set_active_cell(true);
    }

    fn move_left_x(&mut self) {
        if self.loc.x-1 >= 0 {
            self.set_active_cell(false);
            self.loc.x -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_left_end_x(&mut self) {
        self.set_active_cell(false);
        self.loc.x = 0;
        self.set_active_cell(true);
    }

    fn move_left_z(&mut self) {
        if self.loc.z-1 >= 0 {
            self.set_active_cell(false);
            self.loc.z -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_left_end_z(&mut self) {
        self.set_active_cell(false);
        self.loc.z = 0;
        self.set_active_cell(true);
    }

    fn move_down_y(&mut self) {
        if self.loc.y+1 < self.dim.y {
            self.set_active_cell(false);
            self.loc.y += 1;
            self.set_active_cell(true);
        }
    }

    fn move_down_end_y(&mut self) {
        self.set_active_cell(false);
        self.loc.y = self.dim.y-1;
        self.set_active_cell(true);
    }

    fn move_down_w(&mut self) {
        if self.loc.w+1 < self.dim.w {
            self.set_active_cell(false);
            self.loc.w += 1;
            self.set_active_cell(true);
        }
    }

    fn move_down_end_w(&mut self) {
        self.set_active_cell(false);
        self.loc.w = self.dim.w-1;
        self.set_active_cell(true);
    }

    fn move_up_y(&mut self) {
        if self.loc.y-1 >= 0 {
            self.set_active_cell(false);
            self.loc.y -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_up_end_y(&mut self) {
        self.set_active_cell(false);
        self.loc.y = 0;
        self.set_active_cell(true);
    }

    fn move_up_w(&mut self) {
        if self.loc.w-1 >= 0 {
            self.set_active_cell(false);
            self.loc.w -= 1;
            self.set_active_cell(true);
        }
    }

    fn move_up_end_w(&mut self) {
        self.set_active_cell(false);
        self.loc.w = 0;
        self.set_active_cell(true);
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
                let dec_mark = cell.is_marked;
                let mark_color = cell.mark.color;
                cell.is_marked = false;
                if cell.is_bomb {
                    if !matches!(self.state, MinesweeperFieldState::RevealField) {
                        self.state = MinesweeperFieldState::ClickedMine;
                    }
                } else {
                    if cell.abs == 0 {
                        self.do_in_neighbourhood(p, |s, p2| {
                            if p2 != p  {
                                s.uncover_cell(p2);
                            }
                        });
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
                    if dec_mark {
                        match self.marks.get_mut(&mark_color) {
                            Some(m) => {
                                m.amount -= 1;
                                m.total -= 1;
                                if m.amount == 0 {
                                    let loc = m.origin;
                                    let _ = self.marks.remove(&mark_color);
                                    self.do_in_neighbourhood(loc, |s, p| {
                                        let cell = s.cell_at(p).unwrap();
                                        if cell.is_marked && cell.mark.color == mark_color {
                                            cell.rm_mark();
                                        }
                                    });
                                }
                            },
                            None => (),
                        }
                    }
                }
            } else if cell.rel == 0 {
                if self.delta_mode {
                    self.uncover_rel_cell(p);
                    //should still set print_zero when not in delta_mode to avoid graphical bugs when
                    //swapping between modes
                } else {
                    self.do_in_neighbourhood(p, |s, p2| {
                        if p2 != p  {
                            let cell = s.cell_at(p2).unwrap();
                            if cell.is_covered { // have to check here to make chording in non
                                                 // delta_mode work, otherwise it's an endless
                                                 // recrusive loop
                                s.uncover_cell(p2);
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
                    self.uncover_cell(p);
                    self.do_in_neighbourhood(p, |s, p| {
                        let cell = s.cell_at(p).unwrap();
                        if cell.rel == <u8 as TryInto<i8>>::try_into(cell.abs).unwrap() {
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
            let is_marked = cell.is_marked;
            let mark_color = cell.mark.color;
            if cell.is_flagged {
                self.do_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    cell.dec_rel();
                });
                self.flagged_mines += 1;
                if is_marked {
                    match self.marks.get_mut(&mark_color) {
                        Some(m) => {
                            m.amount -= 1;
                            m.total -= 1;
                            m.mines -= 1;
                            if m.amount == 0 || m.mines == 0 {
                                let loc = m.origin;
                                let _ = self.marks.remove(&mark_color);
                                self.do_in_neighbourhood(loc, |s, p| {
                                    let cell = s.cell_at(p).unwrap();
                                    if cell.is_marked && cell.mark.color == mark_color {
                                        cell.rm_mark();
                                    }
                                });
                            }
                        },
                        None => (),
                    }
                }
            } else {
                self.do_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    cell.inc_rel();
                });
                self.flagged_mines -= 1;
                if is_marked {
                    match self.marks.get_mut(&mark_color) {
                        Some(m) => {
                            m.amount += 1;
                            m.total += 1;
                            m.mines += 1;
                        },
                        None => (),
                    }
                }
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

    fn add_mark(&mut self, p: Point) {
        let cell = self.cell_at(p).unwrap();
        if !cell.is_covered && cell.rel >= 0 {
            let marked = self.find_in_neighbourhood(p, |s, p| {
                let cell = s.cell_at(p).unwrap();
                if cell.is_marked && !cell.is_flagged && cell.mark.color != Color::Black {
                    Some(cell.mark.color)
                } else {
                    None
                }
            });
            let mut marked_count: HashMap<Color, usize> = HashMap::new();
            for m in marked {
                *marked_count.entry(m).or_default() += 1;
            }
            let mut contained: Vec<Mark> = Vec::new();
            for (color, amount) in marked_count {
                match self.marks.get(&color) {
                    Some(m) => {
                        if m.total == amount {
                            contained.push(m.clone());
                        }
                    },
                    None => (),
                }
            }
            let mut mines: usize = self.cell_at(p).unwrap().rel.try_into().unwrap();
            for m in &contained {
                mines -= m.mines;
            }
            if mines != 0 {
                let c = Color::Rgb(self.rng.random_range(1..255), self.rng.random_range(1..255), self.rng.random_range(1..255)); // #000000 reserved for uncoverable cells
                let _: HashMap<Color, Mark> = self.marks.extract_if(|_k, v| v.origin == p).collect();
                self.marks.insert(c, Mark {color: c, total: 0, amount: 0, mines: mines, origin: p});
                self.do_in_neighbourhood(p, |s, p2| {
                    let cell = s.cell_at(p2).unwrap();
                    if cell.is_covered && !cell.is_flagged {
                        if cell.is_marked {
                            let cc = cell.mark.color;
                            if cc != Color::Black && !contained.iter().any(|x| x.color == cc) {
                                cell.mark.color = c;
                                cell.mark.mines = mines;
                                cell.mark.origin = p;
                                match s.marks.get_mut(&cc) {
                                    Some(m) => {
                                        m.amount -= 1;
                                        if m.amount == 0 {
                                            let _ = s.marks.remove(&cc);
                                        }
                                    },
                                    None => (),
                                }
                                match s.marks.get_mut(&c) {
                                    Some(m) => m.amount += 1,
                                    None => (),
                                }
                            }
                        } else {
                            cell.is_marked = true;
                            cell.mark.color = c;
                            cell.mark.mines = mines;
                            cell.mark.origin = p;
                            match s.marks.get_mut(&c) {
                                Some(m) => m.amount += 1,
                                None => (),
                            }
                        }
                    }
                });
                match self.marks.get_mut(&c) {
                    Some(m) => m.total = m.amount,
                    None => (),
                }
            } else {
                self.do_in_neighbourhood(p, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    if cell.is_covered && !cell.is_flagged {
                        if cell.is_marked && cell.mark.color != Color::Black && !contained.iter().any(|x| x.color == cell.mark.color) {
                            let cc = cell.mark.color;
                            cell.mark.color = Color::Black;
                            cell.mark.mines = 0;
                            match s.marks.get_mut(&cc) {
                                Some(m) => m.total -= 1,
                                None => (),
                            }
                        } else {
                            cell.is_marked = true;
                        }
                    }
                });
            }
        }
    }

    fn uncover_black_cell(&mut self) {
        for cell in &self.field.clone() {
            if cell.is_marked && cell.mark.color == Color::Black {
                let loc = cell.coord;
                self.uncover_cell(loc);
            }
        }
    }

    fn flag_definite_marked_cell(&mut self) {
        for (color, mark) in &self.marks.clone() {
            if mark.total == mark.amount && mark.total == mark.mines {
                self.do_in_neighbourhood(mark.origin, |s, p| {
                    let cell = s.cell_at(p).unwrap();
                    if cell.is_covered && cell.is_marked && cell.mark.color == *color {
                        let loc = cell.coord;
                        s.toggle_flagged(loc);
                    }
                });
                let _ = self.marks.remove(color);
            }
        }
    }

    fn clear_marks(&mut self) {
        for cell in &mut self.field { // maybe iterate through marks instead of field... could be
                                      // more efficient
            if cell.is_marked {
                cell.is_marked = false;
                cell.mark.color = Color::Black;
                cell.mark.mines = 0;
            }
        }
        self.marks.clear();
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

    fn seed_str(&self) -> String {
        format!("{}", self.seed)
    }

    fn state_str(&self) -> String {
        self.state.as_str().to_string()
    }

    fn field_str(&self) -> String {
        let mut w_bar: String = "-".repeat((13*self.dim.x).try_into().unwrap());
        w_bar.push_str("-+-");
        w_bar = w_bar.repeat(self.dim.z.try_into().unwrap());
        w_bar.truncate(w_bar.len()-3);
        w_bar.push('\n');
        let mut s = String::new();
        for w in 0..self.dim.w {
            for y in 0..self.dim.y {
                for z in 0..self.dim.z {
                    for x in 0..self.dim.x {
                        let coord = Point {x: x, y: y, z: z, w: w};
                        let cell = self.field[coord.to_1d(self.dim)];
                        s.push_str(&cell.as_str());
                    }
                    s.push_str(" | ");
                }
                s.truncate(s.len()-3);
                s.push('\n');
            }
            if w+1 != self.dim.w {
                s.push_str(&w_bar);
            }
        }
        s
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
                let (field_area, info_area) = self.game_area(area);

                self.field.render(
                    field_area,
                    buf
                );

                if self.show_info {
                    let height = self.info.array.len()+2;
                    self.info.render(center_vertical(info_area, height.try_into().unwrap()), buf);
                }
            },
            MinesweeperGameState::Controls => {
                let height = self.controls.array.len()+2;
                let width = self.controls.constraint_len_key+2+self.controls.constraint_len_value;
                self.controls.render(centered_rect(area, width, height.try_into().unwrap()), buf);
            },
            MinesweeperGameState::Settings => {
                let height = self.settings.0.array.len()+2;
                let height2 = self.settings.1.array.len()+2;
                let layout = Layout::horizontal([
                    Constraint::Length(40), //should make them base on KeyValueList, but eh
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
    fn init(&mut self, dim: Point, mines: u16, show_info: bool, delta_mode: bool, sweep_mode: bool, seed: u64) {
        self.field.init(dim, mines, delta_mode, sweep_mode, seed);
        self.state = MinesweeperGameState::Running;
        self.show_info = show_info;
        self.set_info((Alignment::Left, Alignment::Left));
        self.controls = MinesweeperGame::get_controls((Alignment::Left, Alignment::Left));
        self.settings = self.get_settings((Alignment::Left, Alignment::Left), (Alignment::Left, Alignment::Left));
    }

    fn set_info(&mut self, alignment: (Alignment, Alignment)) {
        self.info = self.get_info(alignment); // 10 cause it looks good...
        self.info_panel_min_width = self.info.constraint_len_key+2+self.info.constraint_len_value;
        self.info_panel_max_width = self.info.constraint_len_key+10+self.info.constraint_len_value;
    }

    fn get_controls(alignment: (Alignment, Alignment)) -> KeyValueList<String> {
        KeyValueList::new(true, "Game Controls".to_string(), alignment, vec![
            ("Quit all:".to_string(),                       "ctrl+c".to_string()),
            ("Quit:".to_string(),                           "q, ESC".to_string()),
            ("Controls:".to_string(),                       "c".to_string()),
            ("Settings:".to_string(),                       "o".to_string()),
            ("Move left in x:".to_string(),                 "Leftarrow,  h".to_string()),
            ("Move right in x:".to_string(),                "Rightarrow, l".to_string()),
            ("Move up in y:".to_string(),                   "Uparrow,    k".to_string()),
            ("Move down in y:".to_string(),                 "Downarrow,  j".to_string()),
            ("Move left in z:".to_string(),                 "a, ctrl+h".to_string()),
            ("Move right in z:".to_string(),                "d, ctrl+l".to_string()),
            ("Move up in w:".to_string(),                   "w, ctrl+k".to_string()),
            ("Move down in w:".to_string(),                 "s, ctrl+j".to_string()),
            ("Move to start in x:".to_string(),             "shift+Leftarrow,  H".to_string()),
            ("Move to end in x:".to_string(),               "shift+Rightarrow, L".to_string()),
            ("Move to top in y:".to_string(),               "shift+Uparrow,    K".to_string()),
            ("Move to bottom in y:".to_string(),            "shift+Downarrow,  J".to_string()),
            ("Move to start in z:".to_string(),             "A, alt+h".to_string()),
            ("Move to end in z:".to_string(),               "D, alt+l".to_string()),
            ("Move to top in w:".to_string(),               "W, alt+k".to_string()),
            ("Move to bottom in w:".to_string(),            "S, alt+j".to_string()),
            ("Retry game:".to_string(),                     "r".to_string()),
            ("New game:".to_string(),                       "n".to_string()),
            ("Find free cell:".to_string(),                 "f".to_string()),
            ("Uncover cell:".to_string(),                   "SPACE".to_string()),
            ("Give up/reveal field:".to_string(),           "g".to_string()),
            ("Flag cell:".to_string(),                      "m, e".to_string()),
            ("Flag cell chording:".to_string(),             "M, E".to_string()),
            ("Pause game:".to_string(),                     "p".to_string()),
            ("Toggle info:".to_string(),                    "i".to_string()),
            ("Toggle delta mode:".to_string(),              "u".to_string()),
            ("Toggle sweep mode:".to_string(),              "U".to_string()),
            ("Mark cell:".to_string(),                      "x".to_string()),
            ("Uncover obvious marked cells:".to_string(),   "X".to_string()),
            ("Flag obvious marked cells:".to_string(),      "alt+x".to_string()),
            ("Clear all marks:".to_string(),                "ctrl+x".to_string()),
            ("Save game:".to_string(),                      "ctrl+o".to_string()),
        ])
    }

    fn get_info(&self, alignment: (Alignment, Alignment)) -> KeyValueList<String> {
        KeyValueList::new(false, "Game Info".to_string(), alignment, vec![
            ("Delta mode:".to_string(),      self.field.delta_mode_str()),
            ("Sweep mode:".to_string(),      self.field.sweep_mode_str()),
            ("Cells uncovered:".to_string(), self.field.cells_uncovered_str()),
            ("Mines flagged:".to_string(),   self.field.mines_flagged_str()),
            ("Dimensions:".to_string(),      self.field.dim_str()),
            ("Location:".to_string(),        self.field.loc_str()),
            ("Started at:".to_string(),      self.field.started_str()),
            ("Time elapsed:".to_string(),    self.field.time_elapsed_str()),
            ("Seed:".to_string(),            self.field.seed_str()),
            ("Game state:".to_string(),      self.field.state_str())
        ])
    }

    fn get_settings(&self, alignment_settings: (Alignment, Alignment), alignment_controls: (Alignment, Alignment)) -> (KeyValueList<SettingsOption>, KeyValueList<String>) {
        (
            KeyValueList::new(true, "Game Settings".to_string(), alignment_settings, vec![
                ("Size".to_string(),             SettingsOption {enabled: false, option_type: SettingsOptionTypes::None, value: 0, min: 0, max: 0}),
                ("├─ x:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.x as u64, min: 1, max: i16::MAX as u64}),
                ("├─ y:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.y as u64, min: 1, max: i16::MAX as u64}),
                ("├─ z:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.z as u64, min: 1, max: i16::MAX as u64}),
                ("└─ w:".to_string(),            SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.dim.w as u64, min: 1, max: i16::MAX as u64}),
                ("Mines:".to_string(),           SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: self.field.mines as u64, min: 1, max: i16::MAX as u64}),
                ("Show info:".to_string(),       SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.show_info {1} else {0}, min: 0, max: 1}),
                ("Delta mode:".to_string(),      SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.field.delta_mode {1} else {0}, min: 0, max: 1}),
                ("Sweep mode:".to_string(),      SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: if self.field.sweep_mode {1} else {0}, min: 0, max: 1}),
                ("Use random seed:".to_string(), SettingsOption {enabled: true, option_type: SettingsOptionTypes::Bool, value: 1, min: 0, max: 1}),
                ("└─ Seed:".to_string(),         SettingsOption {enabled: true, option_type: SettingsOptionTypes::Int, value: 0, min: 0, max: u64::MAX}),
            ]),
            KeyValueList::new(false, "Settings Controls".to_string(), alignment_controls, vec![
                ("Exit".to_string(),             "q, ESC".to_string()),
                ("Controls:".to_string(),        "c".to_string()),
                ("Save and exit".to_string(),    "o".to_string()),
                ("Move up:".to_string(),         "any up movement key".to_string()),
                ("Move down:".to_string(),       "any down movement key".to_string()),
                ("Increment value:".to_string(), "any right movement key, +".to_string()),
                ("Decrement value:".to_string(), "any left movement key,  -".to_string()),
                ("0..9:".to_string(),            "edit value".to_string()),
                ("Toggle on".to_string(),        "y, t".to_string()),
                ("Toggle off".to_string(),       "n, f".to_string()),
        ]))
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
        self.info.array[9].1 = self.field.state_str();
    }

    fn move_in_field(&mut self, f: impl Fn(&mut MinesweeperField)) {
        f(&mut self.field);
        self.update_info_loc();
    }

    fn regenerate_field(&mut self) {
        self.field.regenerate(self.settings.0.array[9].1.value == 1);
        self.info.array[0].1 = self.field.delta_mode_str();
        self.info.array[1].1 = self.field.sweep_mode_str();
        //self.info.array[1].1 = self.field.seed_str();
        self.info.array[2].1 = self.field.cells_uncovered_str();
        self.info.array[3].1 = self.field.mines_flagged_str();
        self.info.array[4].1 = self.field.dim_str();
        self.info.array[5].1 = self.field.loc_str();
        self.info.array[6].1 = self.field.started_str();
        self.info.array[7].1 = self.field.time_elapsed_str();
        self.info.array[8].1 = self.field.seed_str();
        self.info.array[9].1 = self.field.state_str();
        self.info.recalc_constraint_len();
    }

    fn regenerate_field_same_seed(&mut self) {
        let tmp = self.settings.0.array[9].1.value;
        self.settings.0.array[9].1.value = 0;
        self.regenerate_field(); //fully regenerate_field cause of sweep_mode
        self.settings.0.array[9].1.value = tmp;
    }

    fn apply_settings(&mut self) {
        self.field.dim = Point {
            x: self.settings.0.array[1].1.value as i16,
            y: self.settings.0.array[2].1.value as i16,
            z: self.settings.0.array[3].1.value as i16,
            w: self.settings.0.array[4].1.value as i16,
        };
        self.field.area = self.field.dim.calc_area();
        self.field.mines = self.settings.0.array[5].1.value as u16;
        self.show_info = if self.settings.0.array[6].1.value == 1 {true} else {false};
        self.field.delta_mode = if self.settings.0.array[7].1.value == 1 {true} else {false};
        self.field.seed = self.settings.0.array[10].1.value;
    }

    fn game_area(&self, area: Rect) -> (Rect, Rect) {
        let layout = Layout::horizontal([
            Constraint::Length(self.field.get_display_width().try_into().unwrap()),
            Constraint::Max(if self.show_info {self.info_panel_max_width} else {0})
        ].into_iter()).flex(Flex::Center).split(area);
        (center_vertical(
            layout[0],
            self.field.get_display_height().try_into().unwrap()
        ), layout[1])
    }

    fn save_game(&self, mut dir: PathBuf) {
        let file_name = self.field.started_str().replace(" ", "_");
        dir.push(&format!("{}.4dminesweeper", file_name));
        if dir.exists() {
            dir.pop();
            let mut i = 1;
            dir.push(&format!("{}_v{}.4dminesweeper", file_name, i));
            while dir.exists() {
                i += 1;
                dir.pop();
            }
        }
        let file = File::create(dir);
        match file.unwrap().write_all(
            format!(
            "Save file for game run on {}\n
Info:
{}
Grid:
{}",
                self.field.started_str(),
                self.info.as_str(0),
                self.field.field_str()).as_bytes()
            ) {
            Err(e) => panic!("{}", e),
            _ => {},
        }
    }

    fn load_file(&mut self, file_name: String, show_info: bool) {
        let path = Path::new(&file_name);
        if !(path.exists() && path.is_file()) {
            panic!("Path \"{file_name}\" doesn't exist or isn't a file");
        } //unwrap already panics, i just don't like the error message, sooo
        let info = MinesweeperGame::default().get_info((Alignment::Left, Alignment::Left));
        let binding = read_to_string(path).unwrap();
        let mut lines = binding.lines();
        let mut found_grid = false;
        let mut cells_uncovered = false; // these 3 are necessary
        let mut mines_flagged = false;   // the reset should be fine
        let mut dimensions = false;      // fingers crossed...
        while let Some(line) = lines.next() {
            if line.starts_with(&info.array[0].0) {
                let (_, mut end) = line.split_at(info.array[0].0.len());
                end = end.trim();
                match end {
                    "On" => self.field.delta_mode = true,
                    "Off" => self.field.delta_mode = false,
                    _ => panic!("Wrong value for delta mode in file \"{}\"", file_name),
                }
            } else if line.starts_with(&info.array[1].0) {
                let (_, mut end) = line.split_at(info.array[1].0.len());
                end = end.trim();
                match end {
                    "On" => self.field.sweep_mode = true,
                    "Off" => self.field.sweep_mode = false,
                    _ => panic!("Wrong value for sweep mode in file \"{}\"", file_name),
                }
            } else if line.starts_with(&info.array[2].0) {
                let (_, mut end) = line.split_at(info.array[2].0.len());
                end = end.trim();
                let v: Vec<_> = end.split('/').collect();
                if v.len() != 2 {
                    panic!("Wrong value for cells uncovered in file \"{}\"", file_name);
                }
                self.field.uncovered_cells = v[0].parse::<u16>()
                    .expect(format!("Cells uncovered has wrong type in file \"{}\"", file_name).as_str());
                cells_uncovered = true;
            } else if line.starts_with(&info.array[3].0) {
                let (_, mut end) = line.split_at(info.array[3].0.len());
                end = end.trim();
                let v: Vec<_> = end.split('/').collect();
                if v.len() != 2 {
                    panic!("Wrong value for mines flagged in file \"{}\"", file_name);
                }
                self.field.flagged_mines = v[0].parse::<u16>()
                    .expect(format!("Mines flagged has wrong type in file \"{}\"", file_name).as_str());
                self.field.mines = v[1].parse::<u16>()
                    .expect(format!("Mines flagged has wrong type in file \"{}\"", file_name).as_str());
                mines_flagged = true;
            } else if line.starts_with(&info.array[4].0) {
                let (_, mut end) = line.split_at(info.array[4].0.len());
                end = end.trim();
                let v: Vec<_> = end.split(' ').collect();
                if v.len() != 4 {
                    panic!("Wrong value for dimensions in file \"{}\"", file_name);
                }
                self.field.dim.x = v[0].parse::<i16>()
                    .expect(format!("Dimension has wrong type in file \"{}\"", file_name).as_str());
                if self.field.dim.x < 1 {panic!("Dimension values must be greater than 0");}
                self.field.dim.y = v[1].parse::<i16>()
                    .expect(format!("Dimension has wrong type in file \"{}\"", file_name).as_str());
                if self.field.dim.y < 1 {panic!("Dimension values must be greater than 0");}
                self.field.dim.z = v[2].parse::<i16>()
                    .expect(format!("Dimension has wrong type in file \"{}\"", file_name).as_str());
                if self.field.dim.z < 1 {panic!("Dimension values must be greater than 0");}
                self.field.dim.w = v[3].parse::<i16>()
                    .expect(format!("Dimension has wrong type in file \"{}\"", file_name).as_str());
                if self.field.dim.w < 1 {panic!("Dimension values must be greater than 0");}
                self.field.area = self.field.dim.calc_area();
                dimensions = true;
            } else if line.starts_with(&info.array[5].0) {
                let (_, mut end) = line.split_at(info.array[5].0.len());
                end = end.trim();
                let v: Vec<_> = end.split(' ').collect();
                if v.len() != 4 {
                    panic!("Wrong value for dimensions in file \"{}\"", file_name);
                }
                self.field.loc.x = v[0].parse::<i16>()
                    .expect(format!("Location has wrong type in file \"{}\"", file_name).as_str());
                self.field.loc.y = v[1].parse::<i16>()
                    .expect(format!("Location has wrong type in file \"{}\"", file_name).as_str());
                self.field.loc.z = v[2].parse::<i16>()
                    .expect(format!("Location has wrong type in file \"{}\"", file_name).as_str());
                self.field.loc.w = v[3].parse::<i16>()
                    .expect(format!("Location has wrong type in file \"{}\"", file_name).as_str());
            } else if line.starts_with(&info.array[6].0) {
                let (_, mut end) = line.split_at(info.array[6].0.len());
                end = end.trim();
                let mut date_str = end.to_owned();
                date_str.push_str(" +0000");
                let offset_in_sec = Local::now()
                    .offset()
                    .local_minus_utc();
                match DateTime::parse_from_str(&date_str, "%Y-%m-%d %H:%M:%S %z") {
                    Ok(date) => self.field.started = <DateTime<FixedOffset> as Into<DateTime<Local>>>::into(date) - Duration::from_secs(offset_in_sec.try_into().unwrap()),
                    _ => panic!("Started has wrong format in file \"{}\"", file_name)
                } // couldn't find a better way to make it respect local time, sooo yeah
            } else if line.starts_with(&info.array[7].0) {
                let (_, mut end) = line.split_at(info.array[7].0.len());
                end = end.trim();
                match Duration::try_from_secs_f32(end.parse::<f32>()
                    .expect(format!("Duration has wrong type in file \"{}\"", file_name).as_str())) {
                    Ok(dur) => self.field.duration = dur,
                    Err(e) => panic!("{}", e),
                } // i like the error message here tho :3
            } else if line.starts_with(&info.array[8].0) {
                let (_, mut end) = line.split_at(info.array[8].0.len());
                end = end.trim();
                self.field.seed = end.parse::<u64>()
                    .expect(format!("Seed has wrong type in file \"{}\"", file_name).as_str());
            } else if line.starts_with(&info.array[9].0) {
                let (_, mut end) = line.split_at(info.array[9].0.len());
                end = end.trim();
                match MinesweeperFieldState::from_str(end) {
                    Some(state) => self.field.state = state,
                    None => panic!("Invalid game state in file \"{}\"", file_name),
                }
            } else if line.starts_with("Grid:") {
                found_grid = true;
                break;
            }
        }
        if !(cells_uncovered && mines_flagged && dimensions) {panic!("Missing necessary values in info section in file \"{}\"\nNecessary values are: \"Cells uncovered\", \"Mines flagged\", \"Dimensions\"", file_name);}
        if self.field.loc.x < 0 || self.field.loc.x >= self.field.dim.x {panic!("Location values must be greater than 0 and smaller than dimensions");}
        if self.field.loc.y < 0 || self.field.loc.x >= self.field.dim.y {panic!("Location values must be greater than 0 and smaller than dimensions");}
        if self.field.loc.z < 0 || self.field.loc.x >= self.field.dim.z {panic!("Location values must be greater than 0 and smaller than dimensions");}
        if self.field.loc.w < 0 || self.field.loc.x >= self.field.dim.w {panic!("Location values must be greater than 0 and smaller than dimensions");}
        if !found_grid {panic!("No grid in file \"{}\"", file_name)}
        self.field.field = vec![MinesweeperCell::new(); self.field.area];
        let mut loc = Point::new();
        for line in lines {
            if line.starts_with('-') {
                if loc.y != self.field.dim.y {panic!("Inconsistent y dimension in file \"{}\"", file_name);}
                loc.y = 0;
                loc.w += 1;
            } else {
                let z_split: Vec<_> = line.split(" | ").collect();
                if z_split.len() != <i16 as TryInto<usize>>::try_into(self.field.dim.z).unwrap() {panic!("Inconsistent z dimension in file \"{}\"", file_name);}
                for x in z_split {
                    let mut cells: Vec<_> = x.split(")(").collect();
                    if cells.len() != <i16 as TryInto<usize>>::try_into(self.field.dim.x).unwrap() {panic!("Inconsistent x dimension in file \"{}\"", file_name);}
                    let tmp1 = cells[0].replace("(", "");
                    cells[0] = &tmp1;
                    let tmp2 = cells[(self.field.dim.x-1) as usize].replace(")", "");
                    cells[(self.field.dim.x-1) as  usize] = &tmp2;
                    for cell_str in cells {
                        let cell_values: Vec<_> = cell_str.split(" ").collect();
                        if cell_values.len() != 3 {panic!("Cells have wrong number of values in file \"{}\"", file_name)}
                        let cell = self.field.cell_at(loc).unwrap();
                        let mask = cell_values[0].parse::<u16>()
                            .expect(format!("Mask is wrong in cell in file \"{}\"", file_name).as_str());
                        cell.is_flagged = (mask & 1) > 0;
                        cell.is_covered = (mask & 2) > 0;
                        cell.is_bomb = (mask & 4) > 0;
                        cell.print_zero = (mask & 8) > 0;
                        cell.abs = cell_values[1].parse::<u8>()
                            .expect(format!("Abs is wrong in cell in file \"{}\"", file_name).as_str());
                        cell.rel = cell_values[2].parse::<i8>()
                            .expect(format!("Rel is wrong in cell in file \"{}\"", file_name).as_str());
                        cell.coord = loc;
                        loc.x += 1;
                    }
                    loc.x = 0;
                    loc.z += 1;
                }
                loc.z = 0;
                loc.y += 1;
            }
        }
        if loc.w+1 != self.field.dim.w {panic!("Inconsistent w dimension in file \"{}\"", file_name);}
        self.state = MinesweeperGameState::Running;
        self.show_info = show_info;
        self.set_info((Alignment::Left, Alignment::Left));
        self.controls = MinesweeperGame::get_controls((Alignment::Left, Alignment::Left));
        self.settings = self.get_settings((Alignment::Left, Alignment::Left), (Alignment::Left, Alignment::Left));
        self.field.set_active_cell(true);
    }
}

fn alignment_from_str(ali: &str) -> Option<Alignment> { //change to result
    match ali.to_lowercase().as_str() {
        "left" => Some(Alignment::Left),
        "right" => Some(Alignment::Right),
        "center" => Some(Alignment::Center),
        _ => None,
    }
}

fn config_style_alignment(tab: Map<String, Value>) -> (Alignment, Alignment) {
    (
        alignment_from_str(
            &tab.get("key")
                .expect("Alignment in style must have \"key\" and \"value\" fields").clone().into_string()
                .expect("Alignment in style must be as string of: left, right or center"))
            .expect("Alignment in style must be as string of: left, right or center"),
        alignment_from_str(
            &tab.get("value")
                .expect("Alignment in style must have \"key\" and \"value\" fields").clone().into_string()
                .expect("Alignment in style must be as string of: left, right or center"))
            .expect("Alignment in style must be as string of: left, right or center")
    )
}

fn color_from_str(c: &str) -> Color { //change to result
    if c.starts_with("#") {
        let without_prefix = c.trim_start_matches("#");
        let rgb = i64::from_str_radix(without_prefix, 16).expect("Colors must be either in hexadecimal RGB format (i.e.: \"#ff2aff\") or a name");
        return Color::Rgb(
                ((rgb >> 16) & 0xff).try_into().unwrap(),
                ((rgb >> 8) & 0xff).try_into().unwrap(),
                (rgb & 0xff).try_into().unwrap());
    } else {
        return Color::from_str(c).unwrap();
    }
}

fn config_style_game_color(tab: Map<String, Value>) -> MinesweeperCellColors {
    let mut colors = MinesweeperCellColors::default();
    match tab.get("cursor") {
        Some(c) => colors.cursor = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
        None => {}
    }
    match tab.get("wrong") {
        Some(c) => colors.wrong = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
        None => {}
    }
    match tab.get("text") {
        Some(c) => colors.text = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
        None => {}
    }
    match tab.get("light") {
        Some(t) => {
            t.clone().into_table().and_then(|light| {
                match light.get("covered") {
                    Some(t) => {
                        t.clone().into_table().and_then(|covered| {
                            match covered.get("default") {
                                Some(c) => colors.light_covered_default = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            match covered.get("neighbour") {
                                Some(c) => colors.light_covered_neighbour = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            Ok(())
                        });
                    },
                    None => {}
                }
                match light.get("uncovered") {
                    Some(t) => {
                        t.clone().into_table().and_then(|uncovered| {
                            match uncovered.get("default") {
                                Some(c) => colors.light_uncovered_default = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            match uncovered.get("neighbour") {
                                Some(c) => colors.light_uncovered_neighbour = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            Ok(())
                        });
                    },
                    None => {}
                }
                Ok(())
            });
        },
        None => {}
    }
    match tab.get("dark") {
        Some(t) => {
            t.clone().into_table().and_then(|dark| {
                match dark.get("covered") {
                    Some(t) => {
                        t.clone().into_table().and_then(|covered| {
                            match covered.get("default") {
                                Some(c) => colors.dark_covered_default = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            match covered.get("neighbour") {
                                Some(c) => colors.dark_covered_neighbour = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            Ok(())
                        });
                    },
                    None => {}
                }
                match dark.get("uncovered") {
                    Some(t) => {
                        t.clone().into_table().and_then(|uncovered| {
                            match uncovered.get("default") {
                                Some(c) => colors.dark_uncovered_default = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            match uncovered.get("neighbour") {
                                Some(c) => colors.dark_uncovered_neighbour = color_from_str(&c.clone().into_string().expect("Colors must be strings")),
                                None => {}
                            }
                            Ok(())
                        });
                    },
                    None => {}
                }
                Ok(())
            });
        },
        None => {}
    }
    return colors;
}

fn keycode_from_string(s: String) -> (KeyModifiers, KeyCode) { //change to result
    let mut press: Vec<_> = s.split("-").collect();
    let key = press.pop().unwrap();
    let mut modifier =
        if press.len() == 0 {
            KeyModifiers::NONE
        } else {
            let mut modifiers = KeyModifiers::NONE;
            for modifier in press {
                let mut tmp = modifier.to_uppercase();
                if tmp == "CTRL" {
                    tmp = String::from("CONTROL");
                }
                match KeyModifiers::from_name(&tmp) {
                    Some(m) => modifiers |= m,
                    None => panic!("Unrecognized modifier: {}", modifier),
                }
            }
            modifiers
        };
    let key =
        if key.len() == 1 {
            let mut c = key.chars().nth(0).unwrap();
            if c.is_uppercase() {
                modifier |= KeyModifiers::SHIFT;
            } else if modifier & KeyModifiers::SHIFT != KeyModifiers::NONE {
                c = c.to_uppercase().collect::<Vec<_>>()[0];
                // is necessary in case someone writes e.g.: "shift-s" in the config
            }
            KeyCode::Char(c)
        } else {
            match key.to_lowercase().as_str() {
                "backspace" => KeyCode::Backspace,
                "enter" => KeyCode::Enter,
                "left" | "leftarrow" => KeyCode::Left,
                "right" | "rightarrow" => KeyCode::Right,
                "up" | "uparrow" => KeyCode::Up,
                "down" | "downarrow" => KeyCode::Down,
                "end" => KeyCode::End,
                "pageup" => KeyCode::PageUp,
                "pagedown" => KeyCode::PageDown,
                "tab" => KeyCode::Tab,
                "backtab" => KeyCode::BackTab,
                "delete" => KeyCode::Delete,
                "insert" => KeyCode::Insert,
                "esc" | "escape" => KeyCode::Esc,
                _ => panic!("Unrecognized key: {}", key),
            }
        };
    (modifier, key)
}

fn add_keycode<T: Clone>(v: T, k: Value, map: &mut HashMap<(KeyModifiers, KeyCode), T>) {
    match k.kind {
        ValueKind::String(s) => {map.insert(keycode_from_string(s), v);},
        ValueKind::Array(a) => {
            for x in a {
                add_keycode(v.clone(), x, map);
            }
        },
        _ => panic!("Wrong type: {}", k.kind),
    }
}

fn config_keymap_movement(tab: Map<String, Value>) -> HashMap<(KeyModifiers, KeyCode), (Movement, Dimension, bool)> {
    let mut map: HashMap<(KeyModifiers, KeyCode), (Movement, Dimension, bool)> = HashMap::new();
    tab.get("left").unwrap().clone().into_table().and_then(|left| {
        add_keycode((Movement::Left, Dimension::X, false), left.get("x").unwrap().clone(), &mut map);
        add_keycode((Movement::Left, Dimension::Z, false), left.get("z").unwrap().clone(), &mut map);
        left.get("start").unwrap().clone().into_table().and_then(|start| {
            add_keycode((Movement::Left, Dimension::X, true), start.get("x").unwrap().clone(), &mut map);
            add_keycode((Movement::Left, Dimension::Z, true), start.get("z").unwrap().clone(), &mut map);
            Ok(())
        })
    }).unwrap();
    tab.get("right").unwrap().clone().into_table().and_then(|right| {
        add_keycode((Movement::Right, Dimension::X, false), right.get("x").unwrap().clone(), &mut map);
        add_keycode((Movement::Right, Dimension::Z, false), right.get("z").unwrap().clone(), &mut map);
        right.get("end").unwrap().clone().into_table().and_then(|end| {
            add_keycode((Movement::Right, Dimension::X, true), end.get("x").unwrap().clone(), &mut map);
            add_keycode((Movement::Right, Dimension::Z, true), end.get("z").unwrap().clone(), &mut map);
            Ok(())
        })
    }).unwrap();
    tab.get("up").unwrap().clone().into_table().and_then(|left| {
        add_keycode((Movement::Up, Dimension::Y, false), left.get("y").unwrap().clone(), &mut map);
        add_keycode((Movement::Up, Dimension::W, false), left.get("w").unwrap().clone(), &mut map);
        left.get("top").unwrap().clone().into_table().and_then(|start| {
            add_keycode((Movement::Up, Dimension::Y, true), start.get("y").unwrap().clone(), &mut map);
            add_keycode((Movement::Up, Dimension::W, true), start.get("w").unwrap().clone(), &mut map);
            Ok(())
        })
    }).unwrap();
    tab.get("down").unwrap().clone().into_table().and_then(|right| {
        add_keycode((Movement::Down, Dimension::Y, false), right.get("y").unwrap().clone(), &mut map);
        add_keycode((Movement::Down, Dimension::W, false), right.get("w").unwrap().clone(), &mut map);
        right.get("bottom").unwrap().clone().into_table().and_then(|end| {
            add_keycode((Movement::Down, Dimension::Y, true), end.get("y").unwrap().clone(), &mut map);
            add_keycode((Movement::Down, Dimension::W, true), end.get("w").unwrap().clone(), &mut map);
            Ok(())
        })
    }).unwrap();
    return map;
}

fn config_keymap_game(tab: Map<String, Value>) -> HashMap<(KeyModifiers, KeyCode), GameFunction> {
    let mut map: HashMap<(KeyModifiers, KeyCode), GameFunction> = HashMap::new();
    add_keycode(GameFunction::New, tab.get("new").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Retry, tab.get("retry").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Free, tab.get("free").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Uncover, tab.get("uncover").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Capitulate, tab.get("capitulate").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Flag, tab.get("flag").unwrap().clone(), &mut map);
    add_keycode(GameFunction::FlagChording, tab.get("flag_chording").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Pause, tab.get("pause").unwrap().clone(), &mut map);
    add_keycode(GameFunction::Save, tab.get("save").unwrap().clone(), &mut map);
    tab.get("toggle").unwrap().clone().into_table().and_then(|toggle| {
        add_keycode(GameFunction::ToggleInfo, toggle.get("info").unwrap().clone(), &mut map);
        add_keycode(GameFunction::ToggleDelta, toggle.get("delta").unwrap().clone(), &mut map);
        add_keycode(GameFunction::ToggleSweep, toggle.get("sweep").unwrap().clone(), &mut map);
        Ok(())
    }).unwrap();
    tab.get("mark").unwrap().clone().into_table().and_then(|mark| {
        add_keycode(GameFunction::MarkSet, mark.get("set").unwrap().clone(), &mut map);
        add_keycode(GameFunction::MarkUncover, mark.get("uncover").unwrap().clone(), &mut map);
        add_keycode(GameFunction::MarkFlag, mark.get("flag").unwrap().clone(), &mut map);
        add_keycode(GameFunction::MarkClear, mark.get("clear").unwrap().clone(), &mut map);
        Ok(())
    }).unwrap();
    return map;
}

fn config_keymap_global(tab: Map<String, Value>) -> HashMap<(KeyModifiers, KeyCode), GlobalFunction> {
    let mut map: HashMap<(KeyModifiers, KeyCode), GlobalFunction> = HashMap::new();
    add_keycode(GlobalFunction::QuitAll, tab.get("quit_all").unwrap().clone(), &mut map);
    add_keycode(GlobalFunction::Quit, tab.get("quit").unwrap().clone(), &mut map);
    add_keycode(GlobalFunction::Controls, tab.get("controls").unwrap().clone(), &mut map);
    add_keycode(GlobalFunction::Settings, tab.get("settings").unwrap().clone(), &mut map);
    return map;
}

fn main() -> color_eyre::Result<()> {
    let settings = Config::builder()
        // Add in `./Settings.toml`
        .add_source(config::File::with_name("./config.toml"))
        // Add in settings from the environment (with a prefix of APP)
        // Eg.. `APP_DEBUG=1 ./target/app` would set the `debug` key
        .add_source(config::Environment::with_prefix("APP"))
        .build()
        .unwrap();
    let mut dim = Point {x: 4, y: 4, z: 4, w: 4};
    let mut mines: u16 = 20;
    let mut show_info = true;
    let mut delta_mode = true;
    let mut sweep_mode = false;
    let mut rand_seed = true;
    let mut set_seed = false;
    let mut seed = 0;
    let mut capture_mouse = false;
    let mut load_file = false;
    let mut dir = UserDirs::new().unwrap().download_dir().unwrap().to_path_buf();
    let mut file_name = String::new();
    let mut args = env::args();
    let Some(program) = args.next() else {panic!("WTF?")};
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "-?" | "--help" => {
                let controls = MinesweeperGame::get_controls((Alignment::Left, Alignment::Left));
                println!("{}", controls.title);
                print!("{}", controls.as_str(2));
                println!("Commandline arguments");
                println!("  -h, -?, --help            Show this menu");
                println!("  -d, --dim, --dimension    Change field dimensions. An array of unsigned integers e.g.: -d 4 4 4 4");
                println!("  -m, --mines               Change amount of mines. An unsigned integer");
                println!("  -i, --show_info           Toggle info box. A boolean value t/f or true/false or y/n or yes/no or on/off (any capitalisation)");
                println!("  -u, --delta_mode          Toggle delta mode. A boolean value t/f or true/false or y/n or yes/no or on/off (any capitalisation)");
                println!("  -U, --sweep_mode          Toggle sweep mode. A boolean value t/f or true/false or y/n or yes/no or on/off (any capitalisation)");
                println!("  -s, --seed                Set seed. An unsigned integer");
                println!("  -r, --random              Toggle random seed. A boolean value t/f or true/false or y/n or yes/no or on/off (any capitalisation)");
                println!("  -c, --capture_mouse       Wether to allow mouse interaction. A boolean value t/f or true/false or y/n or yes/no or on/off (any capitalisation)");
                println!("  -o, --dir                 Where to output save files. Default is \"{}\"", dir.to_str().unwrap());
                println!("Default settings as a command");
                println!("  {program} -d 4 4 4 4 -m 20 -i t -u t -U f -r t -c f");
                println!("Classic Minesweeper as a command... Weirdo...");
                println!("  {program} -d 16 16 1 1 -m 40 -i t -u f -U f -r t -c t");
                return Ok(())
            },
            "-d" | "--dim" | "--dimension" => {
                let x = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.x = x.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", x, arg).as_str());
                if dim.x < 1 {panic!("Dimension values must be greater than 0");}
                let y = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.y = y.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", y, arg).as_str());
                if dim.y < 1 {panic!("Dimension values must be greater than 0");}
                let z = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.z = z.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", z, arg).as_str());
                if dim.z < 1 {panic!("Dimension values must be greater than 0");}
                let w = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                dim.w = w.parse::<i16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", w, arg).as_str());
                if dim.w < 1 {panic!("Dimension values must be greater than 0");}
            },
            "-m" | "--mines" => {
                let value = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                mines = value.parse::<u16>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", value, arg).as_str());
                if mines == 0 {panic!("Amount of mines must be greater than 0");}
            },
            "-i" | "--show_info" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" | "on" => show_info = true,
                    "f" | "n" | "false" | "no" | "off" => show_info = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-u" | "--delta_mode" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" | "on" => delta_mode = true,
                    "f" | "n" | "false" | "no" | "off" => delta_mode = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-U" | "--sweep_mode" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" | "on" => sweep_mode = true,
                    "f" | "n" | "false" | "no" | "off" => sweep_mode = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-r" | "--random" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" | "on" => rand_seed = true,
                    "f" | "n" | "false" | "no" | "off" => rand_seed = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-s" | "--seed" => {
                set_seed = true;
                let value = args.next()
                    .expect(format!("You must provide an unsigned integer for argument \"{}\"", arg).as_str());
                seed = value.parse::<u64>()
                    .expect(format!("Value \"{}\" has wrong type for argument \"{}\"", value, arg).as_str());
            },
            "-c" | "--capture_mouse" => {
                let v = args.next()
                    .expect(format!("You must provide a boolean for argument \"{}\"", arg).as_str());
                match v.to_lowercase().as_str() {
                    "t" | "y" | "true" | "yes" | "on" => capture_mouse = true,
                    "f" | "n" | "false" | "no" | "off" => capture_mouse = false,
                    &_ => panic!("Value \"{v}\" has wrong type for argument \"{arg}\""),
                }
            },
            "-o" | "--dir" => {
                dir = PathBuf::from(&args.next()
                    .expect(format!("You must provide a directory for argument \"{}\"", arg).as_str()));
                if !dir.is_dir() {panic!("\"{}\" is not a directory", dir.to_str().unwrap());}
            },
            &_ => {
                load_file = true;
                file_name = arg;
            }
        }
    }
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = App::new(
        config_keymap_global(settings.get_table("keymap").unwrap()),
        config_keymap_movement(settings.get_table("keymap.movement").unwrap()),
        config_keymap_game(settings.get_table("keymap.game").unwrap()),
        ).run(terminal, dim, mines, show_info, delta_mode, sweep_mode, rand_seed, set_seed, seed, capture_mouse, load_file, file_name, dir,
            config_style_alignment(settings.get_table("style.info.alignment").unwrap()),
            config_style_alignment(settings.get_table("style.controls.alignment").unwrap()),
            config_style_alignment(settings.get_table("style.settings.settings.alignment").unwrap()),
            config_style_alignment(settings.get_table("style.settings.controls.alignment").unwrap()),
            config_style_game_color(settings.get_table("style.game.color").unwrap())
        );
    ratatui::restore();
    result
}

/// The main application which holds the state and logic of the application.
#[derive(Debug, Default)]
pub struct App {
    /// Is the application running?
    running: bool,
    game: MinesweeperGame,
    area: Rect,
    capture_mouse: bool,
    dir: PathBuf,
    global_keymap: HashMap<(KeyModifiers, KeyCode), GlobalFunction>,
    movement_keymap: HashMap<(KeyModifiers, KeyCode), (Movement, Dimension, bool)>,
    game_keymap: HashMap<(KeyModifiers, KeyCode), GameFunction>,
}

impl App {
    /// Construct a new instance of [`App`].
    pub fn new(
        global_keymap: HashMap<(KeyModifiers, KeyCode), GlobalFunction>,
        movement_keymap: HashMap<(KeyModifiers, KeyCode), (Movement, Dimension, bool)>,
        game_keymap: HashMap<(KeyModifiers, KeyCode), GameFunction>,
        ) -> Self {
        let mut app = Self::default();
        app.global_keymap = global_keymap;
        app.movement_keymap = movement_keymap;
        app.game_keymap = game_keymap;
        app
    }

    /// Run the application's main loop.
    pub fn run(
        mut self, mut terminal: DefaultTerminal,
        dim: Point, mines: u16,
        show_info: bool, delta_mode: bool, sweep_mode: bool, rand_seed: bool, set_seed: bool,
        seed: u64,
        capture_mouse: bool, load_file: bool,
        file_name: String, dir: PathBuf,
        alignment_info: (Alignment, Alignment),
        alignment_controls: (Alignment, Alignment),
        alignment_settings_settings: (Alignment, Alignment),
        alignment_settings_controls: (Alignment, Alignment),
        colors: MinesweeperCellColors
        ) -> Result<()> {
        self.running = true;
        self.capture_mouse = capture_mouse;
        self.dir = dir.clone();
        if load_file {
            self.game.load_file(file_name, show_info);
        } else {
            self.game.init(dim, mines, show_info, delta_mode, sweep_mode, if set_seed {seed} else {rand::random::<u64>()});
        }
        self.game.settings.0.array[9].1.value = if rand_seed {1} else {0};
        self.game.info.alignment = alignment_info;
        self.game.controls.alignment = alignment_controls;
        self.game.settings.0.alignment = alignment_settings_settings;
        self.game.settings.1.alignment = alignment_settings_controls;
        self.game.field.colors = colors;
        if capture_mouse {
            execute!(
                std::io::stdout(),
                EnableBracketedPaste,
                EnableFocusChange,
                EnableMouseCapture
            )?;
        }
        while self.running {
            terminal.draw(|frame| self.render(frame))?;
            let start = Instant::now();
            self.handle_crossterm_events()?;
            if matches!(self.game.field.state, MinesweeperFieldState::Running) {
                self.game.field.duration += start.elapsed();
                self.game.update_info_time_elapsed();
            }
        }
        if capture_mouse {
            execute!(
                std::io::stdout(),
                DisableBracketedPaste,
                DisableFocusChange,
                DisableMouseCapture
            )?;
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
        self.area = area;
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
                Event::Mouse(mouse) if self.capture_mouse => self.on_mouse_event(mouse),
                Event::Resize(width, height) => self.check_size(width, height, MinesweeperGameState::Running),
                _ => {}
            }
        }
        Ok(())
    }

    fn on_mouse_event(&mut self, mouse: MouseEvent) {
        let (field_area, _) = self.game.game_area(self.area);
        match self.game.state {
            MinesweeperGameState::Running => {
                if mouse.column >= field_area.x && mouse.column < field_area.x+field_area.width-1 && mouse.row >= field_area.y && mouse.row < field_area.y+field_area.height {
                    let (x, y) = (mouse.column-field_area.x, mouse.row-field_area.y);
                    let loc = Point {
                        x: (x as i16/2)%(self.game.field.dim.x+1),
                        y: y as i16%(self.game.field.dim.y+1),
                        z: (x as i16/2)/(self.game.field.dim.x+1),
                        w: y as i16/(self.game.field.dim.y+1),
                    };
                    if loc.x < self.game.field.dim.x && loc.y < self.game.field.dim.y && loc.z < self.game.field.dim.z && loc.w < self.game.field.dim.w {
                        match self.game.field.state {
                            MinesweeperFieldState::New => {
                                match mouse.kind {
                                    MouseEventKind::Moved => {
                                        self.game.field.set_active_cell(false);
                                        self.game.field.loc = loc;
                                        self.game.field.set_active_cell(true);
                                    },
                                    MouseEventKind::Down(button) => {
                                        match button {
                                            MouseButton::Left => {
                                                self.game.field.state = MinesweeperFieldState::Running;
                                                self.game.field.uncover_cell(self.game.field.loc);
                                                self.game.field.started = Local::now();
                                                self.game.update_info_cells_uncovered();
                                                self.game.update_info_started();
                                            },
                                            _ => {},
                                        }
                                    },
                                    _ => {},
                                }
                            },
                            MinesweeperFieldState::Running | MinesweeperFieldState::RevealField => {
                                match mouse.kind {
                                    MouseEventKind::Moved => {
                                        self.game.field.set_active_cell(false);
                                        self.game.field.loc = loc;
                                        self.game.field.set_active_cell(true);
                                    },
                                    MouseEventKind::Down(button) => {
                                        match button {
                                            MouseButton::Left => {
                                                self.game.field.uncover_cell(self.game.field.loc);
                                                self.game.update_info_cells_uncovered();
                                            },
                                            MouseButton::Right => {
                                                self.game.field.toggle_flagged(self.game.field.loc);
                                                self.game.update_info_mines_flagged();
                                                if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                            }
                                            _ => {},
                                        }
                                    },
                                    _ => {},
                                }
                            },
                            _ => {},
                        }
                    }
                }
            },
            MinesweeperGameState::Controls => {
                match mouse.kind {
                    MouseEventKind::ScrollUp => self.game.controls.dec_pos(),
                    MouseEventKind::ScrollDown => self.game.controls.inc_pos(),
                    _ => {}
                }
            },
            MinesweeperGameState::Settings => {
                match mouse.kind {
                    MouseEventKind::ScrollUp => self.game.settings.0.inc_pos_wrap(),
                    MouseEventKind::ScrollDown => self.game.settings.0.dec_pos_wrap(),
                    MouseEventKind::ScrollRight => self.game.settings.0.get_tuple().1.inc(),
                    MouseEventKind::ScrollLeft => self.game.settings.0.get_tuple().1.dec(),
                    _ => {}
                }
            },
            _ => {}
        }
    }

    /// Handles the key events and updates the state of [`App`].
    fn on_key_event(&mut self, key: KeyEvent) {
        if let Some(global_function) = self.global_keymap.get(&(key.modifiers, key.code)) {
            match global_function {
                GlobalFunction::QuitAll => self.quit(),
                GlobalFunction::Quit => {
                    if matches!(self.game.state, MinesweeperGameState::Running) {
                        if matches!(self.game.field.state, MinesweeperFieldState::New) || matches!(self.game.field.state, MinesweeperFieldState::Running) || matches!(self.game.field.state, MinesweeperFieldState::RevealField) {
                            self.quit();
                        } else {
                            self.game.field.state = MinesweeperFieldState::RevealField;
                        }
                    } else {
                        self.game.state = MinesweeperGameState::Running;
                    }
                },
                GlobalFunction::Controls => {
                    if matches!(self.game.state, MinesweeperGameState::Controls) {
                        self.game.state = MinesweeperGameState::Running;
                    } else {
                        self.game.field.state = MinesweeperFieldState::Paused;
                        self.game.state = MinesweeperGameState::Controls;
                    }
                },
                GlobalFunction::Settings => {
                    if matches!(self.game.state, MinesweeperGameState::Settings) {
                        self.game.apply_settings();
                        self.game.regenerate_field();
                        self.game.state = MinesweeperGameState::Running;
                    } else {
                        self.game.field.state = MinesweeperFieldState::Paused;
                        self.game.state = MinesweeperGameState::Settings;
                    }
                },
            }
        } else {
            match self.game.state {
                MinesweeperGameState::Running => {
                    match self.game.field.state {
                        MinesweeperFieldState::New => {
                            if let Some(movement) = self.movement_keymap.get(&(key.modifiers, key.code)) {
                                match movement {
                                    &(Movement::Left,   Dimension::X, false)    => self.game.move_in_field(|f| f.move_left_x()),
                                    &(Movement::Left,   Dimension::X, true)     => self.game.move_in_field(|f| f.move_left_end_x()),
                                    &(Movement::Right,  Dimension::X, false)    => self.game.move_in_field(|f| f.move_right_x()),
                                    &(Movement::Right,  Dimension::X, true)     => self.game.move_in_field(|f| f.move_right_end_x()),
                                    &(Movement::Up,     Dimension::Y, false)    => self.game.move_in_field(|f| f.move_up_y()),
                                    &(Movement::Up,     Dimension::Y, true)     => self.game.move_in_field(|f| f.move_up_end_y()),
                                    &(Movement::Down,   Dimension::Y, false)    => self.game.move_in_field(|f| f.move_down_y()),
                                    &(Movement::Down,   Dimension::Y, true)     => self.game.move_in_field(|f| f.move_down_end_y()),
                                    &(Movement::Left,   Dimension::Z, false)    => self.game.move_in_field(|f| f.move_left_z()),
                                    &(Movement::Left,   Dimension::Z, true)     => self.game.move_in_field(|f| f.move_left_end_z()),
                                    &(Movement::Right,  Dimension::Z, false)    => self.game.move_in_field(|f| f.move_right_z()),
                                    &(Movement::Right,  Dimension::Z, true)     => self.game.move_in_field(|f| f.move_right_end_z()),
                                    &(Movement::Up,     Dimension::W, false)    => self.game.move_in_field(|f| f.move_up_w()),
                                    &(Movement::Up,     Dimension::W, true)     => self.game.move_in_field(|f| f.move_up_end_w()),
                                    &(Movement::Down,   Dimension::W, false)    => self.game.move_in_field(|f| f.move_down_w()),
                                    &(Movement::Down,   Dimension::W, true)     => self.game.move_in_field(|f| f.move_down_end_w()),
                                    _ => {},
                                }
                            } else if let Some(game_function) = self.game_keymap.get(&(key.modifiers, key.code)) {
                                match game_function {
                                    GameFunction::Free => {
                                        self.game.field.state = MinesweeperFieldState::Running;
                                        self.game.field.find_free_cell();
                                        self.game.field.started = Local::now();
                                        self.game.update_info_cells_uncovered();
                                        self.game.update_info_started();
                                    },
                                    GameFunction::Uncover => {
                                        self.game.field.state = MinesweeperFieldState::Running;
                                        self.game.field.uncover_cell(self.game.field.loc);
                                        self.game.field.started = Local::now();
                                        self.game.update_info_cells_uncovered();
                                        self.game.update_info_started();
                                    },
                                    GameFunction::ToggleInfo => self.game.show_info = !self.game.show_info,
                                    GameFunction::ToggleDelta => self.game.toggle_delta_mode(),
                                    GameFunction::ToggleSweep => {
                                        self.game.toggle_sweep_mode();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    _ => {}
                                }
                            }
                        },
                        MinesweeperFieldState::Running | MinesweeperFieldState::RevealField => {
                            if let Some(movement) = self.movement_keymap.get(&(key.modifiers, key.code)) {
                                match movement {
                                    &(Movement::Left,   Dimension::X, false)    => self.game.move_in_field(|f| f.move_left_x()),
                                    &(Movement::Left,   Dimension::X, true)     => self.game.move_in_field(|f| f.move_left_end_x()),
                                    &(Movement::Right,  Dimension::X, false)    => self.game.move_in_field(|f| f.move_right_x()),
                                    &(Movement::Right,  Dimension::X, true)     => self.game.move_in_field(|f| f.move_right_end_x()),
                                    &(Movement::Up,     Dimension::Y, false)    => self.game.move_in_field(|f| f.move_up_y()),
                                    &(Movement::Up,     Dimension::Y, true)     => self.game.move_in_field(|f| f.move_up_end_y()),
                                    &(Movement::Down,   Dimension::Y, false)    => self.game.move_in_field(|f| f.move_down_y()),
                                    &(Movement::Down,   Dimension::Y, true)     => self.game.move_in_field(|f| f.move_down_end_y()),
                                    &(Movement::Left,   Dimension::Z, false)    => self.game.move_in_field(|f| f.move_left_z()),
                                    &(Movement::Left,   Dimension::Z, true)     => self.game.move_in_field(|f| f.move_left_end_z()),
                                    &(Movement::Right,  Dimension::Z, false)    => self.game.move_in_field(|f| f.move_right_z()),
                                    &(Movement::Right,  Dimension::Z, true)     => self.game.move_in_field(|f| f.move_right_end_z()),
                                    &(Movement::Up,     Dimension::W, false)    => self.game.move_in_field(|f| f.move_up_w()),
                                    &(Movement::Up,     Dimension::W, true)     => self.game.move_in_field(|f| f.move_up_end_w()),
                                    &(Movement::Down,   Dimension::W, false)    => self.game.move_in_field(|f| f.move_down_w()),
                                    &(Movement::Down,   Dimension::W, true)     => self.game.move_in_field(|f| f.move_down_end_w()),
                                    _ => {},
                                }
                            } else if let Some(game_function) = self.game_keymap.get(&(key.modifiers, key.code)) {
                                match game_function {
                                    GameFunction::New => self.game.regenerate_field(),
                                    GameFunction::Retry => self.game.regenerate_field_same_seed(),
                                    GameFunction::Uncover => {
                                        self.game.field.uncover_cell(self.game.field.loc);
                                        self.game.update_info_cells_uncovered();
                                    },
                                    GameFunction::Capitulate => {
                                        if !matches!(self.game.field.state, MinesweeperFieldState::RevealField) {
                                            self.game.field.state = MinesweeperFieldState::GaveUp;
                                        } else {
                                            self.game.field.do_everywhere(|f, p| f.cell_at(p).unwrap().set_covered(false));
                                            self.game.field.uncovered_cells = self.game.field.area as u16;
                                            self.game.update_info_cells_uncovered();
                                        }
                                    },
                                    GameFunction::Flag => {
                                        self.game.field.toggle_flagged(self.game.field.loc);
                                        self.game.update_info_mines_flagged();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    GameFunction::FlagChording => {
                                        self.game.field.toggle_flagged_chording(self.game.field.loc);
                                        self.game.update_info_mines_flagged();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    GameFunction::Pause => self.game.field.state = MinesweeperFieldState::Paused,
                                    GameFunction::Save => self.game.save_game(self.dir.clone()),
                                    GameFunction::ToggleInfo => self.game.show_info = !self.game.show_info,
                                    GameFunction::ToggleDelta => self.game.toggle_delta_mode(),
                                    GameFunction::ToggleSweep => {
                                        self.game.toggle_sweep_mode();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    GameFunction::MarkSet => self.game.field.add_mark(self.game.field.loc),
                                    GameFunction::MarkUncover => self.game.field.uncover_black_cell(),
                                    GameFunction::MarkFlag => self.game.field.flag_definite_marked_cell(),
                                    GameFunction::MarkClear => self.game.field.clear_marks(),
                                    _ => {}
                                }
                            }
                        },
                        MinesweeperFieldState::ClickedMine | MinesweeperFieldState::GaveUp | MinesweeperFieldState::Won => {
                            if let Some(game_function) = self.game_keymap.get(&(key.modifiers, key.code)) {
                                match game_function {
                                    GameFunction::New => self.game.regenerate_field(),
                                    GameFunction::Retry => self.game.regenerate_field_same_seed(),
                                    GameFunction::ToggleInfo => self.game.show_info = !self.game.show_info,
                                    GameFunction::ToggleDelta => self.game.toggle_delta_mode(),
                                    GameFunction::ToggleSweep => {
                                        self.game.toggle_sweep_mode();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    _ => {}
                                }
                            }
                        },
                        MinesweeperFieldState::Paused => {
                            if let Some(game_function) = self.game_keymap.get(&(key.modifiers, key.code)) {
                                match game_function {
                                    GameFunction::New => self.game.regenerate_field(),
                                    GameFunction::Retry => self.game.regenerate_field_same_seed(),
                                    GameFunction::ToggleInfo => self.game.show_info = !self.game.show_info,
                                    GameFunction::ToggleDelta => self.game.toggle_delta_mode(),
                                    GameFunction::ToggleSweep => {
                                        self.game.toggle_sweep_mode();
                                        if self.game.field.sweep_mode {self.game.update_info_cells_uncovered()};
                                    },
                                    _ => {}
                                }
                            }
                        },
                    }
                    self.game.update_info_state(); //maybe change this, so it doesn't get called too
                                                //often
                },
                MinesweeperGameState::Controls => {
                    if let Some(movement) = self.movement_keymap.get(&(key.modifiers, key.code)) {
                        match movement {
                            &(Movement::Up,     _, _) => self.game.controls.dec_pos(),
                            &(Movement::Down,   _, _) => self.game.controls.inc_pos(),
                            _ => {}
                        }
                    }
                },
                MinesweeperGameState::Settings => {
                    if let Some(movement) = self.movement_keymap.get(&(key.modifiers, key.code)) {
                        match movement {
                            &(Movement::Left,   _, _) => { //make sure to readd '+' and '-' for inc
                                                           //and dec
                                self.game.settings.0.get_tuple().1.dec();
                                self.game.settings.0.recalc_constraint_len();
                            },
                            &(Movement::Right,  _, _) => {
                                self.game.settings.0.get_tuple().1.inc();
                                self.game.settings.0.recalc_constraint_len();
                            },
                            &(Movement::Up,     _, _) => self.game.settings.0.dec_pos(),
                            &(Movement::Down,   _, _) => self.game.settings.0.inc_pos(),
                        }
                    } else {
                        match (key.modifiers, key.code) {
                            (_, KeyCode::Char('0')) => {
                                self.game.settings.0.get_tuple().1.append(0);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('1')) => {
                                self.game.settings.0.get_tuple().1.append(1);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('2')) => {
                                self.game.settings.0.get_tuple().1.append(2);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('3')) => {
                                self.game.settings.0.get_tuple().1.append(3);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('4')) => {
                                self.game.settings.0.get_tuple().1.append(4);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('5')) => {
                                self.game.settings.0.get_tuple().1.append(5);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('6')) => {
                                self.game.settings.0.get_tuple().1.append(6);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('7')) => {
                                self.game.settings.0.get_tuple().1.append(7);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('8')) => {
                                self.game.settings.0.get_tuple().1.append(8);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            (_, KeyCode::Char('9')) => {
                                self.game.settings.0.get_tuple().1.append(9);
                                self.game.settings.0.recalc_constraint_len();
                            },
                            //idk if i could've done that better...
                            (_, KeyCode::Char('y') | KeyCode::Char('Y') | KeyCode::Char('t') | KeyCode::Char('T')) => self.game.settings.0.get_tuple().1.set_bool(true),
                            (_, KeyCode::Char('n') | KeyCode::Char('N') | KeyCode::Char('f') | KeyCode::Char('F')) => self.game.settings.0.get_tuple().1.set_bool(false),
                            (_, KeyCode::Delete | KeyCode::Backspace) => self.game.settings.0.get_tuple().1.del(),
                            _ => {}
                        }
                    }
                },
                MinesweeperGameState::TooSmall => {
                    if let Some(game_function) = self.game_keymap.get(&(key.modifiers, key.code)) {
                        match game_function {
                            GameFunction::ToggleInfo => self.game.show_info = !self.game.show_info,
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    /// Set running to false to quit the application.
    fn quit(&mut self) {
        self.running = false;
    }
}
