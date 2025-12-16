# minesweeper_4d

This is a [Ratatui] app and my 8th minesweeper variant, 7th multidimensional one (assuming all the ones I abandoned where at least supposed to be 4D), 4th public and 2nd (excluding the 3 abandoned ones) 4D one.

[Ratatui]: https://ratatui.rs

![An example game](example_gameplay.png)

## How to play

You have to find all the mines in a 4 dimensional field. The pink cursor highlights the current field you're on and its value shows the number of bombs that are around it. To help you find which fields are in the area of influence of the cursor, they are highlighted in a less saturated shade of pink. Have fun finding all the mines!

### Controls

<pre>
  Quit:                 ctrl+C, q, ESC
  Controls:             c
  Settings:             o
  Move left in x:       Leftarrow, h
  Move right in x:      Rightarrow, l
  Move up in y:         Uparrow, k
  Move down in y:       Downarrow, j
  Move left in z:       a, ctrl+h
  Move right in z:      d, ctrl+l
  Move up in w:         w, ctrl+k
  Move down in w:       s, ctrl+j
  New game:             n
  Find free cell:       f
  Uncover cell:         SPACE
  Give up/reveal field: g
  Flag cell:            m, e
  Flag cell chording:   M, E
  Pause game:           p
  Toggle info:          i
  Toggle delta mode:    u
</pre>

## Compiling and running

```
cargo install minesweeper_4d
```

or

Build:

```
cargo build --release
```

Run:

```
./target/release/minesweeper_4d
```

### Commandline arguments

<pre>
  -h, -?, --help            Show this menu
  -d, --dim, --dimension    Change field dimensions. An array of unsigned integers e.g.: -d 4 4 4 4
  -m, --mines               Change amount of mines. An unsigned integer
  -i, --show_info           Toggle info box. A boolean value t/f/ or true/false or y/n or yes/no (any capitalisation)
  -u, --delta_mode          Toggle delta mode. A boolean value t/f/ or true/false or y/n or yes/no (any capitalisation)
</pre>

Default settings as a command

```
minesweeper_4d -d 4 4 4 4 -m 20 -i t -u t
```

## TODO

- [x] Make swapping to and from delta_mode possible
- [x] Fix 0 showing around newly flagged mines
- [ ] Add timer _idk, kinda too lazy..._
- [x] Fix win condition
- [ ] ~~Make random seedable~~
- [x] Add chording
- [x] Editable settings

## License

Copyright (c) itabesamesa/laura418

This project is licensed under the MIT license ([LICENSE] or <http://opensource.org/licenses/MIT>)

[LICENSE]: ./LICENSE

## Special thanks

To Julian Schlüntz for creating [4D Minesweeper](https://store.steampowered.com/app/787980/4D_Minesweeper/) on steam, the original inspiration for this project

To [Ratatui](https://github.com/walternagl-student) for sparing me from implementing a TUI library from scratch again...
