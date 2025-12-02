# Advent

🎄 An [Advent of Code](https://adventofcode.com) runner for your Gleam project!

![An example image](https://github.com/user-attachments/assets/bd52c2cb-66b4-4737-9218-20478264359d)

## Install

To add this to your Gleam project add this to your `gleam.toml`'s dependencies:

```toml
advent = { git = "https://github.com/giacomocavalieri/advent.git", ref = "main" }
```

## Usage

You can then setup and run a year as part of your project's `main` function:

```gleam
import advent

pub fn main() -> Nil {
  advent.year(2025)
  |> advent.add_day(todo as "your solution to day 01")
  |> advent.add_day(todo as "your solution to day 02")
  |> advent.add_day(todo as "...")
  |> advent.add_padding_days(up_to: 25)
  |> advent.run
}
```

## FAQ

- **Where do I put my input files?** The runner expects the input files to be
  located in an `inputs` directory in your project's root with the following
  structure: `inputs/<YEAR>/<DAY>.txt`. So for example the input for day 2 of
  2022 should be in `inputs/2022/02.txt` (days always have two digits)!
- **How can I watch for file changes?** You can use any file watcher, I like
  using [`watchexec`](https://watchexec.github.io) like this:
  ```sh
  watchexec\
    --wrap-process=session\
    --restart\
    --clear\
    --quiet\
    --watch src\
    --watch inputs\
    -- "gleam run"
  ```

## WIP

- [ ] Timing information
- [ ] Add examples to the mix
