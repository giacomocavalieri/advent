import gleam/int
import gleam/list
import gleam_community/ansi
import string_width

pub fn generate(height: Int) -> String {
  [
    ansi.magenta("-ˏˋ★ˊˎ-"),
    ..list.range(1, height)
    |> list.map(generate_level)
  ]
  |> list.append([
    ansi.dim("└─┘"),
  ])
  |> string_width.stack_vertical(align: string_width.Center, gap: 0, with: " ")
}

fn generate_level(level: Int) -> String {
  case level <= 1 {
    True -> ansi.green("><<")
    False ->
      ansi.green(">") <> generate_inner_level(level * 2 - 1) <> ansi.green("<")
  }
}

fn generate_inner_level(count: Int) -> String {
  case count <= 0 {
    True -> ""
    False -> generate_random_point() <> generate_inner_level(count - 1)
  }
}

fn generate_random_point() -> String {
  case int.random(10) {
    0 | 1 | 2 | 3 | 4 | 5 ->
      case int.random(2) {
        0 -> ansi.green(">")
        _ -> ansi.green("<")
      }

    6 -> random_color("‧͙")
    7 -> random_color("❆")
    8 | _ -> random_color("*")
  }
}

fn random_color(string: String) -> String {
  case int.random(2) {
    0 -> ansi.yellow(string)
    _ -> ansi.red(string)
  }
}
