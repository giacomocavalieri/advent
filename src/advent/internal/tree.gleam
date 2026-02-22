import gleam/int
import gleam/list
import gleam_community/ansi
import string_width

pub fn generate(height: Int) -> String {
  [
    ansi.magenta("-ˏˋ★ˊˎ-"),
    ..int.range(from: height, to: 0, with: [], run: fn(acc, level) {
      [generate_level(level), ..acc]
    })
  ]
  |> list.append([ansi.dim("└─┘")])
  |> string_width.stack_vertical(align: string_width.Center, gap: 0, with: " ")
}

fn generate_level(level: Int) -> String {
  case level <= 1 {
    True -> ansi.green("><<")
    False ->
      ansi.green(">")
      <> generate_inner_level(level * 2 - 1, False)
      <> ansi.green("<")
  }
}

fn generate_inner_level(count: Int, previous_is_star: Bool) -> String {
  case count <= 0 {
    True -> ""
    False -> {
      let #(new_point, is_star) = generate_random_point(previous_is_star)
      new_point <> generate_inner_level(count - 1, is_star)
    }
  }
}

fn generate_random_point(previous_is_star: Bool) -> #(String, Bool) {
  case int.random(10) {
    _ if previous_is_star ->
      case int.random(2) {
        0 -> #(ansi.green(">"), False)
        _ -> #(ansi.green("<"), False)
      }

    0 | 1 | 2 | 3 | 4 | 5 ->
      case int.random(2) {
        0 -> #(ansi.green(">"), False)
        _ -> #(ansi.green("<"), False)
      }

    6 -> #(random_color("‧͙"), True)
    7 -> #(random_color("❆"), True)
    8 | _ -> #(random_color("*"), True)
  }
}

fn random_color(string: String) -> String {
  case int.random(2) {
    0 -> ansi.yellow(string)
    _ -> ansi.red(string)
  }
}
