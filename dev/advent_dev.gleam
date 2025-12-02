import advent
import gleam/option

pub fn main() {
  advent.year(2025)
  |> advent.add_day(
    advent.Day(
      day: 1,
      parse: fn(_) { Nil },
      part_a: fn(_) { panic },
      expected_a: option.None,
      wrong_answers_a: [],
      part_b: fn(_) { panic },
      expected_b: option.None,
      wrong_answers_b: [],
    ),
  )
  |> advent.add_padding_days(12)
  |> advent.run
}
