import advent
import gleam/erlang/process
import gleam/option

pub fn main() {
  advent.year(2025)
  |> advent.add_day(
    advent.Day(
      day: 1,
      parse: fn(_) { process.sleep(50) },
      part_a: fn(_) { panic as "woo" },
      expected_a: option.None,
      wrong_answers_a: [],
      part_b: fn(_) { process.sleep(1000) },
      expected_b: option.Some(Nil),
      wrong_answers_b: [],
    ),
  )
  |> advent.add_padding_days(12)
  |> advent.show_timings
  |> advent.run
}
