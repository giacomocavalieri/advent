import advent
import gleam/erlang/process
import gleam/option

pub fn main() {
  advent.year(2025)
  |> advent.timed
  |> advent.add_day(
    advent.Day(
      day: 1,
      parse: fn(_) { Nil },
      part_a: fn(_) { process.sleep(12) },
      expected_a: option.None,
      wrong_answers_a: [],
      part_b: fn(_) { 1 },
      expected_b: option.Some(1),
      wrong_answers_b: [],
    ),
  )
  |> advent.add_day(
    advent.Day(
      day: 2,
      parse: fn(_) { Nil },
      part_a: fn(_) { process.sleep(100) },
      expected_a: option.None,
      wrong_answers_a: [],
      part_b: fn(_) { 1 },
      expected_b: option.Some(1),
      wrong_answers_b: [],
    ),
  )
  |> advent.add_padding_days(up_to: 12)
  |> advent.run
}
