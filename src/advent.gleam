import advent/internal/project
import advent/internal/tree
import exception
import filepath
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/erlang/process.{type Monitor, type Pid, type Subject, Abnormal}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import gleam_community/ansi
import logging
import simplifile
import string_width

/// An Advent of Code day.
///
pub type Day(input, output_a, output_b) {
  Day(
    /// The number of the day.
    day: Int,
    /// A function taking the problem input and parsing it.
    parse: fn(String) -> input,
    /// A function to solve part a.
    part_a: fn(input) -> output_a,
    /// The solution for part a. If `Some`, the output of `part_a` is compared
    /// with this one to make sure it's producing the expected outcome.
    expected_a: Option(output_a),
    /// Values that are known to be wrong for part a.
    /// If the output of `part_a` appears in this list it will be reported as
    /// wrong without any need of submitting it to the Advent of Code website.
    /// This is particularly useful to keep track of past failed attempts and
    /// not risk resubmitting them.
    wrong_answers_a: List(output_a),
    /// A function to solve part b.
    part_b: fn(input) -> output_b,
    /// The solution for part b. If `Some`, the output of `part_b` is compared
    /// with this one to make sure it's producing the expected outcome.
    expected_b: Option(output_b),
    /// Values that are known to be wrong for part b.
    /// If the output of `part_b` appears in this list it will be reported as
    /// wrong without any need of submitting it to the Advent of Code website.
    /// This is particularly useful to keep track of past failed attempts and
    /// not risk resubmitting them.
    wrong_answers_b: List(output_b),
  )
}

/// An Advent of Code year collecting many days.
/// - To build one, use the [`year`](#year) function.
/// - To add new days, use the [`add_day`](#add_day) function.
/// - To add placeholder days, use the [`add_padding_days`](#add_padding_days)
///   function.
///
pub opaque type Year {
  Runner(year: Int, days: Dict(Int, fn() -> Report))
}

/// Creates a new year to collect the solutions for each day.
/// - To add days to run to this year, you can use the [`add_day`](#add_day)
///   function.
/// - After having added all the days you care about, you can run them with the
///   [`run`](#run) function.
///
pub fn year(year: Int) -> Year {
  Runner(year:, days: dict.new())
}

/// Adds a new day to the given year.
/// This day will be run independently from all the other ones.
///
pub fn add_day(runner: Year, day: Day(_, _, _)) -> Year {
  let year = runner.year
  let days =
    dict.insert(runner.days, day.day, fn() {
      case get_input(year, day.day) {
        Error(file) -> NoFile(day: day.day, file:)
        Ok(input) -> run_day(input, day)
      }
    })

  Runner(..runner, days:)
}

/// This adds a number of padding days until there's the given number of days.
/// For example, if the year you're solving has 25 days you could use it like
/// this:
///
/// ```gleam
/// advent.year(2019)
/// |> advent.add_day(todo as "my solution to day 1")
/// |> advent.add_day(todo as "my solution to day 2")
/// |> advent.add_padding_days(up_to: 25)
/// ```
///
/// So despite having only two solutions, the full 25 day calendar will be
/// displayed when ran.
///
pub fn add_padding_days(runner: Year, up_to days: Int) -> Year {
  let total_days = dict.size(runner.days)
  let max_day = dict.keys(runner.days) |> list.fold(1, int.max)
  let missing_days = days - total_days
  case missing_days <= 0 {
    True -> runner
    False ->
      list.range(max_day + 1, max_day + missing_days)
      |> list.map(
        Day(
          day: _,
          parse: fn(_) { Nil },
          part_a: fn(_) { panic as "todo" },
          part_b: fn(_) { panic as "todo" },
          expected_a: None,
          wrong_answers_a: [],
          expected_b: None,
          wrong_answers_b: [],
        ),
      )
      |> list.fold(runner, add_day)
  }
}

/// Run all the days in the given year.
/// All the days run in parallel and new outcomes will be displayed as soon as
/// they are produced.
///
/// > 🎄 The input of each day is expected to be located in a file at the
/// > given path, relative to the root of your Gleam project:
/// > `inputs/<YEAR>/<DAY>.txt`
/// >
/// > For example, the input for day 2 of year 2019 will be
/// > `inputs/2019/02.txt`.
///
pub fn run(year: Year) -> Nil {
  logging.configure()
  logging.set_level(logging.Emergency)
  let me = process.new_subject()

  dict.each(year.days, fn(_day, run_day) {
    process.spawn(fn() { process.send(me, run_day()) })
  })

  let missing_days = dict.keys(year.days) |> set.from_list
  let tree = tree.generate(10)

  report_loop(year.year, tree, me, missing_days, dict.new())
}

fn report_loop(
  year: Int,
  tree: String,
  me: Subject(Report),
  missing_days: Set(Int),
  completed_days: Dict(Int, Report),
) {
  case set.is_empty(missing_days) {
    True -> {
      print_reports(year, tree, missing_days, completed_days)
      io.println("\n\n")
    }

    False -> {
      print_reports(year, tree, missing_days, completed_days)
      let report = process.receive_forever(me)
      let missing_days = set.delete(missing_days, report.day)
      let completed_days = dict.insert(completed_days, report.day, report)
      report_loop(year, tree, me, missing_days, completed_days)
    }
  }
}

fn print_reports(
  year: Int,
  tree: String,
  missing_days: Set(Int),
  completed_days: Dict(Int, Report),
) {
  let max_day =
    missing_days
    |> set.to_list
    |> list.append(dict.keys(completed_days))
    |> list.fold(1, int.max)

  io.println("\u{1b}[2J\u{1b}[0;0H")
  let left =
    ansi.yellow("⋆")
    <> ansi.yellow("꙳")
    <> ansi.red("•̩̩͙")
    <> ansi.red("❅")
    <> ansi.green("*̩̩͙")
    <> ansi.red("‧͙")
    <> ansi.red("‧͙")
    <> ansi.yellow("*̩̩͙")
    <> ansi.red("❆")
    <> ansi.green(" ͙͛")
    <> ansi.red("˚")
    <> ansi.green("₊")
    <> ansi.yellow("⋆")
  let right =
    ansi.yellow("⋆")
    <> ansi.green("₊")
    <> ansi.red("˚")
    <> ansi.green(" ͙͛")
    <> ansi.red("❆")
    <> ansi.yellow("*̩̩͙")
    <> ansi.red("‧͙")
    <> ansi.red("‧͙")
    <> ansi.green("*̩̩͙")
    <> ansi.red("❅")
    <> ansi.red("•̩̩͙")
    <> ansi.yellow("꙳")
    <> ansi.yellow("⋆")

  let #(rows, cols) = term_size()

  [
    tree,
    [
      left <> " " <> ansi.red(int.to_string(year)) <> " " <> right <> "\n",
      calendar_view(today(), max_day, completed_days),
      detail_view(max_day, completed_days),
    ]
      |> string_width.stack_vertical(
        align: string_width.Center,
        gap: 1,
        with: " ",
      ),
  ]
  |> string_width.stack_horizontal(place: string_width.Top, gap: 5, with: " ")
  |> string_width.align(to: cols, align: string_width.Center, with: " ")
  |> vertical_center_align(rows)
  |> io.println
}

fn vertical_center_align(string: String, rows: Int) -> String {
  case string_width.dimensions(string).rows {
    required if required >= rows -> string
    required -> string.repeat("\n", { rows - required } / 2 - 1) <> string
  }
}

fn term_size() -> #(Int, Int) {
  case string_width.get_terminal_size() {
    Error(_) -> #(0, 80)
    Ok(size) -> #(size.rows, size.columns)
  }
}

fn today() -> Int {
  let #(today, _) =
    timestamp.system_time()
    |> timestamp.to_calendar(calendar.local_offset())

  today.day
}

fn calendar_view(today: Int, max_day: Int, completed_days: Dict(Int, Report)) {
  list.range(1, max_day)
  |> list.sized_chunk(into: 7)
  |> list.map(fn(row) {
    let days =
      list.map(row, fn(day) {
        case day == today {
          True -> pretty_day(day)
          False -> ansi.dim(pretty_day(day))
        }
      })
      |> string.join(with: " ")

    let outcomes =
      list.map(row, day_to_dots(_, completed_days))
      |> string.join(with: " ")

    days <> "\n" <> outcomes
  })
  |> string.join(with: "\n")
}

fn day_to_dots(day: Int, completed_days: Dict(Int, Report)) -> String {
  case dict.get(completed_days, day) {
    Error(_) -> ansi.dim("  ")
    Ok(ParseFailed(day: _)) -> ansi.red("xx")
    Ok(NoFile(day: _, file: _)) -> ansi.red("  ")
    Ok(Ran(day: _, outcome_a:, outcome_b:)) ->
      outcome_to_dot(outcome_a) <> outcome_to_dot(outcome_b)
  }
}

fn outcome_to_dot(outcome: Outcome) -> String {
  case outcome {
    TimedOut -> ansi.red("~")
    Crashed("todo") | Todo(_) -> ansi.dim("~")
    Crashed(_) -> ansi.red("x")
    Bare(_) -> ansi.yellow("⋆")
    Success(_) -> ansi.green("⋆")
    Failure(..) | KnownFailure(_) -> ansi.red("⋆")
  }
}

fn detail_view(max_day: Int, completed_days: Dict(Int, Report)) -> String {
  list.range(1, max_day)
  |> list.filter_map(dict.get(completed_days, _))
  |> list.filter_map(pretty_report)
  |> string_width.stack_vertical(align: string_width.Left, gap: 0, with: " ")
}

fn pretty_report(report: Report) -> Result(String, Nil) {
  case report {
    NoFile(..) -> Error(Nil)
    ParseFailed(..) -> Error(Nil)
    Ran(day:, outcome_a:, outcome_b:) ->
      case outcome_to_detail(outcome_a), outcome_to_detail(outcome_b) {
        Ok(detail_a), Ok(detail_b) -> Ok(day_report(day, detail_a, detail_b))
        Ok(detail_a), Error(_) -> Ok(day_report(day, detail_a, "      "))
        Error(_), Ok(detail_b) -> Ok(day_report(day, "      ", detail_b))
        Error(_), Error(_) -> Error(Nil)
      }
  }
}

fn day_report(day: Int, details_a: String, details_b: String) -> String {
  [ansi.dim(pretty_day(day)), details_a, details_b]
  |> string_width.stack_horizontal(place: string_width.Top, gap: 2, with: " ")
}

fn outcome_to_detail(outcome: Outcome) -> Result(String, Nil) {
  case outcome {
    TimedOut -> Ok("timed out")

    Crashed("`panic` expression evaluated.")
    | Crashed("Assertion failed.")
    | Crashed("Pattern match failed, no pattern matched the value.") ->
      Ok(ansi.red("panic"))

    Crashed("todo")
    | Todo(
        "`todo` expression evaluated. This code has not yet been implemented.",
      ) -> Error(Nil)

    Todo(message:) -> Ok(ansi.dim("todo: " <> message))
    Crashed(message:) -> Ok(ansi.red("panic: " <> message))

    Bare(value:) -> Ok(ansi.yellow(value))
    Success(value: _) -> Error(Nil)
    Failure(got: _, expected: _) -> Error(Nil)
    KnownFailure(got: _) -> Error(Nil)
  }
}

fn pretty_day(day: Int) -> String {
  int.to_string(day) |> string.pad_start(2, "0")
}

type Report {
  Ran(day: Int, outcome_a: Outcome, outcome_b: Outcome)
  ParseFailed(day: Int)
  NoFile(day: Int, file: String)
}

type Outcome {
  /// The part couldn't be completed in the allotted time. So it was stopped.
  ///
  TimedOut

  /// The part crashed for some reason.
  ///
  Crashed(message: String)

  /// The part is incomplete and a `todo` expression was evaluated.
  ///
  Todo(message: String)

  /// The part produced some output value. Since there's no known answer we
  /// don't yet know if that's correct!
  ///
  Bare(value: String)

  /// The part produced a value that we know is the correct one!
  ///
  Success(value: String)

  /// The part produced a value that we know is not the correct one!
  ///
  Failure(got: String, expected: String)

  /// The part produced a value that we know is not right because we've already
  /// tried submitting it and it was rejected as a correct solution.
  ///
  KnownFailure(got: String)
}

fn run_day(input: String, data: Day(input, output_a, output_b)) -> Report {
  let Day(
    day:,
    parse:,
    part_a:,
    part_b:,
    expected_a:,
    expected_b:,
    wrong_answers_a:,
    wrong_answers_b:,
  ) = data

  case exception.rescue(fn() { parse(input) }) {
    Error(_) -> ParseFailed(day:)
    Ok(parsed) -> {
      let outcome_a = run_part(part_a, parsed, expected_a, wrong_answers_a)
      let outcome_b = run_part(part_b, parsed, expected_b, wrong_answers_b)
      Ran(day:, outcome_a:, outcome_b:)
    }
  }
}

fn get_input(year year: Int, day day: Int) -> Result(String, String) {
  let project_root = project.find_root()
  let year = int.to_string(year) |> string.pad_start(4, "0")
  let day = int.to_string(day) |> string.pad_start(2, "0")

  let file =
    project_root
    |> filepath.join("inputs")
    |> filepath.join(year)
    |> filepath.join(day <> ".txt")

  simplifile.read(file)
  |> result.replace_error(file)
}

fn run_part(
  part: fn(input) -> output,
  parsed: input,
  expected: Option(output),
  wrong_answers: List(output),
) -> Outcome {
  use <- with_timeout(5000)

  let output = part(parsed)
  let string_output = string.inspect(output)
  case list.contains(wrong_answers, output) {
    True -> KnownFailure(string_output)
    False ->
      case expected {
        None -> Bare(string_output)
        Some(expected) if output == expected -> Success(string_output)
        Some(expected) ->
          Failure(got: string_output, expected: string.inspect(expected))
      }
  }
}

// TIMEOUT HELPERS -------------------------------------------------------------

fn with_timeout(timeout: Int, run: fn() -> Outcome) -> Outcome {
  let me = process.new_subject()

  let #(_pid, monitor) =
    spawn_monitor(fn() { process.send(me, Complete(run())) })

  process.new_selector()
  |> process.select(me)
  |> process.select_specific_monitor(monitor, MonitorDown)
  |> receive_outcome(timeout)
}

fn receive_outcome(
  selector: process.Selector(Message(output)),
  timeout: Int,
) -> Outcome {
  case process.selector_receive(selector, timeout) {
    Error(_) -> TimedOut
    Ok(MonitorDown(process.ProcessDown(reason: Abnormal(reason), ..))) ->
      error_to_outcome(reason)
    Ok(MonitorDown(_)) -> Crashed("unknown reason")
    Ok(Complete(outcome)) ->
      case process.selector_receive(selector, 100) {
        Error(_) -> panic as "monitor didn't send the message somehow"
        Ok(_) -> outcome
      }
  }
}

type Message(output) {
  MonitorDown(process.Down)
  Complete(Outcome)
}

@external(erlang, "erlang", "spawn_monitor")
fn spawn_monitor(function: fn() -> a) -> #(Pid, Monitor)

@external(erlang, "advent_ffi", "decode_error")
fn error_to_outcome(dynamic: dynamic.Dynamic) -> Outcome
