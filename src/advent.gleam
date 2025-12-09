import advent/internal/project
import advent/internal/timer.{
  type TimeMode, type Timing, DoTimings, NoTimings, Timing,
}
import advent/internal/tree
import exception.{Errored, Exited, Thrown}
import filepath
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/erlang/process.{type Monitor, type Pid, type Subject, Abnormal}
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/time/calendar.{type Date, December}
import gleam/time/timestamp
import gleam_community/ansi
import logging
import simplifile
import string_width.{Center, Left, Top}
import tobble
import tobble/table_render_opts

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
  Year(
    year: Int,
    days: Dict(Int, fn() -> Report),
    time_mode: TimeMode,
    run_mode: RunMode,
    pad_up_to_days: Int,
    input_mode: InputMode,
  )
}

type RunMode {
  Sequential
  Parallel
}

type InputMode {
  TryDownloading(session_cookie: String)
  NeverDownload
}

/// Creates a new year to collect the solutions for each day.
/// - To add days to run to this year, you can use the [`add_day`](#add_day)
///   function.
/// - After having added all the days you care about, you can run them with the
///   [`run`](#run) function.
///
pub fn year(year: Int) -> Year {
  Year(
    year:,
    days: dict.new(),
    time_mode: NoTimings,
    run_mode: Parallel,
    pad_up_to_days: 1,
    input_mode: NeverDownload,
  )
}

/// All the days added after this is applied to the runner will also include
/// timing informations in their output.
///
/// The timings take the average of non-outlier measurements and include the
/// time it takes to run the parsing function as well.
///
pub fn timed(year: Year) -> Year {
  Year(..year, time_mode: DoTimings)
}

/// Runs all the days one after another in sequence, rather than the default
/// behaviour of running everything in parallel.
///
pub fn sequential(year: Year) -> Year {
  Year(..year, run_mode: Sequential)
}

/// For all the days added after this option is applied, the runner will try
/// downloading their input if it cannot be found at the expected path.
///
pub fn download_missing_days(year: Year, session_cookie: String) -> Year {
  Year(..year, input_mode: TryDownloading(session_cookie:))
}

/// Adds a new day to the given year.
/// This day will be run independently from all the other ones.
///
pub fn add_day(year: Year, day: Day(_, _, _)) -> Year {
  let Year(year: year_number, days:, time_mode:, input_mode:, ..) = year

  let days =
    dict.insert(days, day.day, fn() {
      case get_input(input_mode, year_number, day.day, today()) {
        Error(report) -> report
        Ok(input) -> run_day(input, time_mode, day)
      }
    })

  Year(..year, days:)
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
pub fn add_padding_days(year: Year, up_to days: Int) -> Year {
  Year(..year, pad_up_to_days: days)
}

/// Run all the days in the given year.
/// All the days run in parallel and new outcomes will be displayed as soon as
/// they are produced.
///
/// > 🎄 The output of each day is expected to be located in a file at the
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

  process.spawn(fn() { days_runner(me, year) })

  let missing_days = set.from_list(dict.keys(year.days))
  let tree = tree.generate(10)

  io.println("\u{1b}[2J\u{1b}[0;0H")
  report_loop(year, tree, me, missing_days, dict.new())
}

fn days_runner(reporter: Subject(Report), year: Year) -> Nil {
  dict.each(year.days, fn(_day, run_day) {
    case year.run_mode {
      Sequential -> process.send(reporter, run_day())
      Parallel -> {
        let _ = process.spawn(fn() { process.send(reporter, run_day()) })
        Nil
      }
    }
  })
}

fn report_loop(
  year: Year,
  tree: String,
  me: Subject(Report),
  missing_days: Set(Int),
  completed_days: Dict(Int, Report),
) {
  case set.is_empty(missing_days) {
    True -> print_reports(year, tree, missing_days, completed_days)

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
  year: Year,
  tree: String,
  missing_days: Set(Int),
  completed_days: Dict(Int, Report),
) {
  let max_day =
    missing_days
    |> set.to_list
    |> list.append(dict.keys(completed_days))
    |> list.fold(1, int.max)

  io.println("\u{1b}[0;0H")
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

  let ribbon_and_calendar =
    [
      left <> " " <> ansi.red(int.to_string(year.year)) <> " " <> right <> "\n",
      calendar_view(year, today(), max_day, completed_days),
    ]
    |> string_width.stack_vertical(align: Center, gap: 1, with: " ")

  let tree_and_calendar =
    [
      tree,
      ribbon_and_calendar,
    ]
    |> string_width.stack_horizontal(place: Top, gap: 5, with: " ")
    |> string_width.align(to: cols, align: Center, with: " ")

  case details_table_view(max_day, completed_days) {
    Error(_) -> [tree_and_calendar]
    Ok(details_table) -> [
      tree_and_calendar,
      details_table,
    ]
  }
  |> string_width.stack_vertical(align: Left, gap: 1, with: " ")
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

fn today() -> Date {
  let #(today, _) =
    timestamp.system_time()
    |> timestamp.to_calendar(calendar.local_offset())

  today
}

fn calendar_view(
  year: Year,
  today: Date,
  max_day: Int,
  completed_days: Dict(Int, Report),
) -> String {
  list.range(1, int.max(max_day, year.pad_up_to_days))
  |> list.sized_chunk(into: 7)
  |> list.map(fn(row) {
    let days =
      list.map(row, fn(day) {
        case day == today.day {
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
    Error(_) | Ok(CannotDownloadFromTheFuture(..)) -> "  "
    Ok(ParseFailed(day: _, message: _)) -> ansi.red("P ")
    Ok(NoFile(..)) | Ok(FailedToDownload(..)) | Ok(FileError(..)) ->
      ansi.red("F ")
    Ok(Ran(day: _, outcome_a:, outcome_b:)) ->
      outcome_to_dot(outcome_a.0) <> outcome_to_dot(outcome_b.0)
  }
}

fn outcome_to_dot(outcome: Outcome) -> String {
  case outcome {
    TimedOut -> ansi.red("~")
    Todo(_) -> ansi.dim("~")
    Crashed(_) -> ansi.red("x")
    Bare(_) -> ansi.yellow("⋆")
    Success(_) -> ansi.green("⋆")
    Failure(..) | KnownFailure(_) -> ansi.red("⋆")
  }
}

fn details_table_view(
  max_day: Int,
  completed_days: Dict(Int, Report),
) -> Result(String, Nil) {
  let completed_days =
    list.range(1, max_day)
    |> list.filter_map(dict.get(completed_days, _))

  let max_digits = max_digits(in: completed_days)
  let rows =
    completed_days
    |> list.filter_map(pretty_report_columns(_, max_digits))

  case rows {
    [] -> Error(Nil)
    _ -> {
      let assert Ok(table) =
        tobble.builder()
        |> tobble.add_row(["day", "part 1", "part 2"])
        |> list.fold(over: rows, with: tobble.add_row)
        |> tobble.build

      tobble.render_with_options(table, [
        table_render_opts.no_horizontal_rules(),
        table_render_opts.line_type_blank(),
      ])
      |> string.replace("•", with: " ")
      |> Ok
    }
  }
}

fn max_digits(in reports: List(Report)) -> Option(#(Int, Int)) {
  let #(digits_units, digits_decimals) =
    list.flat_map(reports, fn(report) {
      case report {
        FailedToDownload(..)
        | CannotDownloadFromTheFuture(..)
        | FileError(..)
        | NoFile(..)
        | ParseFailed(..) -> []
        Ran(outcome_a: #(_, time_a), outcome_b: #(_, time_b), day: _) ->
          case time_a, time_b {
            Some(time_a), Some(time_b) -> [time_a, time_b]
            None, Some(time_b) -> [time_b]
            Some(time_a), None -> [time_a]
            None, None -> []
          }
      }
    })
    |> list.map(fn(time) {
      let elapsed = timer.from_us(time.elapsed_us)
      #(count_digits(elapsed.units), count_digits(elapsed.decimals))
    })
    |> list.unzip

  option.from_result({
    use units <- result.try(list.max(digits_units, int.compare))
    use decimals <- result.try(list.max(digits_decimals, int.compare))
    Ok(#(units, decimals))
  })
}

fn count_digits(number: Int) {
  case number <= 9 {
    True -> 1
    False -> 1 + count_digits(number / 10)
  }
}

fn pretty_report_columns(
  report: Report,
  max_digits: Option(#(Int, Int)),
) -> Result(List(String), Nil) {
  let pretty_day = pretty_day(report.day)
  case report {
    CannotDownloadFromTheFuture(..) -> Error(Nil)
    FileError(..) -> Ok([pretty_day, ansi.red("unexpected file error"), ""])
    FailedToDownload(..) -> Ok([pretty_day, ansi.red("failed to download"), ""])
    NoFile(..) -> Ok([pretty_day, ansi.red("missing input file"), ""])
    ParseFailed(message:, ..)
      if message == panic_default_message
      || message == assert_default_message
      || message == let_assert_default_message
      || message == todo_default_message
      || message == ""
    -> Ok([pretty_day, ansi.red("parse"), ""])
    ParseFailed(message:, ..) ->
      Ok([pretty_day, ansi.red("parse: " <> message), ""])
    Ran(outcome_a: #(result_a, timing_a), outcome_b: #(result_b, timing_b), ..) ->
      case
        outcome_to_detail(result_a, timing_a, max_digits),
        outcome_to_detail(result_b, timing_b, max_digits)
      {
        Ok(detail_a), Ok(detail_b) -> Ok([pretty_day, detail_a, detail_b])
        Ok(detail_a), Error(_) -> Ok([pretty_day, detail_a, ""])
        Error(_), Ok(detail_b) -> Ok([pretty_day, "", detail_b])
        Error(_), Error(_) -> Error(Nil)
      }
  }
}

const panic_default_message = "`panic` expression evaluated."

const assert_default_message = "Assertion failed."

const let_assert_default_message = "Pattern match failed, no pattern matched the value."

const todo_default_message = "`todo` expression evaluated. This code has not yet been implemented."

fn outcome_to_detail(
  outcome: Outcome,
  timing: Option(Timing),
  max_digits: Option(#(Int, Int)),
) -> Result(String, Nil) {
  case outcome, timing {
    TimedOut, _ -> Ok(ansi.red("timed out"))

    Todo(message), _ if message == todo_default_message -> Error(Nil)
    Todo(message: ""), _ -> Ok(ansi.dim("todo"))
    Todo(message:), _ -> Ok(ansi.dim("todo: " <> message))

    Crashed(message), _
      if message == panic_default_message
      || message == assert_default_message
      || message == let_assert_default_message
      || message == ""
    -> Ok(ansi.red("panic"))
    Crashed(message:), _ -> Ok(ansi.red("panic: " <> message))

    Bare(value:), None -> Ok(ansi.yellow(value))
    Bare(value:), Some(timing) ->
      Ok(
        ansi.yellow(value)
        <> " "
        <> ansi.dim(pretty_elapsed_time(timing, max_digits)),
      )

    Success(value: _), None -> Error(Nil)
    Success(value: _), Some(timing) ->
      Ok(ansi.dim(pretty_elapsed_time(timing, max_digits)))

    Failure(got: _, expected: _), _ -> Error(Nil)
    KnownFailure(got: _), _ -> Error(Nil)
  }
}

fn pretty_elapsed_time(
  outcome: Timing,
  max_digits: Option(#(Int, Int)),
) -> String {
  let Timing(elapsed_us:, standard_deviation_us:) = outcome
  let elapsed = timer.from_us(elapsed_us)
  let deviation = timer.from_us_with_unit(standard_deviation_us, elapsed.unit)

  let deviation = case deviation.units, deviation.decimals {
    0, 0 -> ""
    units, 0 -> " ±" <> to_subscript(units)
    units, decimals ->
      " ±" <> to_subscript(units) <> "." <> to_subscript(decimals)
  }

  let #(max_units_digits, max_decimals_digits) =
    option.unwrap(max_digits, #(2, 1))

  string.pad_start(
    int.to_string(elapsed.units),
    to: max_units_digits,
    with: "•",
  )
  <> "."
  <> string.pad_end(
    int.to_string(elapsed.decimals),
    to: max_decimals_digits,
    with: "0",
  )
  <> timer.unit_to_string(elapsed.unit)
  <> deviation
}

fn pretty_day(day: Int) -> String {
  int.to_string(day) |> string.pad_start(2, "0")
}

fn to_subscript(int: Int) -> String {
  digits(int)
  |> list.map(digit_to_subscript)
  |> string.concat
}

fn digit_to_subscript(int: Int) -> String {
  case int {
    0 -> "₀"
    1 -> "₁"
    2 -> "₂"
    3 -> "₃"
    4 -> "₄"
    5 -> "₅"
    6 -> "₆"
    7 -> "₇"
    8 -> "₈"
    9 -> "₉"
    _ -> ""
  }
}

fn digits(int: Int) -> List(Int) {
  digits_loop(int, [])
}

fn digits_loop(int: Int, digits: List(Int)) -> List(Int) {
  case int <= 9 {
    True -> [int, ..digits]
    False -> digits_loop(int / 10, [int % 10, ..digits])
  }
}

type Report {
  Ran(
    day: Int,
    outcome_a: #(Outcome, Option(Timing)),
    outcome_b: #(Outcome, Option(Timing)),
  )
  ParseFailed(day: Int, message: String)
  NoFile(day: Int, file: String)
  FailedToDownload(day: Int, file: String)
  CannotDownloadFromTheFuture(day: Int)
  FileError(day: Int, file: String)
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

fn run_day(
  input: String,
  time_mode: TimeMode,
  data: Day(input, output_a, output_b),
) -> Report {
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

  let parsed =
    exception.rescue(fn() { timer.timed(time_mode, fn() { parse(input) }) })

  case parsed {
    Error(Errored(error)) | Error(Exited(error)) | Error(Thrown(error)) ->
      case error_message(error) {
        Error(_) -> ParseFailed(day:, message: "")
        Ok(message) -> ParseFailed(day:, message:)
      }

    Ok(#(parsed, parse_timing)) -> {
      let #(result_a, timing_a) =
        timer.timed(time_mode, fn() {
          run_part(part_a, parsed, expected_a, wrong_answers_a)
        })

      let #(result_b, timing_b) =
        timer.timed(time_mode, fn() {
          run_part(part_b, parsed, expected_b, wrong_answers_b)
        })

      Ran(
        day:,
        outcome_a: #(result_a, {
          use timing_a <- option.then(timing_a)
          use parse_timing <- option.map(parse_timing)
          timer.add(timing_a, parse_timing)
        }),
        outcome_b: #(result_b, {
          use timing_b <- option.then(timing_b)
          use parse_timing <- option.map(parse_timing)
          timer.add(timing_b, parse_timing)
        }),
      )
    }
  }
}

fn get_input(
  mode: InputMode,
  year year: Int,
  day day: Int,
  today today: Date,
) -> Result(String, Report) {
  let project_root = project.find_root()
  let year_string = int.to_string(year) |> string.pad_start(4, "0")
  let day_string = int.to_string(day) |> string.pad_start(2, "0")

  let file =
    project_root
    |> filepath.join("inputs")
    |> filepath.join(year_string)
    |> filepath.join(day_string <> ".txt")

  case simplifile.read(file) {
    Ok(input) -> Ok(input)
    Error(simplifile.Enoent) ->
      case mode {
        NeverDownload -> Error(NoFile(day:, file:))
        TryDownloading(session_cookie:) ->
          download_and_save(
            day:,
            year:,
            today:,
            using: session_cookie,
            to: file,
          )
      }
    Error(_) -> Error(FileError(day:, file:))
  }
}

const user_agent = "github.com/giacomocavalieri/advent"

fn download_and_save(
  day day: Int,
  year year: Int,
  today today: Date,
  using session_cookie: String,
  to file: String,
) -> Result(String, Report) {
  let wanted_day = calendar.Date(year:, month: December, day:)
  use <- bool.guard(
    when: calendar.naive_date_compare(wanted_day, today) == order.Gt,
    return: Error(CannotDownloadFromTheFuture(day)),
  )

  case simplifile.create_directory_all(filepath.directory_name(file)) {
    Error(_) -> Error(FileError(day:, file:))
    Ok(_) -> {
      let year_string = int.to_string(year)
      let day_string = int.to_string(day)

      let path = "/" <> year_string <> "/day/" <> day_string <> "/input"
      let response =
        request.new()
        |> request.set_host("adventofcode.com")
        |> request.set_path(path)
        |> request.set_scheme(http.Https)
        |> request.set_cookie("session", session_cookie)
        |> request.set_header("user-agent", user_agent)
        |> httpc.send
      case response {
        Error(_) -> Error(FailedToDownload(day:, file:))
        Ok(response) if response.status == 200 ->
          case simplifile.write(response.body, to: file) {
            Error(_) -> Error(FileError(day:, file:))
            Ok(_) -> Ok(response.body)
          }
        Ok(_) -> Error(FailedToDownload(day:, file:))
      }
    }
  }
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

@external(erlang, "advent_ffi", "error_message")
fn error_message(dynamic: dynamic.Dynamic) -> Result(String, Nil)
