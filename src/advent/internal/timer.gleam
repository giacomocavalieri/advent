import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

const allowed_us = 500_000

const allowed_repeats = 100

pub type Timing {
  Timing(elapsed_us: Int, standard_deviation_us: Int)
}

pub type TimeMode {
  DoTimings
  NoTimings
}

pub fn add(to one: Timing, this other: Timing) -> Timing {
  Timing(..one, elapsed_us: one.elapsed_us + other.elapsed_us)
}

pub fn timed(time_mode: TimeMode, fun: fn() -> a) -> #(a, Option(Timing)) {
  case time_mode {
    NoTimings -> #(fun(), None)
    DoTimings -> {
      let #(result, timing) = do_timed(fun)
      #(result, Some(timing))
    }
  }
}

fn do_timed(fun: fn() -> a) -> #(a, Timing) {
  let #(_, value) = timer_tc(fun)

  let times = timed_loop(fun, 0, 0, [])
  let times =
    list.filter_map(times, with: fn(time) {
      case is_outlier(time, times) {
        True -> Error(Nil)
        False -> Ok(int.to_float(time))
      }
    })

  let times_count = list.length(times)
  let elapsed_us = float.sum(times) /. int.to_float(times_count)
  let standard_deviation_us = standard_deviation(times, elapsed_us, times_count)
  #(
    value,
    Timing(
      elapsed_us: float.round(elapsed_us),
      standard_deviation_us: float.round(standard_deviation_us),
    ),
  )
}

fn standard_deviation(times: List(Float), average: Float, samples: Int) -> Float {
  let deviations =
    list.fold(over: times, from: 0.0, with: fn(acc, time) {
      let deviation_from_mean = time -. average
      acc +. { deviation_from_mean *. deviation_from_mean }
    })

  let assert Ok(standard_deviation) =
    float.square_root(deviations /. int.to_float(samples))

  standard_deviation
}

fn timed_loop(
  fun: fn() -> a,
  elapsed_us: Int,
  repeats: Int,
  times_us: List(Int),
) -> List(Int) {
  case elapsed_us >= allowed_us || repeats >= allowed_repeats {
    True -> times_us
    False -> {
      let #(time_us, _) = timer_tc(fun)
      timed_loop(fun, elapsed_us + time_us, repeats + 1, [time_us, ..times_us])
    }
  }
}

/// This function determines if a given timing is an outlier using the method
/// described by Boris lglewicz and David C. Hoaglin in
/// "How to Detect and Handle Outliers"
///
/// In particular this method is described in Chapter 3 on page 11
/// "Outlier Labelling" as a "Modified Z Score".
///
@internal
pub fn is_outlier(time: Int, times: List(Int)) -> Bool {
  case modified_z_score(time, times) {
    Ok(modified_z_score) -> float.absolute_value(modified_z_score) >. 3.5
    Error(_) -> False
  }
}

fn modified_z_score(time: Int, times: List(Int)) -> Result(Float, Nil) {
  let times = list.map(times, int.to_float)
  case sample_median(times) {
    Error(_) -> Error(Nil)
    Ok(median) -> {
      let assert Ok(median_of_absolute_deviations) =
        list.map(times, fn(time) { float.absolute_value(time -. median) })
        |> sample_median
        as "we know times cannot be empty if we get here"

      let modified_z_score =
        { 0.6745 *. { int.to_float(time) -. median } }
        /. median_of_absolute_deviations

      Ok(modified_z_score)
    }
  }
}

@internal
pub fn sample_median(list: List(Float)) -> Result(Float, Nil) {
  let list = list.sort(list, float.compare)
  let length = list.length(list)
  case int.is_even(length) {
    True ->
      case list.drop(list, length / 2 - 1) {
        [a, b, ..] -> Ok({ a +. b } /. 2.0)
        _ -> Error(Nil)
      }

    False ->
      case list.drop(list, length / 2) {
        [] -> Error(Nil)
        [middle, ..] -> Ok(middle)
      }
  }
}

@external(erlang, "timer", "tc")
fn timer_tc(function: fn() -> a) -> #(Int, a)

// NICE FORMATTING OF TIMES ----------------------------------------------------

pub type DisplayUnit {
  Ms
  Us
}

pub type DisplayTime {
  DisplayTime(unit: DisplayUnit, units: Int, decimals: Int)
}

pub fn from_us(us: Int) -> DisplayTime {
  case us >= 100 {
    True -> from_us_with_unit(us, Ms)
    False -> from_us_with_unit(us, Us)
  }
}

pub fn from_us_with_unit(us: Int, unit: DisplayUnit) {
  case unit {
    Ms -> DisplayTime(unit: Ms, units: us / 1000, decimals: us % 1000)
    Us -> DisplayTime(unit: Us, units: us, decimals: us)
  }
}

pub fn unit_to_string(unit: DisplayUnit) {
  case unit {
    Ms -> "ms"
    Us -> "μs"
  }
}
