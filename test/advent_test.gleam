import advent/internal/timer
import gleam/list
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn outlier_detection_test() {
  let samples = [21, 21, 23, 23, 24, 25, 26, 26, 82, 83]

  assert list.map(samples, fn(sample) { timer.is_outlier(sample, samples) })
    == [False, False, False, False, False, False, False, False, True, True]
}

pub fn middle_test() {
  assert timer.sample_median([1.0, 2.0, 3.0]) == Ok(2.0)
  assert timer.sample_median([1.0, 2.0, 3.0, 4.0]) == Ok(2.5)
}
