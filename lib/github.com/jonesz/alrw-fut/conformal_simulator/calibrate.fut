import "nested"
import "pkde"

-- https://futhark-lang.org/examples/histograms.html
def histogram [n] (k: i64) (is: [n]i64) : [k]i32 =
  hist (+) 0 k is (replicate n 1)

-- Given the distribution of conformal scores, compute the correspond `(1-a)` nested set (**Figure 3.2**).
def conformal_cutoff [k] alpha (p: [k]i32) =
  let s = scan (+) 0 p 
  -- The total number of conformal scores is at `last s`; we want to find the minimum conformal
  -- score that eclipses `(1-a) * total`.
  let (idx, _) = zip (iota k) s |> filter (\a -> a.1 >= ((1f32 - alpha) * (f32.i32 (last s)) |> i32.f32)) |> head
  in idx

module mk_calibrate (R: real) = {
  type t = R.t
  module P = mk_pkde R
  module N = mk_nested_min_covering R

  -- Calibrate with pre-existing `Y_hat` (the simulator output).
  def calibrate 'a [m] [B] d_y z sigma (Y: [m]a) (Y_hat: [m][B]a) =
    map2 (\y y_hat -> P.rank d_y sigma y_hat |> flip (N.sequence d_y z) y_hat |> N.cs d_y y) Y Y_hat |> histogram B
}
