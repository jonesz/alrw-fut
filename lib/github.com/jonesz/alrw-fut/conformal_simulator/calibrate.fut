import "nested"
import "pkde"

-- https://futhark-lang.org/examples/histograms.html
def histogram [n] (k: i64) (is: [n]i64) : [k]i32 =
  hist (+) 0 k is (replicate n 1)

module mk_calibrate (R: real) = {
  type t = R.t
  module P = mk_pkde R
  module N = mk_nested R

  -- Calibrate with pre-existing `Y_hat` (the simulator output).
  def calibrate 'a [m] [B] dy sigma (Y: [m]a) (Y_hat: [m][B]a) =
    map2 (\y y_hat -> P.rank dy sigma y_hat |> flip N.sequence y_hat |> N.cs dy y) Y Y_hat |> histogram B
}
