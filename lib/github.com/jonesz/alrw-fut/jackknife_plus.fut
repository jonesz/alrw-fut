--| Implementation based on "Predictive inference with the jackknife+"
--| by Barber, Candes, Ramdas, Tibsharani (https://arxiv.org/pdf/1905.02928).
-- TODO: We could utilize `radix_sort`, but we need to split the modules into
-- `int` and `f32` implementations.
import "../../diku-dk/sorts/merge_sort"

-- The `(1 - a)` quantile of the empirical distribution.
local def q_hat_u 't [n] a (sorted_v: [n]t) =
  let idx = n + 1 |> f32.i64 |> (*) (1f32 - a) |> f32.ceil |> i64.f32
  in sorted_v[idx]

-- The `a` quantile of the empirical distribution.
local def q_hat_l 't [n] a (sorted_v: [n]t) =
  let idx = n + 1 |> f32.i64 |> (*) a |> f32.floor |> i64.f32
  in sorted_v[idx]

-- The set of `u_i` weights trained on the `i` leave-one-out set.
local def u_loo 't [n] A (F: [n]([]t, t)) =
  let loo x = rotate x F |> tail
  in map (loo) (iota n) |> map (A)

-- The residuals for each `u_i` with their corresponding leave-one-out value: |Y_i - u_i(X_i)|.
local def R_loo 't [n] u residual_fn w (F: [n]([]t, t)) =
  let loo x = rotate x F |> head
  let (x_loo, y_loo) = map (loo) (iota n) |> unzip
  in map2 (u) w x_loo |> map2 (residual_fn) y_loo

--| Jackknife+, which will always achieve a coverage rate of `1-2a`.
module mk_jackknife_plus (T: numeric) = {
  type t = T.t

  local def residual_fn y x = (T.-) y x |> T.abs
  -- A comparison based sort because `numeric` has no constraints upon `get_bit`, `bits`.
  local def sort = merge_sort (T.<=)

  --| Fit models `u_i` with `A` according to the jackknife+
  def fit A u F =
    let w = u_loo A F
    let r = R_loo u residual_fn w F
    in zip w r

  --| Provide a confidence interval for `x` according to jackknife+ utilizing previously trained
  --| data.
  def pred a u w_r x =
    let (w, r) = unzip w_r                     -- (weights, residuals).
    let p = map (\w_i -> u w_i x) w            -- Y_{i+1} = u_i(w_i, x_{i+1}).
    let sorted_v_l = map2 (T.-) p r |> sort
    let sorted_v_u = map2 (T.+) p r |> sort
    in (q_hat_l a sorted_v_l, q_hat_u a sorted_v_u)  -- (9).
}

--| The jackknife-minmax which will always achieve a target coverage rate of `1-a`.
module mk_jackknife_mm (T: numeric) = {
  type t = T.t

  module J = mk_jackknife_plus T

  local def sort = merge_sort (T.<=)

  def fit = J.fit

  def pred a u w_r x =
    let (w, r) = unzip w_r                     -- (weights, residuals).
    let p = map (\w_i -> u w_i x) w            -- Y_{i+1} = u_i(w_i, x_{i+1}).
    let r = sort r                             -- Can now take the quantile of the residuals.

    let lb = q_hat_u a r |> (T.-) (T.minimum p)
    let ub = q_hat_u a r |> (T.+) (T.maximum p) 

    in (lb, ub)
}
