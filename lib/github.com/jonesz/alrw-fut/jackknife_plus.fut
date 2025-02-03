-- https://arxiv.org/pdf/1905.02928
import "../../diku-dk/sorts/radix_sort"

local def q_hat_u 't [n] a (sorted_v: [n]t) =
  let idx = n + 1 |> f32.i64 |> (*) (1f32 - a) |> f32.ceil |> i64.f32
  in sorted_v[idx]

local def q_hat_l 't [n] a (sorted_v: [n]t) =
  let idx = n + 1 |> f32.i64 |> (*) a |> f32.floor |> i64.f32
  in sorted_v[idx]

local def mu_loo 't 'w [n] [d] (F: [n]([d]t, t)) (A: []([d]t, t) -> w) : [n]w =
  let loo x = rotate x F |> tail
  in map (loo) (iota n) |> map (A)

local def R_loo 't 'w [n] [d]
                (mu: w -> [d]t -> t)
                (residual: t -> t -> t)
                (weights: [n]w)
                (F: [n]([d]t, t)) =
  let loo x = rotate x F |> head
  let (x_loo, y_loo) = map (loo) (iota n) |> unzip
  in map2 (mu) weights x_loo |> map2 (residual) y_loo

local def jackknife_plus_fit 't 'w [d] [n]
                             (A: []([d]t, t) -> w)
                             (mu: w -> [d]t -> t)
                             (residual: t -> t -> t)
                             (F: [n]([d]t, t)) =
  let weights = mu_loo F A
  let residuals_loo = R_loo mu residual weights F
  in (weights, residuals_loo)

local def jackknife_plus_pred 't 'w [d] [n]
                              (weights: [n]w)
                              (residuals: [n]t)
                              (sort: [n]t -> [n]t)
                              (mu: w -> [d]t -> t)
                              (add: t -> t -> t)
                              (sub: t -> t -> t)
                              (a: f32)
                              (x: [d]t) =
  let pred = map (\z -> mu z x) weights
  let sorted_v_l = map2 (sub) pred residuals |> sort
  let sorted_v_u = map2 (add) pred residuals |> sort
  in (q_hat_l a sorted_v_l, q_hat_u a sorted_v_u)

module mk_jackknife_plus (T: integral) = {
  type t = T.t

  local def residual_fn y x = (T.-) y x |> T.abs

  def fit A mu F =
    jackknife_plus_fit A mu residual_fn F

  def pred weights residuals mu a x =
    jackknife_plus_pred weights residuals (radix_sort T.num_bits T.get_bit) mu (T.+) (T.-) a x
}
