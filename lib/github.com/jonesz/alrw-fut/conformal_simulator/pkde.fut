-- | To convert the simulated set of points into a set of nested sets, we need
-- to rank each member of `sim_hat(x_i, B)` (which is a set of `B` simulated
-- points). LeRoy does this via Gaussian kernel pseudo-density estimator.

module type kde = {
  type t

  -- | Rank the members of `sim_hat(x_i, B)` via a KDE.
  -- Contains two tuning parameters: a distance function `d_y` and a value
  -- `sigma` that is basically the "bandwidth" parameter `h` of KDEs.
  val rank 'a [B] : (d_y: a -> a -> t) -> (sigma: t) -> [B]a -> [B]t
}

module mk_pkde (R: real) : kde with t = R.t = {
  type t = R.t

  def K u = -- (3.1) ~ exp(-1 * u^2)
    (R.**) u (R.i64 2i64) |> R.neg |> R.exp

  -- Rank the members of `sim_hat(x_i, B)` via a GKPDE.
  def rank 'a [B] d_y sigma (y_hat: [B]a) =
    let mean [z] (xs: [z]t) = R.sum xs |> flip (R./) (R.i64 z)
    let inner y_b y_i = -- (3.1) ~ K(d_y(y_b, y_i) / sigma)
      d_y y_b y_i |> flip (R./) sigma |> K

    -- TODO: It's worthwhile to investigate whether a precomputed distance
    -- matrix is faster, rather than computing the distance here.
    in map (\y_b -> map (inner y_b) y_hat |> mean) y_hat
}
