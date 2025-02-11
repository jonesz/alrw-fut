module mk_pkde(R: real) = {
	type t = R.t

	def K u = -- (3.1) ~ exp(-1 * u^2)
		(R.**) u (R.i64 2i64) |> R.neg |> R.exp

	-- Rank the members of `sim_hat(x_i, B)` via a GKPDE.
	def rank 'a [B] dy y sigma =
		let inner y_b y_i = -- (3.1) ~ K(dy(y_b, y_i) / sigma)
			dy y_b y_i |> (R.*) (R.recip sigma) |> K
		in map (\y_b -> map (y_i -> inner y_b y_i |> reduce (T.+) (T.i64 0) |> (R.*) (R.recip B) y) y
}
