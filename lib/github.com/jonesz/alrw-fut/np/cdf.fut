def np_cdf alpha h x =
	-- By the DKW ineq, \eps^2 = log(2/alpha)/(2n).
	let eps = (log (2/a)) / (2 * n) |> sqrt
	let lb = max (h[x] - eps) 0
	let ub = min (h[x] + eps) 1
	in (lb, ub)
