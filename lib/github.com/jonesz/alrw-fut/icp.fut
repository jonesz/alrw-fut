module type icp = {
	--| `x` is an example with `p` features.
	type x [p]

	--| `y` is an output.
	type y

	--| The output, prediction set type of the conformal predictor.
	type Y

	--| The parameters for a specific algorithm.
	type param

	--| The output after calling 'fit'.
	type fit

	val fit [l][p] : param -> [l]x[p] -> [l]y -> fit

	val predict [m][p] : param -> f32 -> [m]x[p] -> [m]y -> x[p] -> Y
}
