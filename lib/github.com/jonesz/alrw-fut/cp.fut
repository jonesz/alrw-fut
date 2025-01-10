-- TODO: Is this truly the transductive case?
--| The default transductive conformal predictor.
module type cp = {
	--| `x` is an example with `p` features.
	type x [p]

	-- TODO: The above has features, but this doesn't? Perhaps we need a module that's 
	-- specific for a "cp_linear_regression" algorithm and maps `p` features to a single output.
	--| 'y' is an output.
	type y

	--| The output, prediction set type of the conformal predictor.
	type Y

	--| The parameters for a specific algorithm.
	type param

	val predict [n][p] : param -> f32 -> [n]x[p] -> [n]y -> x[p] -> Y
}
