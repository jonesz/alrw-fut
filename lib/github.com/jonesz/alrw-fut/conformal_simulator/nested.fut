import "../../../diku-dk/sorts/merge_sort"
import "../../../diku-dk/linalg/linalg"
import "kde"

module mk_nested(R: real) = {
	--| radius.
	type r = R.t
	module KDE = mk_kde(R)
	def radius = ???

	def seq [B] 't (d_y: t -> t -> r) (sigma: r) (sim_hat: [B]t) (y_l: [B]t) : [B](t, r) =
		map (KDE.lambda_i_b d_y sigma y_l) sim_hat -- Compute lambda_i for each simulation.
		|> zip sim_hat                             -- Zip them with the original y_i
		|> merge_sort_by_key (.1) (R.<=)           -- Sort them in ascending value.
		|> map (\a -> (a.0, radius))

	def cs 't [B] (d_y: t -> t -> r) y (lambda_y_i: [B]t) : i64 =
		let contains y_sim =
			d_y y y_sim |> (R.<=) radius

		let tmp = map (contains) lambda_y_i
		in if or tmp
			then zip tmp (iota B) |> filter (.0) |> last |> (.1)
			else 1i64 |> i64.neg
}
