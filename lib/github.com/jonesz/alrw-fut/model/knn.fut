import "../../../diku-dk/sorts/merge_sort"

module mk_knn(T: numeric) = {
	type t = T.t

	def knn (k: i64) metric (F: []([]t, t)) x =
		let (F_x, F_y) = unzip F
		in map (metric x) F_x                   -- d(x, F_x_i)
			|> zip F_y                          -- (F_y_i, d(x, F_x_i))
			|> merge_sort_by_key (.1) (T.<=)    -- above, but ordered.
			|> take k                           -- [5](t, R)
			|> map (.0)                         -- [5]t
			|> reduce (T.+) (T.i64 0)           
			|> flip (T./) (T.i64 k)             -- 1/k * sum([5]t)
}
