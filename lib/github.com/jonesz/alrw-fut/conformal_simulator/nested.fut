import "../../../diku-dk/sorts/merge_sort"
import "../../../diku-dk/linalg/linalg"
import "pkde"

module mk_nested (R: real) = {
  type r = R.t
  type ball = #none | #radius r

  def radius = ???

  -- Compute the sequence of nested sets.
  def sequence 'a [B] (lambda_b: [B]r) (y_hat: [B]a) : ([B]a, [B][B]ball) =
    let (lambda_b_sorted, y_hat_sorted) = zip lambda_b y_hat |> merge_sort_by_key (.0) (R.<=) |> unzip
    -- TODO: 1) There's a far more efficient manner to store this data.
    --       2) There's likely an indexing error here `iota` vs 1-starting-index...

    in ( y_hat_sorted
       , map (\lambda_t ->
                -- the `t`'th order statistic...
                map (\lambda_i ->
                       if (R.>=) lambda_i lambda_t
                       then #radius radius
                       else #none)
                    lambda_b_sorted)
             lambda_b_sorted
       )

  def cs 'a [B] (dy: a -> a -> r) y ((y_hat, nested_sets): ([B]a, [B][B]ball)) =
    -- `y` is contained within the ball if the distance is <= to the ball's radius.
    let y_in_ball b r = dy b y |> (R.>=) r

    -- For all of the nested sets, compute whether each `y_hat` contains `y`.
    let (max_f, _) =
      map (\set ->
             map2 (\b_i r_i ->
                     match r_i
                     case #none -> false
                     case #radius rad -> y_in_ball b_i rad)
                  y_hat
                  set
             |> or)
          nested_sets

      -- To find the maximum index, zip the `bool`s with the index, then filter them,
      -- and take the last th remains
      |> zip (iota B)
      |> filter (.1)
      |> last                   -- TODO: What if the above filter returns nothing?

    in max_f
}
