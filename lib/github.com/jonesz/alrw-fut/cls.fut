--| The transductive conformalized least squares implementation presented in ALRW (2.3.3 Basic Modifications).
import "../../diku-dk/linalg/linalg"
import "../../diku-dk/sorts/radix_sort"
import "cp"

local module type integral_req = {
  type t

  -- TODO: `integral` gives us `num_bits`/`get_bit`; perhaps we could loosen to `real`; of note
  -- these are only needed for `radix_sort` and we could use a sort based on `ordered_field` ops.
  include integral with t = t
  include ordered_field with t = t
}

local module type inner_cls = {
  type t

  val hat [n][p] : [n][p]t -> [1][p]t -> [n+1][n+1]t
  val C_from_hat [n] : [n][n]t -> [n][n]t

  val lower_idx : f32 -> i64 -> i64
  val upper_idx : f32 -> i64 -> i64
}

local module mk_inner_cls (T: integral_req) : inner_cls with t = T.t = {
  type t = T.t
  module L = mk_linalg T

  def hat X x =
      let p_X = X ++ x
      let m =
        L.matmul (transpose p_X) p_X                -- X'X
        |> L.inv                                    -- inv(X'X)
      in L.matmul (L.matmul p_X m) (transpose p_X)  -- X inv(X'X) X'

  def C_from_hat [n] H = 
    L.matsub (L.eye (n)) H            -- I - X inv(X'X) X'

  def lower_idx eps n =
    (eps / 2f32) * (f32.i64 n) |> f32.floor |> i64.f32          -- floor((eps/2)n)

  def upper_idx eps n =
    (1f32 - (eps / 2f32)) * (f32.i64 n) |> f32.ceil |> i64.f32  -- ceil((1f32-(eps/2))n)
}

--| 2.3.3 Two Modifications.
module mk_cls_deleted (T: integral_req) : cp = {
  type x [p] = [p]T.t
  type y     = [1]T.t
  type Y     = (T.t, T.t)
  type param = {}

  module L = mk_linalg T
  module CLS = mk_inner_cls T

  def predict [n] _param eps X Y x =
    let x = [x]

    let H = CLS.hat X x
    let h = L.fromdiag H
    let p_C = CLS.C_from_hat H

    let p_A = 
      let A = (L.matmul p_C (Y ++ [[T.i64 0i64]]) |> flatten) :> [n+1]T.t
      in map2 (\a_i h_i -> (T./) a_i ((T.-) (T.i64 1) h_i)) A h

    let p_B = 
      let B = ((transpose [L.veczeros n]) ++ [[T.i64 1i64]] |> L.matmul p_C |> flatten) :> [n+1]T.t
      in map2 (\b_i h_i -> (T./) b_i ((T.-) (T.i64 1) h_i)) B h

    -- TODO: We could parameterize this by conformity score...
    let conf idx =
      let r =
        let top = (T.-) p_A[idx] p_A[n] -- p_A[idx] - p_A[n]
        let bot = (T.-) p_B[n] p_B[idx] -- p_B[n] - p_B[idx]
        in (T./) top bot                -- top / bot

      let cond = (T.>) ((T.-) p_B[n] p_B[idx]) (T.i64 0i64) -- p_B[n] - p_B[idx] > 0
      in if cond then (r, r) else (T.lowest, T.highest)     -- l = -inf; u = inf

    let (l, u) =
      map (\idx -> conf idx) (iota n) |> unzip              -- l = -inf; u = inf

    let l = radix_sort_float T.num_bits T.get_bit l
    let u = radix_sort_float T.num_bits T.get_bit u

    let l_idx = CLS.lower_idx eps n
    let u_idx = CLS.upper_idx eps n

    in (l[l_idx], u[u_idx])
}

--| 2.3.3 Two Modifications.
module mk_cls_studentized (T: integral_req)
  : cp = {

  type x [p] = [p]T.t
  type y     = [1]T.t
  type Y     = (T.t, T.t)
  type param = {}

  module L = mk_linalg T
  module CLS = mk_inner_cls T

  def predict [n] _param eps X Y x =
    let x = [x]

    let H = CLS.hat X x
    let h = L.fromdiag H
    let p_C = CLS.C_from_hat H

    let p_A = 
      let A = (L.matmul p_C (Y ++ [[T.i64 0i64]]) |> flatten) :> [n+1]T.t
      in map2 (\a_i h_i -> (T./) a_i (T.sqrt ((T.-) (T.i64 1) h_i))) A h

    let p_B = 
      let B = ((transpose [L.veczeros n]) ++ [[T.i64 1i64]] |> L.matmul p_C |> flatten) :> [n+1]T.t
      in map2 (\b_i h_i -> (T./) b_i (T.sqrt ((T.-) (T.i64 1) h_i))) B h

    -- TODO: We could parameterize this by conformity score...
    let conf idx =
      let top = (T.-) p_A[idx] p_A[n] -- p_A[idx] - p_A[n]
      let bot = (T.-) p_B[n] p_B[idx] -- p_B[n] - p_B[idx]
      in (T./) top bot                -- top / bot

    let t =
      map (\idx -> conf idx) (iota n)

    let t = radix_sort_float T.num_bits T.get_bit t

    let l_idx = CLS.lower_idx eps n
    let u_idx = CLS.upper_idx eps n

    in (t[l_idx], t[u_idx])
}
