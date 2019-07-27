(* Calculator The Game - Solver *)

type num_repr = bool * string (* true for positive, false for negative *)
type operation =
    Add of int | Sub of int | Mul of int | Div of int | Square | Cube
  | Del | MulNeg | Append of int | Replace of string * string | Shift of int
  | Reverse | Sum | Inv10 | Mirror | MetaInc of int | StoreSave | StoreUse
type state = int * int option * operation list

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

exception StopSearch (* raised when a search branch is impossible/useless *)

let disp_op = function
  | Add n -> "+" ^ string_of_int n
  | Sub n -> "-" ^ string_of_int n
  | Mul n -> "×" ^ string_of_int n
  | Div n -> "/" ^ string_of_int n
  | Square -> "x^2"
  | Cube -> "x^3"
  | MulNeg -> "+/-"
  | Del   -> "<<"
  | Append n -> string_of_int n
  | Replace (a, b) ->  a ^ "=>" ^ b
  | Shift d -> "shift" ^ (if d = 1 then ">" else "<")
  | Reverse -> "rev"
  | Sum -> "sum"
  | Inv10 -> "Inv10"
  | Mirror -> "mirror"
  | MetaInc n -> "[+]" ^ string_of_int n
  | StoreSave -> "store (s)"
  | StoreUse -> "store (u)"

(* -- Button operations *)
let apply_neg x op = -(op (-x))

let rec del_last x =
  let str_x = string_of_int x in
  if x < 0 then apply_neg x del_last
  else if x < 10 then 0
  else int_of_string (String.sub str_x 0 (String.length str_x - 1))

let rec num_to_digits x =
  let rec aux x =
    if x < 10 then [x]
    else x mod 10 :: aux (x / 10) in
  List.rev (aux x)

let digits_to_num l = List.fold_left (fun a b -> 10 * a + b ) 0 l

let append_num x a = int_of_string (string_of_int x ^ string_of_int a)

let rec sum x =
  if x < 0 then apply_neg x sum
  else x |> num_to_digits |> List.fold_left (+) 0

let rec inv10 x =
  if x < 0 then apply_neg x inv10
  else x |> num_to_digits
       |> List.map (fun x -> if x = 0 then 0 else 10 - x)
       |> digits_to_num

(* very stupid & not efficient *)
let rec num_replace x a b =
  let rec str_replace left right pat rep =
    let pat_l = String.length pat in
    let r_l = String.length right in
    if r_l < pat_l then left ^ right
    else if pat = String.sub right 0 pat_l
    then str_replace (left ^ rep) (String.sub right pat_l (r_l - pat_l)) pat rep
    else str_replace (left ^ String.sub right 0 1) (String.sub right 1 (r_l - 1)) pat rep in
  if x < 0 then apply_neg x (fun x -> num_replace x a b) else  (*suboptimalbutwhatever*)
  int_of_string (str_replace "" (string_of_int x) a b)

let rec reverse x =
  if x < 0 then apply_neg x reverse
  else x |> num_to_digits |> List.rev |> digits_to_num

let rec shift x direction =
  if x < 0 then apply_neg x (fun x -> shift x direction)
  else
    let rec shift_arr direction' xs =
      match (direction', xs) with
      | (1, ds) -> List.rev (shift_arr (-1) (List.rev ds))
      | (-1, d::ds) -> ds @ [d]
      | (_, _) -> raise (Invalid_argument "[direction] must be -1 (left) or 1 (right)") in
    x |> num_to_digits |> shift_arr direction |> digits_to_num

let rec mirror x =
  if x < 0 then apply_neg x mirror
  else digits_to_num (num_to_digits x @ (List.rev (num_to_digits x)))

(* Applying operations and solving *)

let apply_op (curr_num, store_state, op_applied) op =
  let aux a = function
    | Add b -> a + b
    | Sub b -> a - b
    | Mul b -> a * b
    | Div b -> if a mod b = 0 then a / b else raise StopSearch (* no need to divide the undivisible *)
    | Square -> a * a
    | Cube -> a * a * a
    | MulNeg -> -a
    | Del   -> del_last a
    | Append b -> append_num a b
    | Replace (b, c) -> num_replace a b c
    | Shift d -> shift a d
    | Reverse -> reverse a
    | Sum -> sum a
    | Inv10 -> inv10 a
    | Mirror -> mirror a
    | MetaInc n -> a (* stateful ops are handled in solve *)
    | StoreSave -> a
    | StoreUse -> match store_state with
      | Some i -> append_num a i
      | None -> raise StopSearch in
  match op with
  | StoreSave ->
    if curr_num < 0 then raise StopSearch (* weird behaviour with negatives *)
    else (curr_num, Some curr_num, op_applied @ [StoreSave])
  | _ -> (aux curr_num op, store_state, op_applied @ [op])

let inc_buttons i = List.map (function
    | Add a -> Add (a + i)
    | Sub a -> Sub (a + i)
    | Mul a -> Mul (a + i)
    | Div a -> Div (a + i)
    | Append a -> Append (a + i)
    | op -> op)

(* out_ind > in_ind *)
let rec portal in_ind out_ind x =
  if x |> string_of_int |> String.length <= out_ind then x
  else
    let rec apply_portal n a b = match (n, b) with
      | (1, bh::bt) -> (bh, a @ bt)
      | (n, bh::bt) -> apply_portal (n - 1) (a @ [bh]) bt
      | (n, []) -> raise (Failure "This should never happen") in
    let (digit, root) =
      apply_portal ((String.length (string_of_int x)) - out_ind) [] (num_to_digits x) in
    portal in_ind out_ind ((digits_to_num root) + (digit * (pow 10 in_ind)))


let solve start goal moves buttons portal =
  let rec solve_aux (curr_num, store_state, op_applied) moves_left current_buttons history =
    if (abs curr_num) |> string_of_int |> String.length > 6 then (raise StopSearch)
    else if curr_num = goal then op_applied (* reach early *)
    else if moves_left = 0 then []
    else if List.mem (curr_num, store_state, current_buttons) history then raise StopSearch
    else
      let portal_func = match portal with
        | None -> (fun x -> x)
        | Some f -> (fun (x, b, c) -> (f x, b, c)) in
      let map_func button = try
          match button with
          | MetaInc n -> solve_aux
                           (curr_num, store_state, op_applied @ [button])
                           (moves_left - 1) (inc_buttons n current_buttons) ((curr_num, store_state, current_buttons)::history)
          | op -> solve_aux
                    (portal_func (apply_op (curr_num, store_state, op_applied) button))
                    (moves_left - 1) current_buttons ((curr_num, store_state, current_buttons)::history)
        with StopSearch -> [] in
      let merge a b =
        if a <> [] && b <> [] then
          if List.length a < List.length b then a else b
        else a @ b in
      List.fold_left merge [] (List.map map_func current_buttons) in
  solve_aux (start, None, []) moves buttons []

let pretty_result l = (List.map disp_op l) |> String.concat " ➔ "
let solution a b c d e = (solve a b c d e) |> pretty_result |> print_endline

