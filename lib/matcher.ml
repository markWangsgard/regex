(* type re =
  | Empty                 (* matches the empty string, and nothing else *)
  | Char of char          (* matches exactly that one character *)
  | Seq of re * re        (* matches the first, then the second *)
  | Alt of re * re        (* matches either one *)
  | Star of re            (* matches the inner pattern zero or more times *)

  let rec matches re s = 
    match (re, s) with
    | (Empty, "") -> true
    | (Star re, "") -> true
    | (Char ch, s) when String.length s = 1 && String.get s 0 = ch -> true
    | (Alt (a, b), s) -> matches a s || matches b s
    | (Star re, s) -> true
    | _ -> false *)




    type re =
  | Empty                 (* matches the empty string, and nothing else *)
  | Char of char          (* matches exactly that one character *)
  | Seq of re * re        (* matches the first, then the second *)
  | Alt of re * re        (* matches either one *)
  | Star of re            (* matches the inner pattern zero or more times *)

(* [go r input k] matches some prefix of [input] against [r], then calls
   [k] on whatever input is left over.  It is true when some way of
   matching makes [k] return true. *)
let rec go (r : re) (input : char list) (k : char list -> bool) : bool =
  match r with
  | Empty -> k input
  | Star _ when input = [] -> true
  | Star Empty -> false
  | Char c -> (
      match input with
      | x :: rest when x = c -> k rest
      | _ -> false)
  | Seq (a, b) -> go a input (fun rest -> go b rest k)
  | Alt (a, b) -> go a input k || go b input k
  | Star a -> go a input (fun rest -> go r rest k)

let matches (r : re) (s : string) : bool =
  let input = List.init (String.length s) (String.get s) in
  go r input (fun rest -> rest = [])