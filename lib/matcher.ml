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
let rec matchesStartLeavingValidRemainder (startsWith: re) (input: string) (validRemainder: string -> bool) =
  let n = String.length input in
  match (startsWith, input) with
  | (Empty, _) -> validRemainder input
  | (Char c, _) when n > 0 && String.get input 0 = c -> validRemainder (String.sub input 1 (n - 1))
  | (Seq (first, second), _) -> matchesStartLeavingValidRemainder first input (fun rest -> matchesStartLeavingValidRemainder second rest validRemainder)
  | (Alt (a, b), _) -> matchesStartLeavingValidRemainder a input validRemainder || matchesStartLeavingValidRemainder b input validRemainder
  | (Star p, x) -> validRemainder input || matchesStartLeavingValidRemainder p input (fun rest -> String.length rest < n && matchesStartLeavingValidRemainder (Star p) rest validRemainder)
  | _ -> false

let matches r s = matchesStartLeavingValidRemainder r s (fun rest -> rest = "")

(* only the words that pattern matches *)
let keep_matches pattern words = 
  List.filter (fun word -> matches pattern word) words

(* each word paired with whether pattern matches it *)
let label_matches pattern words = 
  List.map (fun word -> (word, matches pattern word)) words

(* the total length of the words pattern matches, using List.fold_left *)
let total_match_length pattern words = 
  List.fold_left (fun acc word -> if matches pattern word then acc + String.length word else acc) 0 words