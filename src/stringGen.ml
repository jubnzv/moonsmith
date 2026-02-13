open Core

let wordlist =
  [ "Lorem"; "ipsum"; "dolor"; "sit"; "amet"; "consectetur";
    "adipiscing"; "elit"; "sed"; "do"; "eiusmod"; "tempor"; "incididunt"; "ut";
    "labore"; "et"; "dolore"; "magna"; "aliqua"; "Ut"; "enim"; "ad"; "minim";
    "veniam"; "quis"; "nostrud"; "exercitation"; "ullamco"; "laboris"; "nisi";
    "ut"; "aliquip"; "ex"; "ea"; "commodo"; "consequat"; "Duis"; "aute"; "irure";
    "dolor"; "in"; "reprehenderit"; "in"; "voluptate"; "velit"; "esse"; "cillum";
    "dolore"; "eu"; "fugiat"; "nulla"; "pariatur"; "Excepteur"; "sint"; "occaecat";
    "cupidatat"; "non"; "proident"; "sunt"; "in"; "culpa"; "qui"; "officia";
    "deserunt"; "mollit"; "anim"; "id"; "est"; "laborum"; ]

let gen_string () =
  let len = List.length wordlist in
  let rec aux acc words_left =
    if words_left >= List.length acc then
      let rand_idx = Random.int_incl 0 (len - 1) in
      aux (acc @ [List.nth_exn wordlist rand_idx]) (( - ) words_left 1)
    else
      acc
  in
  let num_words = Random.int_incl 1 (len / 10) in
  aux [] num_words |> String.concat ~sep:" "

let gen_int_string () =
  string_of_int @@ Random.int_incl 1 10

(* See:https://www.lua.org/pil/20.2.html *)
let regex_letters = [ "a"; "c"; "d"; "l"; "p"; "s"; "u"; "w"; "x"; "z" ]
and regex_modifiers = [ "+"; "*"; "-"; "?" ]

let gen_regexp_string () =
  let patterns_num = Random.int_incl 1 3 in
  let rec aux acc =
    if patterns_num > List.length acc then
      let regexp =
        [ "%";
          Util.choose_one_exn regex_letters;
          match Random.bool () with
          | true  -> Util.choose_one_exn regex_modifiers
          | false -> "" ]
        |> String.concat ~sep:""
      in aux (acc @ [regexp])
    else acc
  in
  aux [] |> String.concat ~sep:""
