(* Pull NFL historical data available *)

open Lwt.Infix
open Types
open Utils

let (>>) = fun x y -> (x >>= fun _ -> y)

(* Read all lines of a csv *)
let rec read_all ?(result = []) ic () =
  let open Lwt_io in
  try_lwt
    lwt new_line = read_line ic in
    read_all ~result:(new_line :: result) ic ()
  with
  | End_of_file -> close ic >|= fun () -> result

let players_from_csv () =
  let path = "players.csv" in
  lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input path in
  lwt csv_data = read_all ic () in
  Lwt.return @@ List.map player_of_string csv_data

let random_roster () = () (* Build a random roster *)

(* Fetch raw html from a webpage *)
let get_html addr =
  let addr_uri = Uri.of_string addr in
  Cohttp_lwt_unix.Client.get addr_uri
  >>= fun (a, b) -> Cohttp_lwt_body.to_string b

(* Fetch raw html for a specific player and season *)
let player_html ?season player =
  let player_url =
    "http://www.nfl.com/player/" ^
    player.first_name ^ player.last_name ^ "/" ^
    (string_of_int player.id) ^ "/gamelogs" ^
    (
      match season with
      | None -> ""
      | Some n -> "?season=" ^ (string_of_int n)
    )
  in
  get_html player_url

(* Get all available seasons for a player *)
let get_player_seasons html =
  let clean_string = Str.global_replace (Str.regexp "[\r\t\n ]") "" html in
  let delim_list = Str.full_split (Str.regexp "<optionvalue=\"[0-9]+") clean_string in
  let token_list =
    List.filter (
      fun sr ->
          match sr with
          | Str.Delim _ -> true
          | _ -> false
    ) delim_list
  in
  let years =
    List.map (
      fun sr ->
        match sr with
        | Str.Delim s -> Str.replace_first (Str.regexp "<optionvalue=\"") "" s |> int_of_string
        | _ -> raise (Failure "ERROR: get_player_seasons - expected Str.Delim type!")
    ) token_list
  in
  let sorted_years = List.sort (fun a b -> if a <= b then -1 else 1) years in
  Lwt.return sorted_years

let count_start_elements (l : Markup.signal list) =
  List.map (
    fun ms ->
      match ms with
      | `Start_element _ -> 1
      | _ -> 0
  ) l
  |> (List.fold_left (+) 0)

let count_end_elements (l : Markup.signal list) =
  List.map (
    fun ms ->
      match ms with
      | `End_element -> 1
      | _ -> 0
  ) l
  |> (List.fold_left (+) 0)

(* Given a list of dom elements, truncate the list after the first tag has been matched *)
let rec truncate_dom_list ?(accum = []) (l : Markup.signal list) =
  match l with
  | [] -> List.rev accum
  | hd :: tl ->
    let start_elems = count_start_elements accum in
    let end_elems = count_end_elements accum in
    if (start_elems <> 0) && (start_elems = end_elems)
    then List.rev accum
    else
      match hd with
      | `Start_element _ ->
        if start_elems + 1 = end_elems
        then List.rev (hd :: accum)
        else truncate_dom_list ~accum:(hd :: accum) tl
      | `End_element ->
        if start_elems = end_elems + 1
        then List.rev (hd :: accum)
        else truncate_dom_list ~accum:(hd :: accum) tl
      | _ -> truncate_dom_list ~accum:(hd :: accum) tl

let markup_of_html html =
  let open Markup in
  string html |> parse_html |> signals |> to_list

(* Eliminate everyting to the left of the tag *)
let html_drop_left ~tag html =
  let clean_html = Str.global_replace (Str.regexp "[\r\t\n\ ]+") " " html in
  let start_loc = Str.search_forward (Str.regexp tag) clean_html 0 in
  String.sub clean_html start_loc ((String.length clean_html) - start_loc)

(* Select a specific section of html inbetween tags ~start_tag and ~end_tag, including tags *)
let html_substring ~tag html =
  html_drop_left ~tag html |> markup_of_html |> truncate_dom_list

(* Get the first games table that appears in an html string *)
let games_table html =
  html_drop_left ~tag:"<table class='data-table1'" html

let get_game_type html =
  markup_of_html html
  |> fun l ->
  let g =
    try
      List.nth l 2
    with
      Failure "nth" -> raise (Failure "ERROR: get_game_type - List.nth")
  in
  match g with
  | `Text [t] -> game_type_of_string t
  | _ -> raise (Failure "ERROR: get_game_type - received unexpected html!")

  let clean_html html =
    Str.global_replace (Str.regexp "[\r\t\n\ ]+") " " html

(* Split html into sections containing season games, i.e. Pre Season, Regular Season, etc... *)
let split_season ?(keep_delims = false) ?(combine_delims = false) ~tag html =
  let delim_list = clean_html html |> Str.full_split (Str.regexp tag) in
  (* Remove all data to the left of the first Str.Delim *)
  let delim_list2 =
    match delim_list with
    | [] -> []
    | Str.Delim _ :: tl -> delim_list
    | _ :: tl -> tl
  in
  (* Remove the last element if it is a delim *)
  let delim_list3 =
    match List.rev delim_list2 with
    | [] -> []
    | Str.Delim _ :: tl -> List.rev tl
    | _ :: tl -> delim_list2
  in
  let rec clean_list ?(accum = []) l =
    match l with
    | [] -> List.rev accum
    | Str.Delim d :: tl ->
      if keep_delims
      then clean_list ~accum:(d :: accum) tl
      else clean_list ~accum tl
    | Str.Text t :: tl -> clean_list ~accum:(clean_html t :: accum) tl
  in
  (* Re-concatenate the delimiters and the text data *)
  let rec concat_html ?(acc = []) ?(delim = "") sl =
    match sl, delim with
    | [], _ -> List.rev acc
    | hd :: tl, "" -> concat_html ~acc ~delim:hd tl
    | hd :: tl, _ -> concat_html ~acc:((delim ^ hd) :: acc) tl
  in
  if combine_delims
  then concat_html @@ clean_list delim_list3
  else clean_list delim_list3

(* Get the headers from a games table *)
let get_headers games_table_html =
  let filter_f x =
      match x with
      | `Text [" "] -> false
      | `Text _ -> true
      | _ -> false
  in
  let map_f x =
    match x with
    | `Text [s] -> s
    | _ -> raise (Failure "ERROR: Unexpected Polymorphic Variance in get_headers.")
  in
  html_drop_left ~tag:"<tr class=\"player-table-key\">" games_table_html
  |> markup_of_html
  |> truncate_dom_list
  |> filter_map ~filter_f ~map_f
  |> fun sl -> Headers sl

let rec split_game_list ?(games = []) ?(game = []) gl =
  match gl, game with
  | [], _ -> List.rev games
  | `Text [s] :: tl, _ -> split_game_list ~games ~game:(s :: game) tl
  | _ :: tl, [] -> split_game_list ~games ~game:[] tl
  | _ :: tl, _ -> split_game_list ~games:((List.rev game) :: games) ~game:[] tl

let clean_game_data ~year game =
  (* Replace "--" with "0" *)
  let arr =
    Array.map (
      fun s ->
        match s with
        | "--" -> "0"
        | _ -> s
    ) (Array.of_list game)
  in
  (* Fix the Date into YYYY-MM-DD format *)
  let l = if arr.(1) = "Bye" then ["0"; "0"] else Str.split (Str.regexp "/") arr.(1) in
  arr.(1) <- ((string_of_int year) ^ "-" ^ (List.hd l) ^ "-" ^ (List.nth l 1));
  (* Fixup the opponet *)
  arr.(2) <- Str.global_replace (Str.regexp "[ @]+") "" arr.(2);
  (* Remove spaces from the score *)
  let arr_cln = Array.map (fun s -> Str.global_replace (Str.regexp "[ ]+") "" s) arr in
  (* Drop items 4 and drop the W/L, this can be obtained from the score *)
  Array.to_list @@
  Array.concat [Array.sub arr_cln 0 3; Array.sub arr_cln 5 (Array.length arr_cln - 5)]

(* Get the data from a games table *)
let get_games_data ~year ~game_type games_table_html =
  html_drop_left ~tag:"<tbody>" games_table_html
  |> markup_of_html
  |> truncate_dom_list
  (* Only keep <tr> tags to separate the list into games and the `Text data *)
  |> List.filter (
    fun x ->
      match x with
      | `Start_element ((_, "tr"), _) -> true
      | `Text [" "] -> false
      | `Text _ -> true
      | _ -> false
  )
  |> split_game_list
  |> List.map (clean_game_data ~year)
  |> fun sll ->
    List.map (
      fun sl ->
          try
            GameData (player_stat_of_string_list ~game_type sl)
          with
            _ -> DataRowError (String.concat "," sl)
        ) sll

(* TODO: Handle Bye games and "Total" rows better *)
(* Pull player stats for a specific season *)
let stats_in_season ?(write_headers = false) ~year player =
  let url =
    "http://www.nfl.com/player/" ^
    player.first_name ^ player.last_name ^ "/" ^ (string_of_int player.id) ^
    "/gamelogs?season=" ^ (string_of_int year)
  in
  lwt html = get_html url in
  (* html tr class is based on the team name *)
  let tr_tag =
    "<tr class=\"" ^ (string_of_team_name player.team) ^ "colors player-table-header\">"
  in
  let game_types =
    split_season ~tag:tr_tag html
    |> List.map get_game_type
  in
  let g_tbl =
    split_season ~keep_delims:true ~combine_delims:true ~tag:"<table class='data-table1'" html
    |> List.map games_table
  in
  let headers = get_headers @@ List.hd g_tbl in
  let data_rows =
    try
      List.map2 (fun gt sl -> get_games_data ~year ~game_type:gt sl) game_types g_tbl
    with
      Invalid_argument _ ->
        [[DataRowError ("ERROR: stats_in_season - player #" ^ (string_of_int player.id))]]
  in
  Lwt.return (if write_headers then (headers :: List.flatten data_rows) else List.flatten data_rows)

(* Fetch & write the historical data for a player *)
let save_player_data ~oc player =
  Lwt_io.print (player.first_name ^ " "  ^ player.last_name ^ "\n") >>
  lwt html = player_html player in
  lwt years = get_player_seasons html in
  lwt player_data =
    Lwt_list.map_s (fun y -> stats_in_season ~year:y player) years
    >|= List.flatten
    >|= List.map string_of_data_row
  in
  let plr_str = string_of_player player in
  let player_and_data = List.map (fun x -> plr_str ^ "," ^ x) player_data in
  Lwt_list.iter_s (Lwt_io.write_line oc) player_and_data

let write_headers oc =
  Lwt_io.write_line oc
    (
      "FirstName,LastName,Height,Weight,Team,Status,Salary,Position,Rookie,ID," ^
      "GameType,Week,GameDate,Opponet,GameResult,PlayedInGame,Starter," ^
      "PassingComp,PassingAtt,PassingPct,PassingYds,PassingAvg,PassingTD," ^
      "PassingInt,PassingSck,PassingScky,PassingRate,RushingAtt,RushingYds," ^
      "RushingAvg,RushingTD,Fumbles,FumblesLost"
    )

let main () =
  let open Lwt_unix in
  lwt oc =
    Lwt_io.open_file
      ~flags:[O_WRONLY; O_CREAT; O_TRUNC]
      ~mode:Lwt_io.output
      "all_player_data.csv"
  in
  try_lwt
    write_headers oc >>
    lwt players = players_from_csv () in
    Lwt_list.iter_s (save_player_data ~oc) players
    >>= fun () -> Lwt_io.close oc
  with
    exn as e -> Lwt_io.close oc >> raise e

module Test = struct

let test_player = {
  first_name = "Trent";
  last_name = "Williams";
  height = 0;
  weight = 0;
  team = Redskins;
  status = Active;
  salary = 0;
  position = Unknown "T";
  rookie = false;
  id = 497073
}

let test () =
  player_html ~season:(2009) test_player
  >>= fun y -> stats_in_season ~year:2009 test_player

end
