(* Pull NFL historical data available *)

#require "lwt.syntax";;
#require "lwt";;
#require "cohttp.lwt";;
#require "uri";;
#require "str";;
#require "markup";;

(* More data available on Yahoo Sports *)

type status = Active | InActive | StatusError of string

type game_type = PreSeason | RegularSeason | PostSeason | GameTypeError of string

let game_type_of_string s =
  match String.uppercase s with
  | "PRESEASON" -> PreSeason
  | "REGULAR SEASON" -> RegularSeason
  | "POSTSEASON" -> PostSeason
  | _ -> GameTypeError s

type position = QB | RB | WR | TE | DST | Unknown of string

type game_result = Win of int * int
                 | Loss of int * int
                 | Tie of int * int

let game_result_of_string s =
  let [a; b] = Str.split (Str.regexp "-") s in
  let s1 = int_of_string a in
  let s2 = int_of_string b in
  if s1 > s2
  then Win (s1, s2)
  else (
    if s1 < s2
    then Loss (s1, s2)
    else Tie (s1, s2)
  )

type team_name = Ravens
                | Bengals
                | Browns
                | Steelers
                | Texans
                | Colts
                | Jaguars
                | Titans
                | Bills
                | Dolphins
                | Patriots
                | Jets
                | Broncos
                | Chiefs
                | Raiders
                | Chargers
                | Bears
                | Lions
                | Packers
                | Vikings
                | Falcons
                | Panthers
                | Saints
                | Buccaneers
                | Cowboys
                | Giants
                | Eagles
                | Redskins
                | Cardinals
                | Rams
                | SF49ers
                | Seahawks
                | TeamNameError of string

type player = {
  first_name : string;
  last_name : string;
  height : int;
  weight : int;
  team : team_name;
  status : status;
  salary : int;
  position : position;
  rookie : bool;
  id : int
}

type team = {
  name : string;
  city : string;
  players : player list;
  conference : string
}

type game_date = {year : int; month : int; day : int}


let game_date_of_string s =
  let [year; month; day] = Str.split (Str.regexp "-") s in
  {
    year = int_of_string year;
    month = int_of_string month;
    day = int_of_string day
  }

(* Player stats for a single game *)
type player_stat = {
  game_type      : game_type;
  week           : int;
  game_date      : game_date;
  opponet        : team_name;
  game_result    : game_result;
  played_in_game : bool;
  starter        : bool;
  passing_comp   : int;
  passing_att    : int;
  passing_pct    : float;
  passing_yds    : float;
  passing_avg    : float;
  passing_td     : float;
  passing_int    : float;
  passing_sck    : float;
  passing_scky   : float;
  passing_rate   : float;
  rushing_att    : int;
  rushing_yds    : float;
  rushing_avg    : float;
  rushing_td     : int;
  fumbles        : int;
  fumbles_lost   : int
}

type data_row = Headers of string list | GameData of player_stat | DataRowError of string

let position_of_string s =
  match String.uppercase s with
  | "QB" -> QB
  | "RB" -> RB
  | "WR" -> WR
  | "TE" -> TE
  | "DT" -> DST
  | _    -> Unknown s

let status_of_string s =
  match String.lowercase s with
  | "active"   -> Active
  | "inactive" -> InActive
  | _ as s -> StatusError s

let rec filter_map ?(accum = []) ~filter_f ~map_f l =
  match l with
  | [] -> List.rev accum
  | hd :: tl ->
    if filter_f hd
    then filter_map ~accum:((map_f hd) :: accum) ~filter_f ~map_f tl
    else filter_map ~accum ~filter_f ~map_f tl

module NFL = struct

  open Lwt.Infix

  type conference = AFC_North
                  | AFC_South
                  | AFC_East
                  | AFC_West
                  | NFC_North
                  | NFC_South
                  | NFC_East
                  | NFC_West

  let team_name_of_string s =
    match s with
    | "Baltimore Ravens"     | "BAL" -> Ravens
    | "Cincinati Bengals"    | "CIN" -> Bengals
    | "Cleveland Browns"     | "CLE" -> Browns
    | "Pittsburgh Steelers"  | "PIT" -> Steelers
    | "Houston Texans"       | "HOU" -> Texans
    | "Indianapolis Colts"   | "IND" -> Colts
    | "Jacksonville Jaguars" | "JAC" -> Jaguars
    | "Tennessee Titans"     | "TEN" -> Titans
    | "Buffalo Bills"        | "BUF" -> Bills
    | "Miami Dolphins"       | "MIA" -> Dolphins
    | "New England Patriots" | "NE"  -> Patriots
    | "New York Jets"        | "NYJ" -> Jets
    | "Denver Broncos"       | "DEN" -> Broncos
    | "Kansas City Chiefs"   | "KC"  -> Chiefs
    | "Okland Raiders"       | "OAK" -> Raiders
    | "San Diego Chargers"   | "SAN" -> Chargers
    | "Chicago Bears"        | "CHI" -> Bears
    | "Detroit Lions"        | "DET" -> Lions
    | "Green Bay Packers"    | "GB"  -> Packers
    | "Minnesota Vikings"    | "MIN" -> Vikings
    | "Atlanta Falcons"      | "ATL" -> Falcons
    | "Carolina Panthers"    | "CAR" -> Panthers
    | "New Orleans Saints"   | "NO"  -> Saints
    | "Tampa Bay Buccaneers" | "TB"  -> Buccaneers
    | "Dallas Cowboys"       | "DAL" -> Cowboys
    | "New York Giants"      | "NYG" -> Giants
    | "Philadelphia Eagles"  | "PHI" -> Eagles
    | "Washington Redskins"  | "WAS" -> Redskins
    | "Arizona Cardinals"    | "ARI" -> Cardinals
    | "Los Angeles Rams"     | "STL" -> Rams
    | "San Francisco 49ers"  | "SF"  -> SF49ers
    | "Seattle Seahawks"     | "SEA" -> Seahawks
    | _                              -> TeamNameError s

  type roster = {
    player_1 : player;
    player_2 : player;
    player_3 : player;
    player_4 : player;
    player_5 : player;
    player_6 : player;
    player_7 : player;
    player_8 : player;
    player_9 : player
  }

  let salary_cap = 50_000

  (* Read all lines of a csv *)
  let rec read_all ?(result = []) ic () =
    let open Lwt_io in
    try_lwt
      lwt new_line = read_line ic in
      read_all ~result:(new_line :: result) ic ()
    with
    | End_of_file -> close ic >|= fun () -> result

  (* Get a player from a row out of the csv file *)
  let player_from_row row =
    let player_data = Str.split (Str.regexp "[,]") row in
    {
      first_name = List.nth player_data 1;
      last_name = List.nth player_data 0;
      height = int_of_string @@ List.nth player_data 3;
      weight = int_of_string @@ List.nth player_data 4;
      team = team_name_of_string @@ List.nth player_data 6;
      status = Active; (* TODO: Make this work *)
      salary = 0; (* TODO: Make this work *)
      position = position_of_string @@ List.nth player_data 2;
      rookie = false; (* TODO: Make this work *)
      id = int_of_string @@ List.nth player_data 7
    }

  let players_from_csv () =
    let path = "players.csv" in
    lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input path in
    lwt csv_data = read_all ic () in
    Lwt.return @@ List.map player_from_row csv_data

  (*
     TODO: Check that a roster is valid.
     Check for draft king rules like:
     Must include players from at least 2 different teams and two different games
  *)

  let random_roster () = () (* Build a random roster *)

  (* TODO: Get all data for a single player *)
  (* TODO: Estimate a players score based on their performance in the last game *)
  (* TODO: Others ways to get a playres expected score, conditional upon other information like defense, points last game, age, etc... *)

  (* Fetch raw html from a webpage *)
  let get_html addr =
    let addr_uri = Uri.of_string addr in
    Cohttp_lwt_unix.Client.get addr_uri
    >>= fun (a, b) -> Cohttp_lwt_body.to_string b

  (* Fetch raw html for a specific player and season *)
  let player_html ~player ~season () =
    let player_url =
      "http://www.nfl.com/player/" ^
      player.first_name ^ player.last_name ^ "/" ^
      (string_of_int player.id) ^ "/gamelogs?season=" ^ (string_of_int season)
    in
    get_html player_url

  (* Get a list of player seasons from html *)
  let rec tokens_of_html ?(result = []) html =
    let year =
      Str.search_forward (Str.regexp "<option value=\"[0-9][0-9][0-9][0-9]>") html 0
      |> fun n -> String.sub html (n+15) 4
    in
    let remaining_html =
      tokens_of_html ~result:(year :: result)
    in
    ()

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
    let [month; day] =
        if arr.(1) = "Bye" then ["0"; "0"] else Str.split (Str.regexp "/") arr.(1)
    in
    arr.(1) <- ((string_of_int year) ^ "-" ^ month ^ "-" ^ day);
    (* Fixup the opponet *)
    arr.(2) <- Str.global_replace (Str.regexp "[ @]+") "" arr.(2);
    (* Remove spaces from the score *)
    let arr_cln = Array.map (fun s -> Str.global_replace (Str.regexp "[ ]+") "" s) arr in
    (* Drop items 4 and drop the W/L, this can be obtained from the score *)
    Array.to_list @@
    Array.concat [Array.sub arr_cln 0 3; Array.sub arr_cln 5 (Array.length arr_cln - 5)]

  let player_stat_of_string_list ~game_type sl = {
    game_type      = game_type;
    week           = int_of_string @@ List.nth sl 0;
    game_date      = game_date_of_string @@ List.nth sl 1;
    opponet        = team_name_of_string @@ List.nth sl 2;
    game_result    = game_result_of_string @@ List.nth sl 3;
    played_in_game = if List.nth sl 4 = "1" then true else false;
    starter        = if List.nth sl 5 = "1" then true else false;
    passing_comp   = int_of_string @@ List.nth sl 6;
    passing_att    = int_of_string @@ List.nth sl 7;
    passing_pct    = float_of_string @@ List.nth sl 8;
    passing_yds    = float_of_string @@ List.nth sl 9;
    passing_avg    = float_of_string @@ List.nth sl 10;
    passing_td     = float_of_string @@ List.nth sl 11;
    passing_int    = float_of_string @@ List.nth sl 12;
    passing_sck    = float_of_string @@ List.nth sl 13;
    passing_scky   = float_of_string @@ List.nth sl 14;
    passing_rate   = float_of_string @@ List.nth sl 15;
    rushing_att    = int_of_string @@ List.nth sl 16;
    rushing_yds    = float_of_string @@ List.nth sl 17;
    rushing_avg    = float_of_string @@ List.nth sl 18;
    rushing_td     = int_of_string @@ List.nth sl 19;
    fumbles        = int_of_string @@ List.nth sl 20;
    fumbles_lost   = int_of_string @@ List.nth sl 21
  }

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
  let stats_in_season ~year player =
    (* Get html with non-space whitespace removed *)
    let url =
      "http://www.nfl.com/player/" ^
      player.first_name ^ player.last_name ^ "/" ^ (string_of_int player.id) ^
      "/gamelogs?season=" ^ (string_of_int year)
    in
    lwt html = get_html url in
    let game_types =
      split_season ~tag:"<tr class=\"NOcolors player-table-header\">" html
      |> List.map get_game_type
    in
    let g_tbl =
      split_season ~keep_delims:true ~combine_delims:true ~tag:"<table class='data-table1'" html
      |> List.map games_table
    in
    let headers = get_headers @@ List.hd g_tbl in
    let data_rows =
      List.map2 (fun gt sl -> get_games_data ~year ~game_type:gt sl) game_types g_tbl
    in
    Lwt.return (headers :: List.flatten data_rows)

  let test () =
    let drew_brees = {
      first_name = "Drew";
      last_name = "Brees";
      height = 0;
      weight = 0;
      team = Saints;
      status = Active;
      salary = 0;
      position = QB;
      rookie = false;
      id = 2504775
    }
    in
    player_html ~player:drew_brees ~season:2009 ()
    >>= fun y -> stats_in_season ~year:2009 drew_brees

end
