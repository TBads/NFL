(* Draft Kings *)

#require "lwt.syntax";;
#require "lwt";;
#require "cohttp.lwt";;
#require "uri";;
#require "str";;
#require "markup";;

(* More data available on Yahoo Sports *)

type status = Active | InActive | StatusError of string

type game_type = PreSeason | RegularSeason | PostSeason

type position = QB | RB | WR | TE | DST | Unknown of string

type game_result = Win of int * int
                 | Loss of int * int

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

(* Player stats for a single game *)
type player_stats = {
  game_type      : game_type;
  date_date      : Unix.tm;
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
    | "Baltimore Ravens"     -> Ravens
    | "Cincinati Bengals"    -> Bengals
    | "Cleveland Browns"     -> Browns
    | "Pittsburgh Steelers"  -> Steelers
    | "Houston Texans"       -> Texans
    | "Indianapolis Colts"   -> Colts
    | "Jacksonville Jaguars" -> Jaguars
    | "Tennessee Titans"     -> Titans
    | "Buffalo Bills"        -> Bills
    | "Miami Dolphins"       -> Dolphins
    | "New England Patriots" -> Patriots
    | "New York Jets"        -> Jets
    | "Denver Broncos"       -> Broncos
    | "Kansas City Chiefs"   -> Chiefs
    | "Okland Raiders"       -> Raiders
    | "San Diego Chargers"   -> Chargers
    | "Chicago Bears"        -> Bears
    | "Detroit Lions"        -> Lions
    | "Green Bay Packers"    -> Packers
    | "Minnesota Vikings"    -> Vikings
    | "Atlanta Falcons"      -> Falcons
    | "Carolina Panthers"    -> Panthers
    | "New Orleans Saints"   -> Saints
    | "Tampa Bay Buccaneers" -> Buccaneers
    | "Dallas Cowboys"       -> Cowboys
    | "New York Giants"      -> Giants
    | "Philadelphia Eagles"  -> Eagles
    | "Washington Redskins"  -> Redskins
    | "Arizona Cardinals"    -> Cardinals
    | "Los Angeles Rams"     -> Rams
    | "San Francisco 49ers"  -> SF49ers
    | "Seattle Seahawks"     -> Seahawks
    | _                      -> TeamNameError s

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

  (* Find the first dom element which has a specific tag and payload *)
  let rec find_dom_start_element ~tag ~dom_list (l : Markup.signal list) =
    match l with
    | [] -> []
    | hd :: tl -> (
        match hd with
        | `Start_element ((_, tag_str), dl) ->
          if (tag_str = tag) && (dl = dom_list)
          then l
          else find_dom_start_element ~tag ~dom_list tl
        | _ -> find_dom_start_element ~tag ~dom_list tl
      )

  (* TEST THIS *)
  let rec seek_exact_start_element
    ?(dom_lists_to_check = [])
    ~tag
    ~target_dom_list
    (l : Markup.signal list) =
    match l with
    | [] ->
      begin
        match dom_lists_to_check with
        | [] -> []
        | hd :: [] ->
          begin
            match hd with
            | `Start_element ((_, tag_str), dl) ->
                if (tag_str = tag) && (dl = target_dom_list) then [hd] else []
            | _ -> []
          end
        | hd :: tl -> seek_exact_start_element ~dom_lists_to_check:[hd] ~tag ~target_dom_list tl
      end
    | hd :: tl ->
      seek_exact_start_element
        ~dom_lists_to_check:(hd :: dom_lists_to_check)
        ~tag
        ~target_dom_list
        tl

  let rec seek_exact_start_element_2
    ?(dom_lists_to_check = [])
    ~tag
    ~target_dom_list
    (l : Markup.signal list) =
    match l with
    | [] -> List.rev dom_lists_to_check
    | hd :: tl ->
      seek_exact_start_element_2
        ~dom_lists_to_check:(hd :: dom_lists_to_check)
        ~tag
        ~target_dom_list
        tl

  (*
      Maybe the easiest way to find the table is to
      (1) search for the table tag regex
      (2) then parse as markup
  *)

  let markup_of_html html =
    let open Markup in
    string html |> parse_html |> signals |> to_list

  (* Select a specific section of html inbetween tags ~start_tag and ~end_tag, including tags *)
  let html_substring ~tag html =
    let clean_html = Str.global_replace (Str.regexp "[\r\t\n\ ]+") " " html in
    (* Remove everything to the left of the target html token *)
    let start_loc = Str.search_forward (Str.regexp tag) clean_html 0 in
    let str_and_extra =
      String.sub clean_html start_loc ((String.length clean_html) - start_loc)
    in
    (* Convert to a Markup.signal list, then parse the right side of the string away *)
    truncate_dom_list @@ markup_of_html str_and_extra

  (* TODO: Pull player stats for a specific season *)
  let stats_in_season player year =
    (* Get html with non-space whitespace removed *)
    let url =
      "http://www.nfl.com/player/" ^
      player.first_name ^ player.last_name ^ "/" ^ (string_of_int player.id) ^
      "/gamelogs?season=" ^ (string_of_int year)
    in
    lwt html = get_html url in
    (* Get the section of html containing all tables *)
    Lwt.return @@ html_substring ~tag:"<table class='data-table1'" html
    (* TODO: Get the preseason table, the parse into a string list list *)
    (* Then expand to get the regular, postseason, etc... tables *)

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
    player_html ~player:drew_brees ~season:2015 ()
    >>= get_player_seasons
    >|= List.hd
    >>= (stats_in_season drew_brees)

end
