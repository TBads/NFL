open Types

let rec filter_map ?(accum = []) ~filter_f ~map_f l =
  match l with
  | [] -> List.rev accum
  | hd :: tl ->
    if filter_f hd
    then filter_map ~accum:((map_f hd) :: accum) ~filter_f ~map_f tl
    else filter_map ~accum ~filter_f ~map_f tl

let game_type_of_string s =
  match String.uppercase s with
  | "PRESEASON" -> PreSeason
  | "REGULAR SEASON" -> RegularSeason
  | "POSTSEASON" -> PostSeason
  | _ -> GameTypeError s

let string_of_game_type gt =
  match gt with
  | PreSeason       -> "PreSeason"
  | RegularSeason   -> "RegularSeason"
  | PostSeason      -> "PostSeason"
  | GameTypeError s -> "ERROR: " ^ s

let string_of_position p =
  match p with
  | QB        -> "QB"
  | RB        -> "RB"
  | WR        -> "WR"
  | TE        -> "TE"
  | DST       -> "DST"
  | Unknown s -> "ERROR: " ^ s

let string_of_status s =
  match s with
  | Active        -> "Active"
  | InActive      -> "Inactive"
  | StatusError s -> "ERROR: " ^ s

let game_result_of_string s =
  let l = Str.split (Str.regexp "-") s in
  let s1 = int_of_string (List.hd l) in
  let s2 = int_of_string (List.nth l 1) in
  if s1 > s2
  then Win (s1, s2)
  else (
    if s1 < s2
    then Loss (s1, s2)
    else Tie (s1, s2)
  )

let string_of_game_result gr =
  match gr with
  | Win (a, b) | Loss (a, b) | Tie (a, b) -> (string_of_int a) ^ "-" ^ (string_of_int b)

let game_date_of_string s =
  let l = Str.split (Str.regexp "-") s in
  {
    year  = int_of_string @@ List.hd l;
    month = int_of_string @@ List.nth l 1;
    day   = int_of_string @@ List.nth l 2
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
  | _ as s     -> StatusError s

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

let string_of_team_name tn =
  match tn with
  | Ravens          -> "BAL"
  | Bengals         -> "CIN"
  | Browns          -> "CLE"
  | Steelers        -> "PIT"
  | Texans          -> "HOU"
  | Colts           -> "IND"
  | Jaguars         -> "JAC"
  | Titans          -> "TEN"
  | Bills           -> "BUF"
  | Dolphins        -> "MIA"
  | Patriots        -> "NE"
  | Jets            -> "NYJ"
  | Broncos         -> "DEN"
  | Chiefs          -> "KC"
  | Raiders         -> "OAK"
  | Chargers        -> "SAN"
  | Bears           -> "CHI"
  | Lions           -> "DET"
  | Packers         -> "GB"
  | Vikings         -> "MIN"
  | Falcons         -> "ATL"
  | Panthers        -> "CAR"
  | Saints          -> "NO"
  | Buccaneers      -> "TB"
  | Cowboys         -> "DAL"
  | Giants          -> "NYG"
  | Eagles          -> "PHI"
  | Redskins        -> "WAS"
  | Cardinals       -> "ARI"
  | Rams            -> "STL"
  | SF49ers         -> "SF"
  | Seahawks        -> "SEA"
  | TeamNameError s -> "ERROR: " ^ s

let player_of_string s =
  let sl = Str.split (Str.regexp ",") s in
  {
    first_name = List.nth sl 1;
    last_name  = List.nth sl 0;
    height     = (try int_of_string @@ List.nth sl 3 with _ -> -1);
    weight     = (try int_of_string @@ List.nth sl 4 with _ -> -1);
    team       = team_name_of_string @@ List.nth sl 6;
    status     = Active; (* TODO *)
    salary     = 0; (* TODO *)
    position   = position_of_string @@ List.nth sl 2;
    rookie     = false; (* TODO *)
    id         = (try int_of_string @@ List.nth sl 7 with _ -> -1)
  }

let string_of_player p =
  p.first_name ^ "," ^
  p.last_name ^ "," ^
  (string_of_int p.height) ^ "," ^
  (string_of_int p.weight) ^ "," ^
  (string_of_team_name p.team) ^ "," ^
  (string_of_status p.status) ^ "," ^
  (string_of_int p.salary) ^ "," ^
  (string_of_position p.position) ^ "," ^
  (string_of_bool p.rookie) ^ "," ^
  (string_of_int p.id)

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

let string_of_player_stat ps =
(string_of_game_type ps.game_type) ^ "," ^
(string_of_int ps.week) ^ "," ^
(string_of_int ps.game_date.year) ^ "-" ^
(string_of_int ps.game_date.month) ^ "-" ^
(string_of_int ps.game_date.day) ^ "," ^
(string_of_team_name ps.opponet) ^ "," ^
(string_of_game_result ps.game_result) ^ "," ^
(string_of_bool ps.played_in_game) ^ "," ^
(string_of_bool ps.starter) ^ "," ^
(string_of_int ps.passing_comp) ^ "," ^
(string_of_int ps.passing_att) ^ "," ^
(string_of_float ps.passing_pct) ^ "," ^
(string_of_float ps.passing_yds) ^ "," ^
(string_of_float ps.passing_avg) ^ "," ^
(string_of_float ps.passing_td) ^ "," ^
(string_of_float ps.passing_int) ^ "," ^
(string_of_float ps.passing_sck) ^ "," ^
(string_of_float ps.passing_scky) ^ "," ^
(string_of_float ps.passing_rate) ^ "," ^
(string_of_int ps.rushing_att) ^ "," ^
(string_of_float ps.rushing_yds) ^ "," ^
(string_of_float ps.rushing_avg) ^ "," ^
(string_of_int ps.rushing_td) ^ "," ^
(string_of_int ps.fumbles) ^ "," ^
(string_of_int ps.fumbles_lost)

let string_of_data_row dr =
  match dr with
  | Headers sl -> String.concat "," sl
  | GameData ps -> string_of_player_stat ps
  | DataRowError s -> s
