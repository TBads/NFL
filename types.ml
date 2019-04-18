type status = Active | InActive | StatusError of string

type game_type = PreSeason | RegularSeason | PostSeason | GameTypeError of string

type position = QB | RB | WR | TE | DST | Unknown of string

type game_result = Win of int * int
                 | Loss of int * int
                 | Tie of int * int

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
  last_name  : string;
  height     : int;
  weight     : int;
  team       : team_name;
  status     : status;
  salary     : int;
  position   : position;
  rookie     : bool;
  id         : int
}

type team = {
  name       : string;
  city       : string;
  players    : player list;
  conference : string
}

type game_date = {year : int; month : int; day : int}

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

type conference = AFC_North
                | AFC_South
                | AFC_East
                | AFC_West
                | NFC_North
                | NFC_South
                | NFC_East
                | NFC_West

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
