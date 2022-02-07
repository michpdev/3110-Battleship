(** Representation of a battleship board.

    This module represents the the state of a battleship board. That
    includes the state of each "tile" on the board (i.e. whether the
    tile is occupied by water or a ship, and whether the tile has been
    attacked or not).*)

(*type t (** The abstract type of values representing battleship
  game*) *)

type size = int
(** The abstract type of values representing a ship's size*)

type position = int * int
(** The abstract type of a tile's position*)

type orientation =
  | Vertical
  | Horizontal
      (** The abstract type representing a ships orientation. Ships can
          be oriented either vertically or horizontally.*)

type tile =
  | Water
  | Ship of orientation
  | Miss
  | Hit
  | Select
      (** The type of a tile on a board. A tile can contain, water, a
          ship a missed shot, or a hit.*)

type board = tile array array
(** The type of a board. A board is a 10x10 grid of tiles of of varying
    conditions.*)

type attack_result =
  | Miss
  | Hit
  | Sink

  type attack_phase =
  | Random
  | Targeted

type ship_pos = position list list ref
(** The type of ship positions. Each element of the list of ships is a
    list of tile positions each individual ship occupies. Ship tiles
    that have been attacked should not be in this list. *)

val init_ship_pos : unit -> ship_pos

val add_ship_pos : position -> size -> orientation -> ship_pos -> unit
(** [add_ship_pos (x, y) ship_size orient ships] is the updated list of
    ship positions after adding a ship at (x,y) Requires: (x,y) is a
    valid ship position on the board *)

val del_ship_pos : position -> ship_pos -> ship_pos
(** [del_ship_pos (x, y) ships] is the updated list of ship positions
    after an attack on (x,y) Raises: [SunkShip (x,y)] if an entire ship
    sunk, [Winner] if all ships have been sunk *)

val ships_to_score : ship_pos -> int
(** [ships_to_score ships] is the number of ships that have been sunk *)

val board_open_to_close : board -> board

val blank_board : size -> board
(**[blank_board] is a board consisting entirely of water tiles.*)

val reset_board : board -> board

val random_board : 'a -> ship_pos -> tile array array
(**[random_board] is a board conssisting of water tiles and 5 ships
   placed at random locations.*)

val is_valid_position :
  size * size -> size -> orientation -> board -> bool
(**Checks if the position on the board is valid.*)

val place_ship :
  board -> ship_pos -> position -> orientation -> size -> board
(**[place_ship b p o s ] is the board [b] with the ship of size [s] with
   orientation [o] placed at position [p].*)

val attack : position -> board -> ship_pos -> attack_result
(* [attack p b ] is the result of attacking position p on board b. *)

val to_string : tile -> string

val to_pos : int -> int -> position

(* [simple_bot] is a random position to attack on the battleship board.
   [simple_bot] does not attack duplicate locations*)
val simple_bot : unit -> position

(*Checks to see if there are any ships left on a given board*)
val check_for_ships : board -> bool

(*Checks the attack tile value*)
val attack_tile_val : position -> board -> attack_result

val update_board_with_attack : position -> board -> unit

(*Checks if the neighboring block can be hit*)
val is_valid_neighbor: position -> bool

(*Updates the attack phase for the AI*)
val update_attack_phase: position -> board -> attack_phase

(*Resets the board*)
val reset_board: board -> board

(*Contains the alogirthm for the medium bot*)
val medium_bot : position -> board -> position