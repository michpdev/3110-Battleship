val print_board :
  Battleship.tile array array ->
  (Battleship.tile -> string) ->
  string ->
  string

val place_ship :
  Battleship.board ->
  Battleship.ship_pos ->
  int ->
  int ->
  int ->
  Battleship.orientation ->
  Battleship.board

val pos_to_tile :
  Battleship.board ->
  int ->
  int ->
  int ->
  Battleship.orientation ->
  Battleship.tile ->
  unit

val attack :
  string ->
  Battleship.board ->
  Battleship.ship_pos ->
  Battleship.attack_result

val check_collisions:
Battleship.board  ->
  Battleship.size ->
  Battleship.size ->
    Battleship.size ->
     Battleship.orientation ->
      bool
