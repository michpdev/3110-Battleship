open StdLabels
open Battleship

let index (c : char) : int = Char.code c - 65

let print_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1) t'
    in
    loop 0 "" lst
  in
  pp_elts lst

let print_board
    (tile : Battleship.tile array array)
    (pp_elt : Battleship.tile -> string)
    (acc : string) =
  let tile = Array.to_list tile in
  let rec loop n acc = function
    | [] -> acc
    | h :: t ->
        loop (n + 1)
          (acc ^ Int.to_string n
          ^ (if n > 9 then " |" else "  |")
          ^ print_list pp_elt (Array.to_list h)
          ^ "|\n")
          t
  in
  "\n    ABCDEFGHIJ\n" ^ "    __________\n" ^ loop 1 "" tile
  ^ "    ‾‾‾‾‾‾‾‾‾‾"

let orientation_check orient1 orient2 = Bool.to_int (orient1 = orient2)

let pos_to_tile board ship_size posx posy orient tile_type =
  for i = 0 to ship_size - 1 do
    match orient with
    | Horizontal -> (
        match Array.get (Array.get board posy) (posx + i) with
        | Ship _ -> ()
        | _ -> Array.set (Array.get board posy) (posx + i) tile_type)
    | Vertical -> (
        match Array.get (Array.get board (posy + i)) posx with
        | Ship _ -> ()
        | _ -> Array.set (Array.get board (posy + i)) posx tile_type)
  done

let check_collisions board ship_size posx posy orient =
  let ret = ref false in
  for i = 0 to ship_size - 1 do
    match orient with
    | Horizontal -> (
        match Array.get (Array.get board posy) (posx + i) with
        | Ship _ -> ret := true
        | _ -> ())
    | Vertical -> (
        match Array.get (Array.get board (posy + i)) posx with
        | Ship _ -> ret := true
        | _ -> ())
  done;
  !ret

let rec place_ship
    (board : tile array array)
    (ship_pos : Battleship.ship_pos)
    ship_size
    posx
    posy
    orient : board =
  let length = Array.length board in
  pos_to_tile board ship_size posx posy orient Select;
  ANSITerminal.erase Screen;
  ANSITerminal.erase Eol;
  print_string
    (print_board board Battleship.to_string ""
    ^ "\nWhere would you like to place your ship?\n>");
  match read_line () with
  | "w" ->
      if posy - 1 < 0 then
        place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient Water;
        place_ship board ship_pos ship_size posx (posy - 1) orient)
  | "s" ->
      if
        posy + 1 + (orientation_check orient Vertical * (ship_size - 1))
        >= length
      then place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient Water;
        place_ship board ship_pos ship_size posx (posy + 1) orient)
  | "d" ->
      if
        posx + 1
        + (orientation_check orient Horizontal * (ship_size - 1))
        >= length
      then place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient Water;
        place_ship board ship_pos ship_size (posx + 1) posy orient)
  | "a" ->
      if posx - 1 < 0 then
        place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient Water;
        place_ship board ship_pos ship_size (posx - 1) posy orient)
  | "z" ->
      if check_collisions board ship_size posx posy orient then
        place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient (Ship orient);
        Battleship.place_ship board ship_pos (posx, posy) orient
          ship_size)
  | " " ->
      if
        match orient with
        | Horizontal -> posy + ship_size > length
        | Vertical -> posx + ship_size > length
      then place_ship board ship_pos ship_size posx posy orient
      else (
        pos_to_tile board ship_size posx posy orient Water;
        place_ship board ship_pos ship_size posx posy
          (match orient with
          | Horizontal -> Vertical
          | Vertical -> Horizontal))
  | "q" -> exit 0
  | "quit" -> exit 0
  | _ -> place_ship board ship_pos ship_size posx posy orient

let rec attack
    (pos : string)
    (board : Battleship.board)
    (ship_pos : Battleship.ship_pos) : Battleship.attack_result =
  let x = String.trim pos
  and check_coord (coord : int) =
    if coord >= Array.length board || coord < 0 then None else Some ()
  in
  let generate_coord1 : int option =
    try Some (index x.[0]) with
    | _ -> None
  and generate_coord2 : int option =
    try Some (int_of_string String.(sub x 1 (length x - 1)) - 1) with
    | _ -> None
  in
  match generate_coord1 with
  | None ->
      print_string
        "\n\
         Hmm looks like the first coordinate isn't a letter, could you \
         try again?\n";
      attack
        (match read_line () with
        | "q" -> exit 0
        | "quit" -> exit 0
        | s -> s)
        board ship_pos
  | Some c1 -> (
      match generate_coord2 with
      | None ->
          print_string
            "\n\
             Hmm looks like the second coordinate isn't a valid \
             number, could you try again?\n";
          attack
            (match read_line () with
            | "q" -> exit 0
            | "quit" -> exit 0
            | s -> s)
            board ship_pos
      | Some c2 -> (
          print_string ("\n" ^ string_of_int c1 ^ string_of_int c2);
          match check_coord c1 with
          | None ->
              print_string
                "\n\
                 This is not a valid location, please use the format \
                 [LETTER][NUMBER]\n";
              attack
                (match read_line () with
                | "q" -> exit 0
                | "quit" -> exit 0
                | s -> s)
                board ship_pos
          | Some _ -> (
              match check_coord c2 with
              | None ->
                  print_string
                    "\n\
                     This is not a valid location, please use the \
                     format [LETTER][NUMBER]\n";
                  attack
                    (match read_line () with
                    | "q" -> exit 0
                    | "quit" -> exit 0
                    | s -> s)
                    board ship_pos
              | Some _ ->
                  Battleship.attack
                    (Battleship.to_pos (index x.[0]) c2)
                    board ship_pos)))
