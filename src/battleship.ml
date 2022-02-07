type orientation =
  | Vertical
  | Horizontal

type tile =
  | Water
  | Ship of orientation
  | Miss
  | Hit
  | Select

type attack_result =
  | Miss
  | Hit
  | Sink

let to_string = function
  | Water -> "~"
  | Ship Vertical -> "|"
  | Ship Horizontal -> "—"
  | Hit -> "x"
  | Miss -> "o"
  | Select -> "▓"

type size = int

type position = int * int

type board = tile array array

type ship_pos = position list list ref

exception Winner

exception SunkShip of position

let init_ship_pos () = ref []

let add_ship_pos (x, y) ship_size orient ships =
  let rec make_ship (x, y) orient size =
    if size = 0 then []
    else
      match orient with
      | Horizontal ->
          (x, y) :: make_ship (x + 1, y) Horizontal (size - 1)
      | Vertical -> (x, y) :: make_ship (x, y + 1) Vertical (size - 1)
  in
  let new_ship = make_ship (x, y) orient ship_size in
  let new_ships_lst = new_ship :: !ships in
  ships := new_ships_lst

let del_ship_pos (x, y) ships =
  let updated_ships =
    List.map
      (fun ship_lst ->
        List.filter (fun (x', y') -> (x', y') <> (x, y)) ship_lst)
      !ships
  in
  if List.exists (fun ship -> ship = []) updated_ships then (
    let no_sunk_ships =
      List.filter (fun ship -> ship <> []) updated_ships
    in
    if no_sunk_ships = [] then raise Winner else ships := no_sunk_ships;
    raise (SunkShip (x, y)))
  else ships := updated_ships;
  ships

let ships_to_score ships = 5 - List.length !ships

let print_string_of_int_tuple t =
  print_string
    ("(" ^ string_of_int (fst t) ^ ", " ^ string_of_int (snd t) ^ ")")

(* Requires: x < 10, y < 10, b is a valid board*)
let attack_tile_val ((x, y) : position) (b : board) : attack_result =
  (* Hard coded case for first bot attack of game *)
  if (x, y) = (-1, -1) then Miss
  else
    let attack_column = Array.get b x in
    let attack_tile = Array.get attack_column y in
    match attack_tile with
    | Water -> Miss
    | Miss -> Miss
    | Ship Vertical -> Hit
    | Ship Horizontal -> Hit
    | Hit -> Hit
    | Select -> failwith {|Attacked "Select" tile|}

let update_board_with_attack ((x, y) : position) (b : board) =
  let result = attack_tile_val (x, y) b in
  if result = Hit then b.(x).(y) <- Hit else b.(x).(y) <- Miss

let attack ((x, y) : position) (b : board) (ships : ship_pos) :
    attack_result =
  update_board_with_attack (y, x) b;
  try
    (match del_ship_pos (x, y) ships with
    | ship_pos -> ships := !ship_pos);
    attack_tile_val (y, x) b
  with
  | Winner -> attack_tile_val (y, x) b
  | SunkShip s -> attack_tile_val (y, x) b

let blank_board size = Array.make_matrix size size Water

let rec get_column brd x = Array.map (fun row -> row.(x)) brd

let is_valid_position (x, y) ship_size orient brd =
  match orient with
  | Horizontal ->
      x + ship_size <= 10
      && Array.for_all
           (fun i -> i = Water)
           (Array.sub brd.(y) x ship_size)
  | Vertical ->
      y + ship_size <= 10
      && Array.for_all
           (fun i -> i = Water)
           (Array.sub (get_column brd x) y ship_size)

let place_ship brd ship_pos (x, y) orient ship_size =
  add_ship_pos (x, y) ship_size orient ship_pos;
  match orient with
  | Horizontal ->
      brd.(y) <-
        Array.mapi
          (fun i elm ->
            if i >= x && i < x + ship_size then Ship Horizontal else elm)
          brd.(y);
      brd
  | Vertical ->
      let replace row x =
        Array.set row x (Ship Vertical);
        row
      in
      Array.mapi
        (fun i row ->
          if i >= y && i < y + ship_size then replace row x else row)
        brd

let reset_board (b : board) =
  Array.map (fun row -> Array.map (fun v -> Water) row) b

let random_board size (ships : ship_pos) =
  let _ = Random.self_init () in
  let rec add_ship ship_sizes brd =
    let x, y = (Random.int 10, Random.int 10) in
    let orient = if Random.int 2 = 1 then Vertical else Horizontal in
    match ship_sizes with
    | [] -> brd
    | h :: t ->
        if is_valid_position (x, y) h orient brd then
          let new_brd = place_ship brd ships (x, y) orient h in
          add_ship t new_brd
        else add_ship ship_sizes brd
  in
  add_ship [ 5; 4; 3; 3; 2 ] (blank_board 10)

let to_pos (x : int) (y : int) : position = (x, y)

let board_open_to_close brd =
  let convert_row row =
    Array.map
      (fun elm ->
        match elm with
        | Ship x -> Water
        | _ -> elm)
      row
  in
  Array.map (fun row -> convert_row row) brd

(*Sorting algorithm from
  https://discuss.ocaml.org/t/more-natural-preferred-way-to-shuffle-an-array/217*)
let knuth_shuffle a =
  let _ = Random.self_init () in
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let get_coord shuffled_coords =
  let options_as_lst = Array.to_list !shuffled_coords in
  shuffled_coords := Array.of_list (List.tl options_as_lst);
  List.hd options_as_lst

let simple_bot () =
  let coords = Array.make_matrix 10 10 (0, 0) in
  (* populate coords with coordinates*)
  for x = 0 to 9 do
    let x = x in
    for y = 0 to 9 do
      coords.(x).(y) <- (x, y)
    done
  done;

  (*Create 1d array of coordinate pairs*)
  let array_of_coords_1d = Array.(concat (to_list coords)) in
  (*Shuffle the pairs*)
  let shuffled_coords = ref (knuth_shuffle array_of_coords_1d) in

  (*Each time we call the function, return the first of the shuffled
    pair and remove that pair from the array*)
  get_coord shuffled_coords

let check_for_ships (board : board) : bool =
  let ret = ref false in
  for i = 0 to Array.length board - 1 do
    ret :=
      !ret
      || Array.exists
           (fun x ->
             match x with
             | Ship _ -> true
             | _ -> false)
           (Array.get board i)
  done;
  !ret

(* medium bot *)

type attack_phase =
  | Random
  | Targeted

(* hashtbl of every other board tile *)

let tbl_to_array tbl =
  Hashtbl.fold
    (fun k v acc -> Array.(append (make 1 k) acc))
    tbl
    (Array.make 0 (99, 99))

let rand_attack_options () =
  let coord_tbl = Hashtbl.create 50 in
  for x = 0 to 9 do
    let x = x in
    for y = 0 to 9 do
      if (x mod 2 = 1 && y mod 2 = 0) || (x mod 2 = 0 && y mod 2 = 1)
      then Hashtbl.add coord_tbl (x, y) ()
    done
  done;
  tbl_to_array coord_tbl

let already_attacked = ref []

(* List of a hit location's neighbors*)
let targeted_lst = ref []

let random_arr = ref (knuth_shuffle (rand_attack_options ()))

(* [is_valid_neighbor pos)] is true if pos is a valid board position
   that hasn't already been attacked*)
let is_valid_neighbor ((x, y) : position) =
  (* filter for position within board *)
  let in_board = (x >= 0 && x <= 9) && y >= 0 && y <= 9 in
  let not_attacked = not (List.mem (x, y) !already_attacked) in
  in_board && not_attacked

(* [get_neighbors pos] updates [targeted_lst] with the tiles surrounding
   [pos] that are on the board and haven't been attacked *)
let get_neighbors (x, y) =
  let neighbors = [ (x - 1, y); (x + 1, y); (x, y + 1); (x, y - 1) ] in
  let valid_neighbors = List.filter is_valid_neighbor neighbors in
  let new_targeted = List.append valid_neighbors !targeted_lst in
  targeted_lst := new_targeted

(* [targeted_helper pos] updates attack list and returns a position to
   attack *)
let targeted_helper ((x, y) : position) (b : board) : position =
  let last_attack_result = attack_tile_val (x, y) b in
  if last_attack_result = Hit then get_neighbors (x, y) else ();
  let coord =
    match List.hd !targeted_lst with
    | exception _ -> failwith "empty target list"
    | x, y -> (x, y)
  in
  (* add to already attacked *)
  let _ = already_attacked := coord :: !already_attacked in
  (* remove from targeted_lst *)
  let _ =
    targeted_lst :=
      List.filter (fun (x, y) -> (x, y) <> coord) !targeted_lst
  in
  (* remove from random array *)
  let random_arr_as_lst = Array.to_list !random_arr in
  let filtered_rand_lst =
    List.filter (fun (x, y) -> (x, y) <> coord) random_arr_as_lst
  in
  let _ = random_arr := Array.of_list filtered_rand_lst in
  coord

(* [update_attack_phase pos b] is Targeted if the last attack hit a ship
   or there are coordinates in targeted_list, and Random otherwise *)
let update_attack_phase ((x, y) : position) (b : board) =
  let last_attack_result = attack_tile_val (x, y) b in
  let targets_remaining = !targeted_lst != [] in
  match (last_attack_result, targets_remaining) with
  (* Hit ship for first time, no targets remaining *)
  | Hit, _ -> Targeted
  | Miss, true -> Targeted
  | Miss, false -> Random
  (* If ship sunk, clear targeted_list and return to random strategy *)
  | Sink, _ ->
      targeted_lst := [];
      Random

let medium_bot (pos : position) (b : board) : position =
  let phase = update_attack_phase pos b in
  match phase with
  | Random ->
      let coord = get_coord random_arr in
      already_attacked := coord :: !already_attacked;
      coord
  | Targeted -> targeted_helper pos b
