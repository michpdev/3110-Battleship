open OUnit2
open Game
open Battleship
open Gui

let blank_test (name : string) (j : int) (e : Battleship.board) : test =
  name >:: fun _ -> assert_equal e (blank_board j)

let is_valid_position_test
    (name : string)
    (a : size * size)
    (s : size)
    (o : orientation)
    (b : tile array array)
    (e : bool) : test =
  name >:: fun _ ->
  assert_equal e (Battleship.is_valid_position a s o b)

let place_ship_test
    (name : string)
    (b : board)
    (p : ship_pos)
    (a : position)
    (o : orientation)
    (s : size)
    (e : board) : test =
  name >:: fun _ -> assert_equal e (Battleship.place_ship b p a o s)

let to_string_test (name : string) (t : tile) (e : string) : test =
  name >:: fun _ -> assert_equal e (to_string t)

let to_pos_test (name : string) (x : int) (y : int) (e : position) :
    test =
  name >:: fun _ -> assert_equal e (to_pos x y)

let check_for_ships_test (name : string) (b : board) (e : bool) : test =
  name >:: fun _ -> assert_equal e (check_for_ships b)

let attack_test
    (name : string)
    (p : position)
    (b : board)
    (s : ship_pos)
    (e : attack_result) : test =
  name >:: fun _ -> assert_equal e (Battleship.attack p b s)

let attack_tile_val_test
    (name : string)
    (p : position)
    (b : board)
    (e : attack_result) : test =
  name >:: fun _ -> assert_equal e (attack_tile_val p b)

let reset_board_test
(name : string)
    (b : board)
    (e : board) : test =
  name >:: fun _ -> assert_equal e (reset_board b)

let update_board_with_attack_test
    (name : string)
    (p : position)
    (b : board)
    (e : unit) : test =
  name >:: fun _ -> assert_equal e (update_board_with_attack p b)

let is_valid_neighbor_test
(name : string)
(p : position)
(e : bool) : test =
name >:: fun _ -> assert_equal e (is_valid_neighbor p)

let update_attack_phase_test
(name : string)
(p : position)
(b: board)
(e : Battleship.attack_phase) : test =
name >:: fun _ -> assert_equal e (update_attack_phase p b)


let gui_check_collisions_test 
(name: string)
(b:board)
(s: size)
(x: size)
(y: size)
(o: orientation)
(e: bool) : test =
name >:: fun _ -> assert_equal e (Gui.check_collisions b s x y o)
let gui_print_board_test
  (name: string)
  (b: board)
  (p: tile -> string)
  (acc: string)
  (e: string) : test = 
  name >:: fun _ -> assert_equal e (Gui.print_board b p acc)

let gui_place_ship_test
    (name : string)
    (b : Battleship.board)
    (s : ship_pos)
    (a : int)
    (x : int)
    (y : int)
    (o : Battleship.orientation)
    (e : Battleship.board) : test =
  name >:: fun _ -> assert_equal e (Gui.place_ship b s a x y o)

let gui_pos_to_tile_test
    (name : string)
    (b : Battleship.board)
    (s : int)
    (x : int)
    (y : int)
    (o : Battleship.orientation)
    (t : Battleship.tile)
    (e : unit) : test =
  name >:: fun _ -> assert_equal e (Gui.pos_to_tile b s x y o t)

let gui_attack_test
    test
    (name : string)
    (a : string)
    (b : Battleship.board)
    (p : ship_pos)
    (e : Battleship.attack_result) : test =
  name >:: fun _ -> assert_equal e (Gui.attack a b p)

let battleship_tests =
  [
    (**First Set of Tests Test Impossible Boards, but will still test the functionality of the code*)
    blank_test "should be 3 x 3 grid" 3
      [| [| Water; Water; Water |];
        [| Water; Water; Water |];
        [| Water; Water; Water |];|];
    blank_test "should be 10 x 10 grid" 10
        [|[|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
          [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |];
      is_valid_position_test "yes" (0,0) 2 Vertical 
    [|[|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |]  true;
      is_valid_position_test "no" (0,0) 5 Horizontal 
      [|[|Water;Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |]  false;
    to_string_test "test water" Water "~";
    to_string_test "test hit" Hit "x";
    to_string_test "test miss" Miss "o";
    to_string_test "test select" Select "▓";
    to_pos_test "should be (4,6)" 4 6 (4, 6);
    to_pos_test "should be (0,0)" 0 0 (0, 0);
    check_for_ships_test "no" (blank_board 10) false;
    check_for_ships_test "no" (blank_board 3) false;
    check_for_ships_test "yes"
      [| [| Water; Water; Water; Water; Water |];
        [| Water; Ship Horizontal;Ship Horizontal;Ship Horizontal; Water;|];
        [| Water; Water; Water; Water; Water |];
        [| Water; Water; Water; Water; Water |];
        [| Water; Water; Water; Water; Water |];|] true;
    check_for_ships_test "yes"
    [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |]  true;
    attack_tile_val_test "should be Hit" (0,0) [|[|Hit; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |]  Hit;
    attack_tile_val_test "should be Miss" (1,5) 
    [|[|Hit; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Miss; Miss; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |]  Miss;
   attack_tile_val_test "should be hit" (3,3) 
    [|[|Hit; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Miss; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Hit; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Hit; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] Hit;   
   is_valid_neighbor_test "should be true" (0,0) true; 
   update_attack_phase_test "Should be random" (3,3) 
   [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] Random;   
    update_attack_phase_test "Should be targeted" (2,0) 
    [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Hit; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
     [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] Targeted;   
     update_attack_phase_test "Should be random" (0,3) 
     [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] Random;  
      update_attack_phase_test "Should be targeted" (2,0) 
      [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Miss; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Hit; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Miss; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] Targeted;   
       check_for_ships_test  "no"  
       [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] false;
       reset_board_test  "reset"  
       [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] (blank_board 10);
      reset_board_test "reset" [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
    [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] (blank_board 10);
    
  ]
let gui_tests = [gui_check_collisions_test "should be true"
[|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|]; |] 3 0 1 Vertical true;
      gui_check_collisions_test "should be false"
[|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Ship Horizontal; Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water; Water; Water|]; |] 4 1 4 Horizontal false;
      gui_check_collisions_test "should be false"
      [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Ship Horizontal; Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water|];
      [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
      [|Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water; Water; Water|]; |] 5 0 0  Horizontal false;
       gui_check_collisions_test "should be true"
       [|[|Water;Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Ship Horizontal; Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water|];
       [|Ship Vertical; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Water; Water; Water; Water; Water; Water; Water; Water; Water; Water|];
       [|Ship Horizontal; Ship Horizontal; Ship Horizontal; Water; Water; Water; Water; Water; Water; Water|]; |]3 0 9 Horizontal true; 
   

]
let final_tests = []

let tests =
  "Battleship tests" >::: List.flatten [ battleship_tests; gui_tests; final_tests  ]

let _ = run_test_tt_main tests