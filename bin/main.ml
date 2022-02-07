open Game.Battleship
open Game.Gui

let wins = ref 0

let losses = ref 0

let setup_board
    (board : Game.Battleship.board)
    (ships : Game.Battleship.ship_pos) : unit =
  print_string
    (print_board (place_ship board ships 5 0 0 Horizontal) to_string "");
  print_string
    (print_board (place_ship board ships 4 0 0 Horizontal) to_string "");
  print_string
    (print_board (place_ship board ships 3 0 0 Horizontal) to_string "");
  print_string
    (print_board (place_ship board ships 3 0 0 Horizontal) to_string "");
  print_string
    (print_board (place_ship board ships 2 0 0 Horizontal) to_string "")

type difficulty =
  | Easy
  | Medium
  | Hard

(* TODO: Update to be selected by user *)
let difficulty_ref = ref Medium

let last_attack_pos = ref (-1, -1)

let board1 = Game.Battleship.blank_board 10

let ships1 = ref []

let board2 = Game.Battleship.blank_board 10

let ships2 = ref []

let ships3 = ref []

let board3 = Game.Battleship.random_board 10 ships3

let simple_attack board ships =
  match
    Game.Battleship.attack (Game.Battleship.simple_bot ()) board ships
  with
  | Miss -> print_string "The bot missed!\n"
  | Hit -> print_string "You got hit!\n"
  | Sink -> print_string "You lost a battleship!\n"

let medium_attack board ships =
  let attacked_position =
    Game.Battleship.medium_bot !last_attack_pos board
  in
  (* print_string (string_of_int (fst attacked_position) ^ "," ^
     string_of_int (snd attacked_position)); *)
  last_attack_pos := attacked_position;
  match Game.Battleship.attack attacked_position board ships with
  | Miss -> print_string "The bot missed!\n"
  | Hit -> print_string "You got hit!\n"
  | Sink -> print_string "You lost a battleship!\n"

let rec end_match (dummy : unit) =
  let win_ratio =
    if !losses <> 0 then float_of_int !wins /. float_of_int !losses
    else -1.0
  in
  let _ = Game.Battleship.reset_board board1
  and _ = Game.Battleship.reset_board board2
  and _ = Game.Battleship.reset_board board3 in
  ships1 := [];
  ships2 := [];
  ships3 := [];
  let rec prompt_player (dummy : unit) : unit =
    print_string "Would you like like to play another match?";
    match read_line () with
    | "y" -> ()
    | "Y" -> ()
    | "yes" -> ()
    | "Yes" -> ()
    | "YES" -> ()
    | "n" -> exit 0
    | "N" -> exit 0
    | "no" -> exit 0
    | "No" -> exit 0
    | "NO" -> exit 0
    | "q" -> exit 0
    | "quit" -> exit 0
    | _ ->
        print_string
          "I'm sorry I don't understand what you are saying, please \
           use y/n\n";
        prompt_player ();
        choose_number ()
  in
  print_string
    ("You have currently won " ^ string_of_int !wins
   ^ " matches and have lost " ^ string_of_int !losses
   ^ ", giving you a total win ratio of "
    ^ string_of_float win_ratio
    ^ ". "
    ^
    if win_ratio = -1.0 then
      "Wow, you've never lost a match before! Either you're the best \
       battleship player ever or you haven't played that many matches \
       -_-"
    else if win_ratio <= 0.25 then "Wow, you really suck"
    else if win_ratio <= 0.5 then
      "That's a pretty bad win rate, not gonna lie"
    else if win_ratio = 1.0 then "Come on, you can do better than this!"
    else if win_ratio <= 1.5 then "Hey that's decent"
    else if win_ratio <= 1.75 then
      "That's a pretty good score there, champ"
    else "You are clearly a god at this game\n");
  prompt_player ();
  choose_number ()

and choose_number (dummy : unit) : unit =
  let rec choose_difficulty (dummy : unit) =
    print_string "Easy, Medium, or Hard?\n";
    match read_line () with
    | "Easy" -> difficulty_ref := Easy
    | "easy" -> difficulty_ref := Easy
    | "Medium" -> difficulty_ref := Medium
    | "medium" -> difficulty_ref := Medium
    | "Hard" -> difficulty_ref := Hard
    | "hard" -> difficulty_ref := Hard
    | "q" -> exit 0
    | "quit" -> exit 0
    | _ ->
        print_string
          "I'm sorry I did not understand you, please answer with:\n";
        choose_difficulty ()
  in
  print_string
    "Would you like to play with another person or against a bot?\n";
  match read_line () with
  | "person" ->
      print_string "Player 1 please place your ships";
      setup_board board1 ships1;
      ANSITerminal.erase Screen;
      print_string "Player 2 please place your ships";
      setup_board board2 ships2;
      ANSITerminal.erase Screen;
      main_loop_coop board1 board2 ships1 ships2
  | "Person" ->
      print_string "Player 1 please place your ships";
      setup_board board1 ships1;
      ANSITerminal.erase Screen;
      print_string "Player 2 please place your ships";
      setup_board board2 ships2;
      ANSITerminal.erase Screen;
      main_loop_coop board1 board2 ships1 ships2
  | "Another person" ->
      print_string "Player 1 please place your ships";
      setup_board board1 ships1;
      ANSITerminal.erase Screen;
      print_string "Player 2 please place your ships";
      setup_board board2 ships2;
      ANSITerminal.erase Screen;
      main_loop_coop board1 board2 ships1 ships2
  | "another person" ->
      print_string "Player 1 please place your ships";
      setup_board board1 ships1;
      ANSITerminal.erase Screen;
      print_string "Player 2 please place your ships";
      setup_board board2 ships2;
      ANSITerminal.erase Screen;
      main_loop_coop board1 board2 ships1 ships2
  | "bot" ->
      choose_difficulty ();
      print_string "Please place your ships";
      setup_board board1 ships1;
      main_loop_bot board1 board3 ships1 ships3
  | "Bot" ->
      choose_difficulty ();
      print_string "Please place your ships";
      setup_board board1 ships1;
      main_loop_bot board1 board3 ships1 ships3
  | "A bot" ->
      choose_difficulty ();
      print_string "Please place your ships";
      setup_board board1 ships1;
      main_loop_bot board1 board3 ships1 ships3
  | "a bot" ->
      choose_difficulty ();
      print_string "Please place your ships";
      setup_board board1 ships1;
      main_loop_bot board1 board3 ships1 ships3
  | "q" -> exit 0
  | "quit" -> exit 0
  | _ ->
      print_string
        "I did not recognize that, please say person or bot\n";
      choose_number ()

and main_loop_coop board1 board2 ships1 ships2 : unit =
  let rec pass_to_p1 (t : unit) : unit =
    print_string "Does player 1 have the board?\n";
    match read_line () with
    | "y" -> ()
    | "Y" -> ()
    | "yes" -> ()
    | "Yes" -> ()
    | "YES" -> ()
    | "q" -> exit 0
    | "quit" -> exit 0
    | _ ->
        print_string "Plese pass the screen to player 1\n";
        pass_to_p1 ()
  and pass_to_p2 (t : unit) : unit =
    print_string "Does player 2 have the board?\n";
    match read_line () with
    | "y" -> ()
    | "Y" -> ()
    | "yes" -> ()
    | "Yes" -> ()
    | "YES" -> ()
    | "q" -> exit 0
    | "quit" -> exit 0
    | _ ->
        print_string "Plese pass the screen to player 2\n";
        pass_to_p2 ()
  in
  pass_to_p1 ();
  ANSITerminal.erase Screen;
  print_string
    ((*"FOR TESTING: \n" ^ print_board board2 to_string "" ^ "\n\n" ^*)
     print_board (board_open_to_close board2) to_string ""
    ^ "\n\n"
    ^ print_board board1 to_string "");
  print_string
    ("\nSCORE: "
    ^
    match (ships_to_score ships2, ships_to_score ships1) with
    | x, y when x > y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " You\n"
    | x, y when y > x ->
        string_of_int y ^ "-" ^ string_of_int x ^ " Player 2\n"
    | x, y when x == y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " TIED\n"
    | _, _ -> "TIED\n");
  print_string "Player 1's turn to attack\n";
  print_string "Where would you like to attack?\n";
  (match
     Game.Gui.attack
       (match read_line () with
       | "q" -> exit 0
       | "quit" -> exit 0
       | s -> s)
       board2 ships2
   with
  | Miss ->
      ANSITerminal.erase Screen;
      print_string "Aw man you missed!\n"
  | Hit ->
      ANSITerminal.erase Screen;
      print_string "Got a hit!\n"
  | Sink ->
      ANSITerminal.erase Screen;
      print_string "You sunk a battleship!\n");
  (*ANSITerminal.erase Above;*)
  if true <> check_for_ships board2 then (
    print_string "Congratulations, you win!\n";
    wins := !wins + 1;
    exit 0)
  else pass_to_p2 ();
  ANSITerminal.erase Screen;
  print_string
    ((*"FOR TESTING: \n" ^ print_board board1 to_string "" ^ "\n\n" ^ *)
     print_board (board_open_to_close board1) to_string ""
    ^ "\n\n"
    ^ print_board board2 to_string "");
  print_string
    ("\nSCORE: "
    ^
    match (ships_to_score ships1, ships_to_score ships2) with
    | x, y when x > y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " You\n"
    | x, y when y > x ->
        string_of_int y ^ "-" ^ string_of_int x ^ " Player 1\n"
    | x, y when x == y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " TIED\n"
    | _, _ -> "TIED\n");
  print_string "Player 2's turn to attack\n";
  print_string "Where would you like to attack?\n";
  (match
     Game.Gui.attack
       (match read_line () with
       | "q" -> exit 0
       | "quit" -> exit 0
       | s -> s)
       board1 ships1
   with
  | Miss ->
      ANSITerminal.erase Screen;
      print_string "Aw man you missed!\n"
  | Hit ->
      ANSITerminal.erase Screen;
      print_string "Got a hit!\n"
  | Sink ->
      ANSITerminal.erase Screen;
      print_string "You sunk a battleship!\n");
  (*ANSITerminal.erase Above;*)
  if true <> check_for_ships board1 then (
    print_string "Congratulations, you win!\n";
    losses := !losses + 1;
    exit 0)
  else main_loop_coop board1 board2 ships1 ships2

and main_loop_bot board1 board2 ships1 ships2 : unit =
  ANSITerminal.erase Screen;
  print_string
    ((*"FOR TESTING: \n" ^ print_board board2 to_string "" ^ "\n\n" ^ *)
     print_board (board_open_to_close board2) to_string ""
    ^ "\n\n"
    ^ print_board board1 to_string "");
  print_string
    ("\nSCORE: "
    ^
    match (ships_to_score ships2, ships_to_score ships1) with
    | x, y when x > y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " You\n"
    | x, y when y > x ->
        string_of_int y ^ "-" ^ string_of_int x ^ " Bot\n"
    | x, y when x == y ->
        string_of_int x ^ "-" ^ string_of_int y ^ " TIED\n"
    | _, _ -> "TIED\n");
  print_string "Where would you like to attack?\n";
  (match
     Game.Gui.attack
       (match read_line () with
       | "q" -> exit 0
       | "quit" -> exit 0
       | s -> s)
       board2 ships2
   with
  | Miss ->
      ANSITerminal.erase Screen;
      print_string "Aw man you missed!\n"
  | Hit ->
      ANSITerminal.erase Screen;
      print_string "Got a hit!\n"
  | Sink ->
      ANSITerminal.erase Screen;
      print_string "You sunk a battleship!\n");
  (*ANSITerminal.erase Above;*)
  if true <> check_for_ships board2 then (
    print_string "Congratulations, you win!\n";
    wins := !wins + 1;
    exit 0)
  else
    print_string
      ((*"FOR TESTING: \n" ^ print_board board2 to_string "" ^ "\n\n"
         ^ *)
       print_board (board_open_to_close board2) to_string ""
      ^ "\n\n"
      ^ print_board board1 to_string "");
  print_string "Bot attacking...\n";
  (match !difficulty_ref with
  | Easy -> simple_attack board1 ships1
  | Medium -> medium_attack board1 ships1
  | Hard -> failwith "hard diff not implemented");
  (* (match Game.Battleship.attack (Game.Battleship.simple_bot ())
     board1 ships1 with | Miss -> print_string "The bot missed!\n" | Hit
     -> print_string "You got hit!\n" | Sink -> print_string "You lost a
     battleship!\n"); *)
  (*ANSITerminal.erase Above; *)
  if true <> check_for_ships board1 then (
    print_string "The bot wins!\n";
    losses := !losses + 1;
    end_match ())
  else main_loop_bot board1 board2 ships1 ships2

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Battleship Game engine.\n";

  (* Do any instantiation now *)
  choose_number ()

let x = main ()
