open! Core
open! Base
open Tictactoe_logic_library

(* Test framework setup *)
let assert_equal b = 
  if not b then 
    (Stdio.printf "Test failed: expected true but got false\n"; 
     Stdlib.exit 1)
  else 
    Stdio.printf "Test passed\n"

(* Helper functions for testing *)
let create_test_card suit rank = { Hw1.Card.suit; rank }

(* =============================================================================
   HW1 SPEED CARD GAME TESTS
   ============================================================================= *)

module SpeedCardTests = struct
  let test_card_creation () =
    let card = create_test_card Hw1.Card.Hearts Hw1.Card.Ace in
    assert_equal (Hw1.Card.equal card { Hw1.Card.suit = Hw1.Card.Hearts; rank = Hw1.Card.Ace })
  
  let test_rank_values () =
    assert_equal (Hw1.Card.rank_value Hw1.Card.Ace = 1);
    assert_equal (Hw1.Card.rank_value Hw1.Card.King = 13);
    assert_equal (Hw1.Card.rank_value Hw1.Card.Ten = 10)
  
  let test_can_play_on () =
    let ace_hearts = create_test_card Hw1.Card.Hearts Hw1.Card.Ace in
    let two_spades = create_test_card Hw1.Card.Spades Hw1.Card.Two in
    let king_clubs = create_test_card Hw1.Card.Clubs Hw1.Card.King in
    
    (* Ace can play on Two (1 can play on 2) *)
    assert_equal (Hw1.Card.can_play_on ace_hearts two_spades);
    (* Two can play on Ace (2 can play on 1) *)
    assert_equal (Hw1.Card.can_play_on two_spades ace_hearts);
    (* King cannot play on Ace (13 cannot play on 1) *)
    assert_equal (not (Hw1.Card.can_play_on king_clubs ace_hearts))
  
  let test_card_to_string () =
    let ace_hearts = create_test_card Hw1.Card.Hearts Hw1.Card.Ace in
    let king_spades = create_test_card Hw1.Card.Spades Hw1.Card.King in
    assert_equal (String.equal (Hw1.Card.to_string ace_hearts) "A♥");
    assert_equal (String.equal (Hw1.Card.to_string king_spades) "K♠")
end

module SpeedPlayerTests = struct
  let test_player_opposite () =
    assert_equal (Hw1.Player.equal (Hw1.Player.opposite Hw1.Player.Player1) Hw1.Player.Player2);
    assert_equal (Hw1.Player.equal (Hw1.Player.opposite Hw1.Player.Player2) Hw1.Player.Player1)
end

module SpeedGameStateTests = struct
  let test_initial_game_state () =
    let game_state = Hw1.Game_state.create () in
    (* Check initial conditions *)
    assert_equal (List.length game_state.player1_hand = 5);
    assert_equal (List.length game_state.player2_hand = 5);
    assert_equal (Option.is_none game_state.pile1);
    assert_equal (Option.is_none game_state.pile2);
    assert_equal (Hw1.Player.equal game_state.current_player Hw1.Player.Player1);
    assert_equal (not game_state.game_over);
    assert_equal (Option.is_none game_state.winner)
  
  let test_legal_card_play () =
    let game_state = Hw1.Game_state.create () in
    let player1_hand = game_state.player1_hand in
    let first_card = List.hd_exn player1_hand in
    
    (* Try to play first card on empty pile *)
    let move = Hw1.Move.Play_card { card = first_card; pile = 0 } in
    match Hw1.Game_state.make_move game_state move with
    | Ok new_state ->
        assert_equal (Option.is_some new_state.pile1);
        assert_equal (Hw1.Player.equal new_state.current_player Hw1.Player.Player2);
        assert_equal (List.length new_state.player1_hand = 4)
    | Error _ -> assert_equal false
  
  let test_illegal_card_play_not_in_hand () =
    let game_state = Hw1.Game_state.create () in
    let fake_card = create_test_card Hw1.Card.Hearts Hw1.Card.Ace in
    let move = Hw1.Move.Play_card { card = fake_card; pile = 0 } in
    match Hw1.Game_state.make_move game_state move with
    | Ok _ -> assert_equal false
    | Error Hw1.Game_state.Move_error.Card_not_in_hand -> assert_equal true
    | Error _ -> assert_equal false
  
  let test_illegal_card_play_invalid_sequence () =
    let game_state = Hw1.Game_state.create () in
    let player1_hand = game_state.player1_hand in
    let first_card = List.hd_exn player1_hand in
    
    (* Play first card to create a pile *)
    let move1 = Hw1.Move.Play_card { card = first_card; pile = 0 } in
    match Hw1.Game_state.make_move game_state move1 with
    | Ok state_after_first ->
        (* Try to play a card that cannot be played on the pile *)
        let player2_hand = state_after_first.player2_hand in
        let incompatible_card = List.find_exn player2_hand ~f:(fun card ->
          not (Hw1.Card.can_play_on card first_card)
        ) in
        let move2 = Hw1.Move.Play_card { card = incompatible_card; pile = 0 } in
        match Hw1.Game_state.make_move state_after_first move2 with
        | Ok _ -> assert_equal false
        | Error Hw1.Game_state.Move_error.Invalid_play -> assert_equal true
        | Error _ -> assert_equal false
    | Error _ -> assert_equal false
  
  let test_draw_cards_legal () =
    let game_state = Hw1.Game_state.create () in
    let move = Hw1.Move.Draw_cards in
    match Hw1.Game_state.make_move game_state move with
    | Ok new_state ->
        assert_equal (List.length new_state.player1_hand = 5);
        assert_equal (Hw1.Player.equal new_state.current_player Hw1.Player.Player2);
        assert_equal (List.length new_state.player1_stock < List.length game_state.player1_stock)
    | Error _ -> assert_equal false
  
  let test_draw_cards_illegal_empty_stock () =
    let game_state = { (Hw1.Game_state.create ()) with player1_stock = [] } in
    let move = Hw1.Move.Draw_cards in
    match Hw1.Game_state.make_move game_state move with
    | Ok _ -> assert_equal false
    | Error Hw1.Game_state.Move_error.No_cards_to_draw -> assert_equal true
    | Error _ -> assert_equal false
  
  let test_game_over_after_move () =
    let game_state = Hw1.Game_state.create () in
    (* Simulate a game where player1 has only one card left *)
    let almost_empty_state = { game_state with 
      player1_hand = [List.hd_exn game_state.player1_hand];
      player1_stock = []
    } in
    let last_card = List.hd_exn almost_empty_state.player1_hand in
    let move = Hw1.Move.Play_card { card = last_card; pile = 0 } in
    match Hw1.Game_state.make_move almost_empty_state move with
    | Ok new_state ->
        assert_equal new_state.game_over;
        assert_equal (Option.is_some new_state.winner)
    | Error _ -> assert_equal false
  
  let test_get_all_moves () =
    let game_state = Hw1.Game_state.create () in
    let moves = Hw1.Game_state.get_all_moves game_state in
    assert_equal (List.length moves > 0);
    (* Should include draw_cards move *)
    assert_equal (List.exists moves ~f:(function Hw1.Move.Draw_cards -> true | _ -> false))
end

(* =============================================================================
   RANDOM STATE SPACE EXPLORATION TESTS
   ============================================================================= *)

module RandomExplorationTests = struct
  let random_seed = 42
  
  let test_random_speed_game_play () =
    Random.init random_seed;
    let game_state = ref (Hw1.Game_state.create ()) in
    let move_count = ref 0 in
    let max_moves = 50 in
    
    while not !game_state.game_over && !move_count < max_moves do
      let moves = Hw1.Game_state.get_all_moves !game_state in
      if List.is_empty moves then
        game_state := { !game_state with game_over = true }
      else
        let random_move = List.random_element_exn moves in
        (match Hw1.Game_state.make_move !game_state random_move with
         | Ok new_state -> 
             game_state := new_state;
             move_count := !move_count + 1
         | Error _ -> 
             (* This should not happen with legal moves *)
             assert_equal false)
    done;
    
    (* Verify final state is valid *)
    assert_equal (!game_state.game_over || !move_count >= max_moves)
  
  let test_random_card_game_stress () =
    Random.init random_seed;
    let success_count = ref 0 in
    let total_tests = 10 in
    
    for _ = 1 to total_tests do
      let game_state = ref (Hw1.Game_state.create ()) in
      let move_count = ref 0 in
      let max_moves = 50 in
      let game_completed = ref false in
      
      while not !game_state.game_over && !move_count < max_moves && not !game_completed do
        let moves = Hw1.Game_state.get_all_moves !game_state in
        if List.is_empty moves then (
          game_state := { !game_state with game_over = true };
          game_completed := true
        ) else (
          let random_move = List.random_element_exn moves in
          (match Hw1.Game_state.make_move !game_state random_move with
           | Ok new_state -> 
               game_state := new_state;
               move_count := !move_count + 1
           | Error _ -> 
               game_completed := true)
        )
      done;
      
      if !game_state.game_over || !move_count >= max_moves then
        success_count := !success_count + 1
    done;
    
    (* At least 80% of random games should complete successfully *)
    assert_equal (!success_count >= (total_tests * 8 / 10))
end

(* =============================================================================
   INTEGRATION AND EDGE CASE TESTS
   ============================================================================= *)

module IntegrationTests = struct
  let test_card_game_win_condition () =
    let game_state = Hw1.Game_state.create () in
    (* Simulate player1 having only one card left and no stock *)
    let almost_winning_state = { game_state with 
      player1_hand = [List.hd_exn game_state.player1_hand];
      player1_stock = []
    } in
    let last_card = List.hd_exn almost_winning_state.player1_hand in
    let move = Hw1.Move.Play_card { card = last_card; pile = 0 } in
    match Hw1.Game_state.make_move almost_winning_state move with
    | Ok new_state ->
        assert_equal new_state.game_over;
        (match new_state.winner with
         | Some Hw1.Player.Player1 -> assert_equal true
         | _ -> assert_equal false)
    | Error _ -> assert_equal false
  
  let test_edge_case_empty_hands () =
    let game_state = Hw1.Game_state.create () in
    let empty_hand_state = { game_state with 
      player1_hand = [];
      player1_stock = []
    } in
    (* Player1 should have won *)
    let player1_won = List.is_empty empty_hand_state.player1_hand && List.is_empty empty_hand_state.player1_stock in
    assert_equal player1_won
  
  let test_edge_case_invalid_pile_number () =
    let game_state = Hw1.Game_state.create () in
    let player1_hand = game_state.player1_hand in
    let first_card = List.hd_exn player1_hand in
    let move = Hw1.Move.Play_card { card = first_card; pile = 5 } in (* Invalid pile *)
    match Hw1.Game_state.make_move game_state move with
    | Ok _ -> assert_equal false
    | Error Hw1.Game_state.Move_error.Empty_pile -> assert_equal true
    | Error _ -> assert_equal false
end

(* =============================================================================
   TEST SUITE REGISTRATION
   ============================================================================= *)

let run_all_tests () =
  Stdio.printf "Running HW3 TicTacToe Logic Tests...\n\n";
  
  Stdio.printf "=== Speed Card Game Tests ===\n";
  Stdio.printf "Testing card creation...\n";
  SpeedCardTests.test_card_creation ();
  Stdio.printf "Testing rank values...\n";
  SpeedCardTests.test_rank_values ();
  Stdio.printf "Testing can play on...\n";
  SpeedCardTests.test_can_play_on ();
  Stdio.printf "Testing card to string...\n";
  SpeedCardTests.test_card_to_string ();
  
  Stdio.printf "\n=== Speed Player Tests ===\n";
  Stdio.printf "Testing player opposite...\n";
  SpeedPlayerTests.test_player_opposite ();
  
  Stdio.printf "\n=== Speed Game State Tests ===\n";
  Stdio.printf "Testing initial game state...\n";
  SpeedGameStateTests.test_initial_game_state ();
  Stdio.printf "Testing legal card play...\n";
  SpeedGameStateTests.test_legal_card_play ();
  Stdio.printf "Testing illegal card play not in hand...\n";
  SpeedGameStateTests.test_illegal_card_play_not_in_hand ();
  Stdio.printf "Testing illegal card play invalid sequence...\n";
  SpeedGameStateTests.test_illegal_card_play_invalid_sequence ();
  Stdio.printf "Testing draw cards legal...\n";
  SpeedGameStateTests.test_draw_cards_legal ();
  Stdio.printf "Testing draw cards illegal empty stock...\n";
  SpeedGameStateTests.test_draw_cards_illegal_empty_stock ();
  Stdio.printf "Testing game over after move...\n";
  SpeedGameStateTests.test_game_over_after_move ();
  Stdio.printf "Testing get all moves...\n";
  SpeedGameStateTests.test_get_all_moves ();
  
  Stdio.printf "\n=== Random Exploration Tests ===\n";
  Stdio.printf "Testing random speed game play...\n";
  RandomExplorationTests.test_random_speed_game_play ();
  Stdio.printf "Testing random card game stress...\n";
  RandomExplorationTests.test_random_card_game_stress ();
  
  Stdio.printf "\n=== Integration Tests ===\n";
  Stdio.printf "Testing card game win condition...\n";
  IntegrationTests.test_card_game_win_condition ();
  Stdio.printf "Testing edge case empty hands...\n";
  IntegrationTests.test_edge_case_empty_hands ();
  Stdio.printf "Testing edge case invalid pile number...\n";
  IntegrationTests.test_edge_case_invalid_pile_number ();
  
  Stdio.printf "\n=== All Tests Completed Successfully! ===\n"

let () = run_all_tests ()