open! Core
open Speed_logic_library

(* Simple test for alpha-beta search on Speed card game *)
let test_alpha_beta_search () =
  let game_state = Hw2_speed_logic.Game_state.create () in
  match Hw4_speed_alpha_beta.alpha_beta game_state ~depth:3 with
  | Some move -> 
      Stdio.printf "Alpha-beta found move: %s\n" 
        (Sexp.to_string (Hw2_speed_logic.Move.sexp_of_t move))
  | None -> 
      Stdio.printf "Alpha-beta found no move\n"

let run_alpha_beta_tests () =
  Stdio.printf "Running Speed Alpha-Beta Tests...\n";
  test_alpha_beta_search ();
  Stdio.printf "Alpha-beta tests completed\n"

let () = run_alpha_beta_tests ()