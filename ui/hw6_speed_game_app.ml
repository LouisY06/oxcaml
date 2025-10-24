open! Core
open! Bonsai_web

let () =
  Bonsai_web.Start.start
    ~bind_to_element_with_id:"app"
    Hw6_speed_game_ui.app
