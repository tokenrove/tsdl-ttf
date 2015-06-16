open Tsdl
open Tsdl_ttf

let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a

let unwind ~(protect:'a -> unit) f x =
  try let y = f x in protect x; y
  with e -> protect x; raise e

let () =
  Sdl.init Sdl.Init.everything >>= fun () ->
  Ttf.init () >>= fun () ->
  assert (Ttf.was_init ());
  let Some font = Ttf.open_font "f500.ttf" 72 in

  let display_width = 640 in
  let display_height = 480 in
  Sdl.create_window_and_renderer ~w:display_width ~h:display_height Sdl.Window.windowed
  >>= fun (window, renderer) ->
  Sdl.render_set_logical_size renderer display_width display_height >>= fun () ->
  Sdl.get_window_surface window >>= fun display ->
  let e = Sdl.Event.create () in
  let r = Sdl.Rect.create 0 0 0 0 in
  let fg_color = Sdl.Color.create 255 255 255 255 in
  let rec loop () =
    Sdl.fill_rect display None 0l >>= fun () ->
    let Some sface = Ttf.render_text_solid font "foobar" fg_color in
    Sdl.blit_surface sface None display r >>= fun () ->
    Sdl.update_window_surface window >>= fun () ->
    match Sdl.wait_event (Some e) with
    | `Error err -> Sdl.log "Could not wait event: %s" err; ()
    | `Ok () ->
      match Sdl.Event.(enum (get e typ)) with
      | `Quit | `Key_down -> ()
      | _ -> loop ()
  in
  unwind ~protect:(fun () ->
      Sdl.destroy_window window;
      Ttf.quit ();
      Sdl.quit ()) loop ()
