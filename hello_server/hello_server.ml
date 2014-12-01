open Lwt
open Cohttp
open Cohttp_lwt_unix

let make_server () =
  let callback conn_id req body =
    let uri = Request.uri req in
    match Uri.path uri with
    | "/" -> Server.respond_string ~status:`OK ~body:"hello!\n" ()
    | _ -> Server.respond_string ~status:`Not_found ~body:"Route not found" ()
  in
  let conn_closed conn_id () = () in
  Server.create { Server.callback; conn_closed }

let _ =
  Lwt_unix.run (make_server ())
