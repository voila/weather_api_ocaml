(* ocamlbuild -use-ocamlfind weather.native *)
(* curl http://localhost:8080/weather/melbourne,australia *)

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Api
 


let make_server temperature =
 let callback conn_id req body =
  let uri = Request.uri req in
  match Re_str.(split_delim (regexp_string "/") (Uri.path uri)) with
  | ""::"weather"::city::_ -> 
    let open Response_j in 
    let open Core.Std in
    let t0 = Time.now () in
    temperature city >>= fun temp ->
    let t1 = Time.now () in
    let response = { city = city; temp = temp; 
                     took = Core.Span.to_string (Time.diff t1 t0); } in
    let json = string_of_response response in
    let headers = 
       Header.init_with "content-type" "application/json; charset=utf-8" in
     Server.respond_string ~headers ~status:`OK ~body:json ()
  | _ ->
    Server.respond_string ~status:`Not_found ~body:"Route not found" ()
 in
 let conn_closed conn_id () = () in
 Server.create { Server.callback; Server.conn_closed }


let _ = 
  Lwt_unix.run (make_server MW.temperature)


