(* ocamlbuild -use-ocamlfind weather.native *)
(* curl http://localhost:8080/weather/melbourne,australia *)

(* TODO:

error handling: Client.get throws exception 
curl http://localhost:8080/weather/melbourne,australia
Error: Failure("resolution failed: name resolution failed")

    val get_temp : city:string -> float option Lwt.t


 *)


open Lwt
open Cohttp
open Cohttp_lwt_unix


(* type weather_provider = < temperature : string -> float Lwt.t > *)

let open_weather_map = object
  method temperature city =
    let open Openweathermap_j in
    Client.get (Uri.of_string 
    ("http://api.openweathermap.org/data/2.5/weather?q=" ^ city))
    >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    >>= fun s -> return (string_of_weather (weather_of_string s))
end
 
(** web  server *)  
let make_server () =
 let callback conn_id req body =
  let uri = Request.uri req in
  match Re_str.(split_delim (regexp_string "/") (Uri.path uri)) with
  | ""::"weather"::city::_ -> open_weather_map#temperature city >>= fun json ->
     let headers = 
       Header.init_with "content-type" "application/json; charset=utf-8" in
     Server.respond_string ~headers ~status:`OK ~body:json ()
  | _ ->
    Server.respond_string ~status:`Not_found ~body:"Route not found" ()
 in
 let conn_closed conn_id () = () in
 Server.create { Server.callback; Server.conn_closed }


let _ = 
  Lwt_unix.run (make_server ())



