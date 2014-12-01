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
open Openweathermap_j

    
let query city = 
  Client.get (Uri.of_string 
    ("http://api.openweathermap.org/data/2.5/weather?q=" ^ city))
  >>= fun (_, body) -> Cohttp_lwt_body.to_string body
  >>= fun s -> return (string_of_weather (weather_of_string s))

(** web  server *)  
let make_server () =
 let callback conn_id req body =
  let uri = Request.uri req in
  match Re_str.(split_delim (regexp_string "/") (Uri.path uri)) with
  | ""::"weather"::city::_ -> query city >>= fun json ->
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





(*

(** Weather provider *)
module type WEATHER = sig
    (** get city's temperature in celsius *)
    val get_temp : city:string -> float Lwt.t
end


(** OpenWeatherMap Provider *)
module OpenWeatherMap : WEATHER = struct

  let celsius_of_kelvin t = t -. 273.15

  let call_api uri_str = 
   (* try_lwt  *)
     Client.get (Uri.of_string uri_str) >>= fun (_, body) -> 
   Cohttp_lwt_body.to_string body
   (* with e -> None *)

  let temp_of_json ~json = 
    let open Ezjsonm in 
    get_dict (from_string json)
    |> List.assoc "main" |> get_dict 
    |> List.assoc "temp" |> get_float |> celsius_of_kelvin 
  
  let get_temp ~city = 
    let uri = "http://api.openweathermap.org/data/2.5/weather?q=" ^ city in
    call_api uri >>= fun j -> return (temp_of_json j)

end


(** WeatherUnderground Provider *)
module WeatherUnderground : WEATHER = struct
  
let key = "aac6f09bc1d32c24"

  let celsius_of_kelvin t = t -. 273.15

  let call_api uri_str =
    Client.get (Uri.of_string uri_str) >>= fun (_, body) -> 
    Cohttp_lwt_body.to_string body
  
  let temp_of_json json = 
    let open Ezjsonm in
    get_dict (from_string json) 
    |> List.assoc "current_observation" |> get_dict 
    |> List.assoc "temp_c" |> get_float 

  let get_temp ~city = 
    let uri =  "http://api.wunderground.com/api/" ^ key ^ 
                 "/conditions/q/" ^ city ^ ".json" 
    in
    call_api uri >>= fun j -> return (temp_of_json j)

end

*)

(*
let _ = 
  let temperature providers city =
    let time_since start = 
      Printf.sprintf "%.02f ms" ((Unix.gettimeofday () -. start) *. 1000.)
    in  
    let json_of city temp start = 
      let open Ezjsonm in 
      to_string(`O [("city", `String city); 
                    ("temp", `Float temp);
                    ("took", `String (time_since start))])
    in
    let average temps =
      let sum, len = 
        List.fold_left (fun (s,l) t -> (s +. t, l + 1)) (0.0, 0) temps in
      (sum /. float_of_int len)
    in
    let start = Unix.gettimeofday () in
    Lwt_list.map_p (fun get_temp -> get_temp ~city) providers >>= 
      fun temps -> return (json_of city (average temps) start)
  in
  let providers = [OpenWeatherMap.get_temp; WeatherUnderground.get_temp;] 
  in
  Lwt_unix.run make_server (temperature providers)

*)
 
