(* ocamlbuild -use-ocamlfind weather.native *)
(* curl http://localhost:8080/weather/melbourne,australia *)

open Lwt
open Cohttp
open Cohttp_lwt_unix



module type Weather = sig
    (** get city's temperature in kelvin *)
    val get_temp : city:string -> float Lwt.t
  end


module OpenWeatherMap : Weather = struct
  open Openweathermap_j
  
  let get_temp ~city = 
  Client.get (Uri.of_string 
    ("http://api.openweathermap.org/data/2.5/weather?q=" ^ city))
  >>= fun (_, body) -> Cohttp_lwt_body.to_string body
  >>= fun s -> 
    let w = weather_of_string s in 
    let temp = w.main.temp in
    Lwt_io.printf "openWeatherMap: %s: %.2f\n" city temp 
  >>= fun _ -> return temp
  
end


module WeatherUnderground : Weather = struct
  open Weatherunderground_j
  
  let key = "..." (* your WU API key *)
  let kelvin_of_celsius t = t +. 273.15
  let get_temp ~city = 
  Client.get (Uri.of_string ("http://api.wunderground.com/api/" ^ 
    key ^ "/conditions/q/" ^ city ^ ".json"))
  >>= fun (_, body) -> Cohttp_lwt_body.to_string body
  >>= fun s -> 
    let c = conditions_of_string s in 
    let temp = kelvin_of_celsius c.current_observation.temp_c in
    Lwt_io.printf "WeatherUnderground: %s: %.2f\n" city temp 
  >>= fun _ -> return temp
  
end


    
let average xs =
  let sum = List.fold_left (+.) 0. xs in
  (sum /. float_of_int (List.length xs))
    

let temperature ~city ~providers =
  let open Response_j in 
  let open Core.Std in
    let t0 = Time.now () in
    Lwt_list.map_p (fun get_temp -> get_temp ~city) providers >>= 
      fun temps -> 
        let t1 = Time.now () in
        let response = { city = city; temp = average temps; 
                         took = Core.Span.to_string (Time.diff t1 t0); }
        in return (string_of_response response)



let make_server temp =
 let callback conn_id req body =
  let uri = Request.uri req in
  match Re_str.(split_delim (regexp_string "/") (Uri.path uri)) with
  | ""::"weather"::city::_ -> temp ~city >>= fun json ->
     let headers = 
       Header.init_with "content-type" "application/json; charset=utf-8" in
     Server.respond_string ~headers ~status:`OK ~body:json ()
  | _ ->
    Server.respond_string ~status:`Not_found ~body:"Route not found" ()
 in
 let conn_closed conn_id () = () in
 Server.create { Server.callback; Server.conn_closed }


let _ = 
  let providers = [OpenWeatherMap.get_temp; WeatherUnderground.get_temp;] 
  in Lwt_unix.run (make_server (temperature ~providers))
