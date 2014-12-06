open Lwt
open Cohttp
open Cohttp_lwt_unix




module type WeatherProvider =
sig 
  val temperature: string -> float Lwt.t 
end 





module WeatherUnderground : WeatherProvider =
struct
  let kelvin_of_celsius t = t +. 273.15

  let key = "aac6f09bc1d32c24"

  let name = "WeatherUnderground"

  let uri city = "http://api.wunderground.com/api/" ^ 
    key ^ "/conditions/q/" ^ city ^ ".json"

  let temperature city = 
  Client.get (Uri.of_string (uri city))
  >>= fun (_, body) -> Cohttp_lwt_body.to_string body
  >>= fun s -> 
    let open Weatherunderground_j in
    let c = conditions_of_string s in
    let temp = kelvin_of_celsius c.current_observation.temp_c in
    let name = c.current_observation.display_location.full in
    Lwt_io.printf "%s: %.2f\n" name temp 
  >>= fun _ -> return temp  
end


module OpenWeatherMap : WeatherProvider =
struct
  let name = "OpenWeatherMap"

  let uri city =
    "http://api.openweathermap.org/data/2.5/weather?q=" ^ city
  
  let temperature city = 
  Client.get (Uri.of_string (uri city))
  >>= fun (_, body) -> Cohttp_lwt_body.to_string body
  >>= fun s -> 
    let open Openweathermap_j in
    let w = weather_of_string s in
    let temp = w.main.temp in
    Lwt_io.printf "%s: %.2f\n" w.name temp 
  >>= fun _ -> return temp
  end



module MultipleWeather (M1 : WeatherProvider) 
                       (M2 : WeatherProvider) : WeatherProvider = 
struct
  let average xs =
    let sum = List.fold_left (+.) 0. xs in
    (sum /. float_of_int (List.length xs))
    
  let temperature city =
    Lwt_list.map_p (fun gt -> gt city) [M1.temperature; M2.temperature] >>= 
      fun temps -> return (average temps)
      
end

module MW = MultipleWeather (OpenWeatherMap) (WeatherUnderground) 

