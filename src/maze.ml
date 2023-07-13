open! Core

type t =
  | Wall
  | Start
  | End
  | Path

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let of_file input_file =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.foldi ~init:Position.Map.empty ~f:(fun y init line ->
       List.foldi (String.to_list line) ~init ~f:(fun x init value ->
         Map.add_exn
           init
           ~key:(x, y)
           ~data:
             (match value with
              | '#' -> Wall
              | 'S' -> Start
              | 'E' -> End
              | '.' -> Path
              | _ -> failwith "Not valid char")))
;;

let valid_neighbors ~maze ~curr_pos =
  let x, y = curr_pos in
  List.filter
    [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
    ~f:(fun position ->
      match Map.find maze position with
      | Some value -> (match value with Wall | Start -> false | _ -> true)
      | None -> false)
;;

let rec dfs ~maze ~curr ~path =
  match Map.find_exn maze curr with
  | End -> path @ [ curr ]
  | _ ->
    let neighbors =
      valid_neighbors ~maze ~curr_pos:curr
      |> List.filter ~f:(fun neighbor ->
           not (List.mem path neighbor ~equal:Position.equal))
    in
    if List.length neighbors = 0
    then []
    else (
      let temp_map =
        List.map neighbors ~f:(fun position ->
          dfs ~maze ~curr:position ~path:(path @ [ curr ]))
        |> List.filter ~f:(fun list -> List.length list > 0)
        (* |> List.sort ~compare:(fun list_1 list_2 -> Int.compare
           (List.length list_1) (List.length list_2)) *)
      in
      if List.length temp_map = 0 then [] else List.hd_exn temp_map)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze = of_file input_file in
        let curr =
          Map.filter maze ~f:(fun value ->
            match value with Start -> true | _ -> false)
          |> Map.keys
          |> List.hd_exn
        in
        let positions = dfs ~maze ~curr ~path:[] in
        print_s [%message "" (positions : Position.T.t list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
