open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  parse contents
  $$ "a[class]"
  |> filter (fun node ->
       R.attribute "class" node
       |> String.equal "ipc-primary-image-list-card__title")
  |> to_list
  |> List.map ~f:(fun node -> leaf_text node |> Option.value ~default:"")
  |> List.dedup_and_sort ~compare:String.compare
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
