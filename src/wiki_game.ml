open! Core

let ( >> ) f g x = f x |> g

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> filter (fun a ->
       let link = R.attribute "href" a in
       let is_wiki = String.is_substring ~substring:"wiki/" link in
       let not_namespace =
         match Wikipedia_namespace.namespace link with
         | Some _special -> false
         | None -> true
       in
       is_wiki && not_namespace)
  |> to_list
  |> List.map ~f:(fun li -> R.attribute "href" li)
  |> List.dedup_and_sort ~compare:String.compare
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `Forward ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let format =
  String.chop_prefix_if_exists ~prefix:"/"
  >> String.chop_prefix_if_exists ~prefix:"wiki/"
  >> String.lstrip ?drop:None
  >> (String.tr_multi ~target:"-/()*" ~replacement:"_____" |> Staged.unstage)
  >> String.lstrip ?drop:None
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let rec linker ~link ~max_depth ~how_to_fetch =
  if max_depth <> 0
  then (
    let contents = File_fetcher.fetch_exn how_to_fetch ~resource:link in
    let links = get_linked_articles contents in
    List.concat_map links ~f:(fun sub_link ->
      [ format link, format sub_link ]
      @ linker ~link:sub_link ~max_depth:(max_depth - 1) ~how_to_fetch))
  else []
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let total_links =
    linker ~link:origin ~max_depth ~how_to_fetch
    |> List.dedup_and_sort ~compare:[%compare: string * string]
  in
  List.iter total_links ~f:(fun (link1, link2) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph
       if they don't already exist. *)
    G.add_edge graph link1 link2);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph;
  printf !"Done! Wrote dot file to %{File_path}\n%!" output_file
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
