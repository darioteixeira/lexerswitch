open Lexing
open Parser


let make_position =
    let lnum = ref 0 in
    fun () ->
        {
        pos_fname = "";
        pos_lnum = (incr lnum; !lnum);
        pos_bol = 0;
        pos_cnum = 0;
        }


let main tokenizer =
    let open MenhirInterpreter in
    let rec loop result = match result with
        | InputNeeded env ->
            Printf.eprintf "@@@ InputNeeded: %s\n%!" (Tokenizer.string_of_env env);
            let ante_position = make_position () in
            let post_position = make_position () in
            let token = Tokenizer.next_token tokenizer env in
            let result = MenhirInterpreter.offer result (token, ante_position, post_position) in
            loop result
        | Shifting (env1, env2, _) ->
            Printf.eprintf "@@@ Shifting (before): %s\n%!" (Tokenizer.string_of_env env1);
            Printf.eprintf "@@@ Shifting (after): %s\n%!" (Tokenizer.string_of_env env2);
            let result = MenhirInterpreter.resume result in
            loop result
        | AboutToReduce (env, _) ->
            Printf.eprintf "@@@ AboutToReduce: %s\n%!" (Tokenizer.string_of_env env);
            let result = MenhirInterpreter.resume result in
            loop result
        | HandlingError env ->
            Printf.eprintf "@@@ HandlingError: %s\n%!" (Tokenizer.string_of_env env);
            let result = MenhirInterpreter.resume result in
            loop result
        | Accepted res ->
            Printf.eprintf "@@@ Accepted\n%!";
            res
        | Rejected ->
            Printf.eprintf "@@@ Rejected\n%!";
            assert false in
	Incremental.main () |> loop
    

let () =
	let lexbuf = Sedlexing.Utf8.from_channel Pervasives.stdin in
    let tokenizer = Tokenizer.make lexbuf in
    main tokenizer |> Ast.sprint |> print_endline

