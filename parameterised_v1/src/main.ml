open Lexing

let make_position =
    let lnum = ref 0 in
    fun () ->
        {
        pos_fname = "";
        pos_lnum = (incr lnum; !lnum);
        pos_bol = 0;
        pos_cnum = 0;
        }

let () =
    let module C = Context.Make (struct end) in 
    let module P = Parser.Make (C) in
    let module T = Tokenizer.Make (C) (P) in
	let lexbuf = Sedlexing.Utf8.from_channel Pervasives.stdin in
    let tokenizer = T.make lexbuf in
    let lexer_maker () =
        let ante_position = make_position () in
        let token = T.next_token tokenizer in
        let post_position = make_position () in
        (token, ante_position, post_position) in
    MenhirLib.Convert.Simplified.traditional2revised P.main lexer_maker |> Ast.sprint |> print_endline

