open Parser
open Lexer

module MI = MenhirInterpreter
module MG = MenhirLib.General

type context = General | Raw | Literal of string

type tokenizer =
    {
    lexbuf: Sedlexing.lexbuf;
    mutable context: context;
    mutable waiting: Lexer.lexeme option;
    }

let make lexbuf =
    {
    lexbuf;
    context = General;
    waiting = None;
    }

let token_of_lexeme = function
    | Begin "p"     -> BEGIN_PARAGRAPH
    | Begin "q"     -> BEGIN_QUOTE
    | Begin "v"     -> BEGIN_VERBATIM
    | Begin "s"     -> BEGIN_SOURCE
    | Begin _       -> assert false
    | End "p"       -> END_PARAGRAPH
    | End "q"       -> END_QUOTE
    | End "v"       -> END_VERBATIM
    | End "s"       -> END_SOURCE
    | End _         -> assert false
    | Simple "bold" -> BOLD
    | Simple "link" -> LINK
    | Simple _      -> assert false
    | Open          -> OPEN
    | Close         -> CLOSE
    | Eof           -> EOF
    | Eop           -> EOP
    | Space         -> SPACE
    | Text txt      -> TEXT txt


let string_of_env env =
    let stack = MI.stack env in
    let debug_symbol: type a. a MI.symbol -> string = function
        | MI.T t ->
            begin match t with
                | MI.T_error           -> "T_error"
                | MI.T_TEXT            -> "T_TEXT"
                | MI.T_SPACE	       -> "T_SPACE"
                | MI.T_OPEN	           -> "T_OPEN"
                | MI.T_LINK	           -> "T_LINK"
                | MI.T_EOP	           -> "T_EOP"
                | MI.T_EOF	           -> "T_EOF"
                | MI.T_END_VERBATIM	   -> "T_END_VERBATIM"
                | MI.T_END_SOURCE	   -> "T_END_SOURCE"
                | MI.T_END_QUOTE	   -> "T_END_QUOTE"
                | MI.T_END_PARAGRAPH   -> "T_END_PARAGRAPH"
                | MI.T_CLOSE	       -> "T_CLOSE"
                | MI.T_BOLD	           -> "T_BOLD"
                | MI.T_BEGIN_VERBATIM  -> "T_BEGIN_VERBATIM"
                | MI.T_BEGIN_SOURCE	   -> "T_BEGIN_SOURCE"
                | MI.T_BEGIN_QUOTE	   -> "T_BEGIN_QUOTE"
                | MI.T_BEGIN_PARAGRAPH -> "T_BEGIN_PARAGRAPH"
            end
        | MI.N n ->
            begin match n with
                | MI.N_wrapped_block	     -> "N_wrapped_block"
                | MI.N_spaceish	             -> "N_spaceish"
                | MI.N_raw_bundle	         -> "N_raw_bundle"
                | MI.N_option_spaceish_	     -> "N_option_spaceish_"
                | MI.N_option_inline_bundle_ -> "N_option_inline_bundle_"
                | MI.N_main	                 -> "N_main"
                | MI.N_list_wrapped_block_   -> "N_list_wrapped_block_"
                | MI.N_inline_without_space  -> "N_inline_without_space"
                | MI.N_inline_bundle	     -> "N_inline_bundle"
                | MI.N_inline_list	         -> "N_inline_list"
                | MI.N_inline	             -> "N_inline"
                | MI.N_frag	                 -> "N_frag"
                | MI.N_block	             -> "N_block"
            end in
    let f (MI.Element (s, v, _, _)) acc =
        let sym = MI.incoming_symbol s in
        debug_symbol sym :: acc in
    MenhirLib.General.foldr f stack [] |> String.concat "; "

    
let next_lexeme tokenizer env = match tokenizer.waiting with
    | Some lexeme ->
        tokenizer.waiting <- None;
        lexeme
    | None ->
        let lexer = match tokenizer.context with
            | General            -> Lexer.general
            | Raw                -> Lexer.raw
            | Literal terminator -> Lexer.literal terminator in
        let (prev_lexeme, this_lexeme, nlines) = lexer tokenizer.lexbuf in
        match prev_lexeme with
            | Some prev_lexeme ->
                tokenizer.waiting <- Some this_lexeme;
                prev_lexeme
            | None ->
                this_lexeme


let set_context tokenizer env =
    let next_symbol: MI.element MG.head Lazy.t -> ('a * MI.element MG.stream) option = function
        | lazy MG.Nil                                      -> None
        | lazy (MG.Cons (MI.Element (state, _, _, _), tl)) -> Some (MI.X (MI.incoming_symbol state), tl) in
    let stack = MI.stack env in
    match next_symbol stack with
        | Some (MI.X MI.T MI.T_BEGIN_VERBATIM, _) ->
            Printf.eprintf "$$$ tokenizer.context is now Literal (v)\n%!";
            tokenizer.context <- Literal "v"
        | Some (MI.X MI.T MI.T_BEGIN_SOURCE, _) ->
            Printf.eprintf "$$$ tokenizer.context is now Literal (s)\n%!";
            tokenizer.context <- Literal "s"
        | Some (MI.X MI.T MI.T_END_VERBATIM, _) ->
            Printf.eprintf "$$$ tokenizer.context is now General\n%!";
            tokenizer.context <- General
        | Some (MI.X MI.T MI.T_END_SOURCE, _) ->
            Printf.eprintf "$$$ tokenizer.context is now General\n%!";
            tokenizer.context <- General
        | Some (MI.X MI.T MI.T_OPEN, tl) ->
            begin match next_symbol tl with
                | Some (MI.X MI.T MI.T_LINK, _) ->
                    Printf.eprintf "$$$ tokenizer.context is now Raw\n%!";
                    tokenizer.context <- Raw
                | _ ->
                    ()
            end
        | Some (MI.X MI.T MI.T_CLOSE, _) ->
            Printf.eprintf "$$$ tokenizer.context is now General\n%!";
            tokenizer.context <- General
        | _ ->
            ()


let next_token tokenizer env =
    set_context tokenizer env;
    let lexeme = next_lexeme tokenizer env in
    Printf.eprintf "$$$ NEXT_TOKEN: %s\n%!" (Lexer.string_of_lexeme lexeme);
    token_of_lexeme lexeme

