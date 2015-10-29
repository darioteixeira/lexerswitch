module Make (C: Context.S) (P: module type of Parser.Make (C)) =
struct
    open Lexer
    open P

    type tokenizer =
        {
        lexbuf: Sedlexing.lexbuf;
        mutable waiting: Lexer.lexeme option;
        mutable literal: string option;
        }

    let token_of_lexeme tokenizer = function
        | Begin "p"     -> BEGIN_PARAGRAPH
        | Begin "q"     -> BEGIN_QUOTE
        | Begin "v"     -> tokenizer.literal <- Some "v"; BEGIN_VERBATIM
        | Begin "s"     -> tokenizer.literal <- Some "s"; BEGIN_SOURCE
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

    let next_lexeme tokenizer = match tokenizer.waiting with
        | Some lexeme ->
            tokenizer.waiting <- None;
            lexeme
        | None ->
            let lexer = match C.get () with
                | C.General -> Lexer.general
                | C.Raw     -> Lexer.raw
                | C.Literal -> match tokenizer.literal with
                    | Some l -> Lexer.literal l
                    | None   -> assert false in
            let (prev_lexeme, this_lexeme, nlines) = lexer tokenizer.lexbuf in
            match prev_lexeme with
                | Some prev_lexeme ->
                    tokenizer.waiting <- Some this_lexeme;
                    prev_lexeme
                | None ->
                    this_lexeme

    let next_token tokenizer =
        let lexeme = next_lexeme tokenizer in
        Printf.eprintf "$$$ NEXT_TOKEN: %s\n%!" (Lexer.string_of_lexeme lexeme);
        token_of_lexeme tokenizer lexeme

    let make lexbuf =
        {
        lexbuf;
        waiting = None;
        literal = None;
        }
end

