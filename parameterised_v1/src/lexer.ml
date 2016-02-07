module String = BatString


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type lexeme =
    | Begin of string
    | End of string
    | Simple of string
    | Open
    | Close
    | Eof
    | Eop
    | Space
    | Text of string

type accum =
    {
    buffer: Buffer.t;
    mutable nlines: int;
    }

type lexer = Sedlexing.lexbuf -> lexeme option * lexeme * int


(********************************************************************************)
(** {1 Debug facilities}                                                        *)
(********************************************************************************)

let string_of_lexeme = function
    | Begin txt  -> Printf.sprintf "BEGIN (%s)" txt
    | End txt    -> Printf.sprintf "END (%s)" txt
    | Simple txt -> Printf.sprintf "SIMPLE (%s)" txt
    | Open       -> Printf.sprintf "OPEN"
    | Close      -> Printf.sprintf "CLOSE"
    | Eof        -> Printf.sprintf "EOF"
    | Eop        -> Printf.sprintf "EOP"
    | Space      -> Printf.sprintf "SPACE"
    | Text txt   -> Printf.sprintf "TEXT (%s)" txt


(********************************************************************************)
(** {1 List of regular expressions used in the scanners}                        *)
(********************************************************************************)

let lower = [%sedlex.regexp? 'a' .. 'z']
let ident = [%sedlex.regexp? Plus lower]

let simple = [%sedlex.regexp? '\\', ident]
let open_marker = [%sedlex.regexp? '{']
let close_marker = [%sedlex.regexp? '}']
let begin_marker = [%sedlex.regexp? "\\begin", open_marker, ident, close_marker]
let end_marker = [%sedlex.regexp? "\\end", open_marker, ident, close_marker]

let space = [%sedlex.regexp? ' ']
let newline = [%sedlex.regexp? '\n']
let eop = [%sedlex.regexp? Star space, newline, Plus (Star space, newline), Star space]
let escape = [%sedlex.regexp? '\\']


(********************************************************************************)
(** {1 Auxiliary functions}                                                     *)
(********************************************************************************)

let whole_lexbuf lexbuf =
    Sedlexing.Utf8.lexeme lexbuf

let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf left (Sedlexing.lexeme_length lexbuf - left - right)

let sub_lexbuf ~pos ~len lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf pos len

let count_newlines lexbuf =
    let adder acc el = if el = 0x0a then acc+1 else acc in
    Array.fold_left adder 0 (Sedlexing.lexeme lexbuf)

let make_accum () =
    {
    buffer = Buffer.create 80;
    nlines = 0;
    }

let return ?lexbuf accum this_lexeme =
    let this_nlines = match lexbuf with
        | Some lexbuf -> count_newlines lexbuf
        | None        -> 0 in
    let accum_lexeme =
        if Buffer.length accum.buffer = 0
        then None
        else Some (Text (Buffer.contents accum.buffer)) in
    (accum_lexeme, this_lexeme, accum.nlines + this_nlines)


(********************************************************************************)
(** {1 Actual scanners}                                                         *)
(********************************************************************************)

let general lexbuf =
    Printf.eprintf "&&& LEXER: general\n%!";
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | begin_marker, Star space, Opt newline ->
            return ~lexbuf accum (Begin (trim_lexbuf ~left:7 lexbuf |> String.trim |> String.rchop))
        | end_marker, Star space, Opt newline ->
            return ~lexbuf accum (End (trim_lexbuf ~left:5 lexbuf |> String.trim |> String.rchop))
        | simple ->
            return accum (Simple (trim_lexbuf ~left:1 lexbuf))
        | open_marker ->
            return accum Open
        | close_marker ->
            return accum Close
        | eof ->
            return accum Eof
        | eop ->
            return ~lexbuf accum Eop
        | Plus (space | newline) ->
            if Buffer.length accum.buffer = 0
            then
                (None, Space, count_newlines lexbuf)
            else begin
                Buffer.add_char accum.buffer ' ';
                accum.nlines <- accum.nlines + count_newlines lexbuf;
                loop ()
            end
        | escape, newline ->
            accum.nlines <- accum.nlines + 1;
            loop ()
        | escape, any ->
            Buffer.add_string accum.buffer (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false in
    loop ()


let raw lexbuf =
    Printf.eprintf "&&& LEXER: raw\n%!";
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | close_marker ->
            return accum Close
        | eof ->
            return accum Eof
        | escape, newline ->
            accum.nlines <- accum.nlines + 1;
            loop ()
        | newline ->
            Buffer.add_char accum.buffer '\n';
            accum.nlines <- accum.nlines + 1;
            loop ()
        | escape, any ->
            Buffer.add_string accum.buffer (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false in
    loop ()


let literal terminator lexbuf =
    Printf.eprintf "&&& LEXER: literal (%s)\n%!" terminator;
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | Opt newline, end_marker, Star space, Opt newline ->
            if (whole_lexbuf lexbuf |> String.trim |> String.slice ~first:5 ~last:(-1)) = terminator
            then
                return ~lexbuf accum (End terminator)
            else begin
                Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
                loop ()
            end
        | eof ->
            return accum Eof
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false in
    loop ()

