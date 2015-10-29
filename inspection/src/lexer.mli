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

type lexer = Sedlexing.lexbuf -> lexeme option * lexeme * int


(********************************************************************************)
(** {1 Debug facilities}                                                        *)
(********************************************************************************)

val string_of_lexeme: lexeme -> string


(********************************************************************************)
(** {1 Actual lexers}                                                           *)
(********************************************************************************)

val general: lexer
val raw: lexer
val literal: string -> lexer

