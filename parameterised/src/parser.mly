%token BEGIN_PARAGRAPH END_PARAGRAPH
%token BEGIN_QUOTE END_QUOTE
%token BEGIN_VERBATIM END_VERBATIM
%token BEGIN_SOURCE END_SOURCE
%token OPEN CLOSE
%token EOP EOF
%token SPACE
%token <string> TEXT
%token BOLD
%token LINK

%nonassoc inline_SPACE
%nonassoc TEXT

%parameter <C: Context.S>

%start main

%type <Ast.t> main

%%

main:
    | frag EOF                                                  {$1}

frag:
    | spaceish? wrapped_block*                                  {$2}

spaceish:
    | SPACE                                                     {()}
    | EOP                                                       {()}

wrapped_block:
    | block spaceish?                                           {$1}

block:
    | inline_without_space inline* EOP                          {Ast.Paragraph ($1 :: $2)}
    | BEGIN_PARAGRAPH inline* END_PARAGRAPH                     {Ast.Paragraph $2}
    | BEGIN_QUOTE frag END_QUOTE                                {Ast.Quote $2}
    | set_literal BEGIN_VERBATIM set_general TEXT END_VERBATIM  {Ast.Verbatim $4}
    | set_literal BEGIN_SOURCE set_general TEXT END_SOURCE      {Ast.Source $4}

inline_without_space:
    | TEXT                                                      {Ast.Text $1}
    | BOLD inline_bundle                                        {Ast.Bold $2}
    | LINK raw_bundle inline_bundle?                            {Ast.Link ($2, $3)}

inline:
    | SPACE TEXT                                                {Ast.Text (" " ^ $2)}
    | SPACE %prec inline_SPACE                                  {Ast.Text " "}
    | inline_without_space                                      {$1}

inline_bundle:
    | OPEN inline* CLOSE                                        {$2}

raw_bundle:
    | set_raw OPEN set_general TEXT CLOSE                       {$4}

set_general:
    | /* empty */                                               {C.(set General)}

set_raw:
    | /* empty */                                               {C.(set Raw)}

set_literal:
    | /* empty */                                               {C.(set Literal)}

