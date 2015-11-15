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

%nonassoc _inline_SPACE_ _inline_list_EMPTY_
%nonassoc SPACE
%nonassoc TEXT BOLD LINK

%start main

%type <Ast.t> main

%%

main:
    | frag EOF                                      {$1}

frag:
    | spaceish? wrapped_block*                      {$2}

spaceish:
    | SPACE                                         {()}
    | EOP                                           {()}

wrapped_block:
    | block spaceish?                               {$1}

block:
    | inline_without_space inline_list              {Ast.Paragraph ($1 :: $2)}
    | BEGIN_PARAGRAPH inline_list END_PARAGRAPH     {Ast.Paragraph $2}
    | BEGIN_QUOTE frag END_QUOTE                    {Ast.Quote $2}
    | BEGIN_VERBATIM TEXT END_VERBATIM              {Ast.Verbatim $2}
    | BEGIN_SOURCE TEXT END_SOURCE                  {Ast.Source $2}

inline_without_space:
    | TEXT                                          {Ast.Text $1}
    | BOLD inline_bundle                            {Ast.Bold $2}
    | LINK raw_bundle inline_bundle?                {Ast.Link ($2, $3)}

inline:
    | SPACE %prec _inline_SPACE_                    {Ast.Text " "}
    | SPACE TEXT                                    {Ast.Text (" " ^ $2)}
    | inline_without_space                          {$1}

inline_bundle:
    | OPEN inline_list CLOSE                        {$2}

inline_list:
    | (* empty *) %prec _inline_list_EMPTY_         {[]}
    | inline inline_list                            {$1 :: $2}
        
raw_bundle:
    | OPEN TEXT CLOSE                               {$2}

