type t = block list

and block =
	| Paragraph of inline list
    | Quote of block list
	| Verbatim of string
    | Source of string

and inline =
	| Text of string
	| Bold of inline list
	| Link of string * inline list option


let sprint ast =
	let rec sprint_frag frag =
		String.concat "\n" (List.map sprint_block frag)
	and sprint_block = function
		| Paragraph seq ->
			Printf.sprintf "\\begin{p}\n%s\n\\end{p}" (sprint_seq seq)
		| Quote frag ->
			Printf.sprintf "\\begin{q}\n%s\n\\end{q}" (sprint_frag frag)
		| Verbatim raw ->
			Printf.sprintf "\\begin{v}\n%s\n\\end{v}" raw
		| Source raw ->
			Printf.sprintf "\\begin{s}\n%s\n\\end{s}" raw
	and sprint_seq seq =
		String.concat "" (List.map sprint_inline seq)
	and sprint_inline = function
		| Text text ->
			text
		| Bold seq ->
			Printf.sprintf "\\bold{%s}" (sprint_seq seq)
		| Link (url, maybe_seq) ->
			begin match maybe_seq with
                | Some seq -> Printf.sprintf "\\link{%s}{%s}" url (sprint_seq seq)
				| None	   -> Printf.sprintf "\\link{%s}" url
			end
	in sprint_frag ast

