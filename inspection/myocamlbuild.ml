open Ocamlbuild_plugin


let menhir_opts = S
	[
	A"--dump";
	A"--explain";
    A"--inspection";
	(*A"--log-automaton"; A"0";*)
	(*A"--log-code"; A"0";*)
	(*A"--log-grammar"; A"0";*)
	A"--strict";
	A"--table";
	(*A"--trace";*)
	]


let _ = dispatch begin function
	| After_rules ->
		flag ["menhir"] menhir_opts;
	| _ -> ()
end

