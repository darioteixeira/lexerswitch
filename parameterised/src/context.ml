module type S =
sig
    type t = General | Raw | Literal

    val set: t -> unit
    val get: unit -> t
end


module Make (M: sig end): S =
struct
    type t = General | Raw | Literal

    let to_string = function
        | General -> "general"
        | Raw     -> "raw"
        | Literal -> "literal"

    let cur = ref General

    let set context =
        Printf.eprintf "@@@ set context=%s\n%!" (to_string context);
        cur := context

    let get () =
        Printf.eprintf "@@@ get context=%s\n%!" (to_string !cur);
        !cur
end

