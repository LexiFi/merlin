let init ?env_ocamlpath:_ ?config:_ ?toolchain:_ () = ()
let package_directory _ = raise Not_found
let resolve_path ~base:_ ~explicit:_ _ = raise Not_found
let package_property _ _ _ = raise Not_found
let ocaml_stdlib () = ""
let package_deep_ancestors _ _ = []
let list_packages () = []
exception No_such_package of string
