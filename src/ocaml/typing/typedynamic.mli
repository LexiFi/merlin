(***************************************************************************)
(*  Copyright (C) 2000-2022 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

val assign_global_names: Parsetree.structure -> Parsetree.structure

val full_name_mod: Env.t -> Path.t -> string

val illegal_dyn_use: Location.t -> 'a

val stype_of_type: Env.t -> Location.t -> Types.type_expr -> Mlfi_types.stype * Path.t list

val ttype_of: loc:Location.t -> Parsetree.core_type -> Parsetree.expression

val build_stypes: Typedtree.structure -> unit

val get_stype: int -> Mlfi_types.stype * Path.t list

val reset: unit -> unit

module Typath: sig
  type step =
    | Ttypath_constructor of Longident.t Location.loc * int
    | Ttypath_field of Longident.t Location.loc
    | Ttypath_tuple of int * int
    | Ttypath_list of Typedtree.expression
    | Ttypath_array of Typedtree.expression

  val encode: step list -> Typedtree.expression_desc
  val decode: Typedtree.expression -> step list option
end
