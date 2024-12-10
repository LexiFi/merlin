(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Parsetree
open Types

val store_props: Env.t -> core_type -> attributes -> attributes
val store_props_tuple: Env.t -> core_type list -> attributes -> attributes
val path_of_props: (string * string) list -> Path.t
val props_of_path: Path.t -> (string * string) list option
val restore_props: attributes -> type_expr -> type_expr
val restore_props_tuple: attributes -> type_expr list -> type_expr list
val props_attributes: Env.t -> attributes -> attributes
val really_approx_expr: Env.t -> expression -> string
val approx_expr: Env.t -> expression -> string option
