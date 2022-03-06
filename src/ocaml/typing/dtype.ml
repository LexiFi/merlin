(***************************************************************************)
(*  Copyright (C) 2000-2022 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Path
open Types
open Mlfi_types

(* Avoid duplicated warnings *)
let warns = Hashtbl.create 8
let warning loc w =
  let k = (loc, w) in
  if not (Hashtbl.mem warns k) then begin
    Location.prerr_warning loc w;
    Hashtbl.add warns k ();
  end

type error =
  | Illegal_dyn_type of string * type_expr

exception Error of Location.t * error
let error loc err = raise (Error (loc, err))
let errstr loc ty s = error loc (Illegal_dyn_type(s, ty))

let path_date =
  Pdot (Pident (Ident.create_persistent "Mlfi_date"), "t")

let ident_mlfi_acontract =
  Ident.create_persistent "Mlfi_acontract"

let ident_mlfi_contract =
  Ident.create_persistent "Mlfi_contract"

let path_mlfi_acontract_contract =
  Pdot (Pident ident_mlfi_acontract, "contract")

let path_mlfi_contract_contract =
  Pdot (Pident ident_mlfi_contract, "contract")

let path_mlfi_contract_observable =
  Pdot (Pident ident_mlfi_contract, "observable")

let path_mlfi_acontract_obs =
  Pdot (Pident ident_mlfi_acontract, "obs")

let path_is_contract path =
  Path.same path path_mlfi_acontract_contract ||
  Path.same path path_mlfi_contract_contract

let path_is_observable path =
  Path.same path path_mlfi_contract_observable ||
  Path.same path path_mlfi_acontract_obs

let path_name loc env path =
  let s = Printtyp.full_name_typ env path in
  if String.contains s '*' then warning loc (Warnings.Not_a_global_type s);
  s

let stype_of_type { Typedtree.ctyp_type = ty; ctyp_env = env; ctyp_loc = loc } =

  let memotbl = Hashtbl.create 16 in
  let nb_used_types = ref 0 in
  let used_types = ref [] in

  let existing_type loc addr =
    let i =
      try
       let (i, _, _) = List.find (fun (_, _, addr2) -> addr = addr2) !used_types in
       i
      with Not_found ->
        let i = !nb_used_types in
        incr nb_used_types;
        used_types := (i, loc, addr) :: !used_types;
        i
    in
    DT_var i
  in

  let build_dt_prop props t =
    if List.mem ("no_dynamic_type", "") props then
      error loc (Illegal_dyn_type("no_dynamic_type property", ty));
    match props with
    | [] -> t
    | _ -> DT_prop(props, t)
  in
  let rec dyn rec_types t =
    let t = Ctype.repr t in
    let (depth, rtypes) = rec_types in
    if depth > 100 then
      errstr loc ty "maximum depth exceeded, probably because of non-guarded recursion";
    let rec_types = (depth + 1, rtypes) in
    match t.desc with
    | Tprop (props, t) ->
        build_dt_prop props (dyn rec_types t)
    | Tvar _ ->
        errstr loc ty "type variable"
    | Tpoly (t, []) -> dyn rec_types t
    | Tpoly _ -> errstr loc ty "poly"
    | Tunivar _ -> errstr loc ty "univar"
    | Tarrow (label, t1, t2, _) ->
        (* TODO: should we add a '?' prefix for Optional ? *)
        DT_arrow (Btype.label_name label, dyn rec_types t1, dyn rec_types t2)
    | Ttuple tys -> DT_tuple (List.map (dyn rec_types) tys)
    | Tvariant row ->
        let {row_fields; row_closed; _} = Btype.row_repr row in
        if not row_closed then errstr loc ty "open poly variant";
        let fields =
          List.fold_right (fun field acc ->
              match field with
              | (label, Rpresent topt) ->
                  (label, topt, true) :: acc
              | (_, Rabsent) ->
                  acc
              | (label, Reither (_, [], _, _)) ->
                  (label, None, false) :: acc
              | (label, Reither (_, [ty], _, _)) ->
                  (label, Some ty, false) :: acc
              | (_, Reither (_, (_ :: _ :: _), _, _)) ->
                  errstr loc ty "conjunctive poly variant"
            ) row_fields []
        in
        let fields = List.sort (fun (n, _, _) (n', _, _) -> Stdlib.compare n n') fields in
        DT_polyvariant (List.map (function
            | (s, None, p) -> s, None, p
            | (s, Some t, p) -> s, Some (dyn rec_types t), p
          ) fields)
    | Tobject (ty, _) ->
        let (fields, rest) = Ctype.flatten_fields ty in
        begin match rest.desc with
        | Tnil -> ()
        | Tvar _ -> errstr loc ty "open object type"
        | _ -> assert false
        end;
        let fields =
          List.fold_right
            (fun (n, k, t) l ->
              let is_mono =
                match (Ctype.repr t).desc with
                | Tpoly (_, []) -> true
                | Tpoly (_, _) -> false
                | _ -> assert false
              in
               match Btype.field_kind_repr k with
               | Fpresent when is_mono -> (n, t) :: l
               | _ -> l)
            fields [] in
        let fields =
          List.sort (fun (n, _) (n', _) -> Stdlib.compare n n') fields in
        DT_object (List.map (fun (s, t) -> (s, dyn rec_types t)) fields)
    | Tsubst _ -> assert false
    | Tpackage(path, l) ->
        let s = Path.name path in
        let s =
          match l with
          | [] -> s
          | _ -> Printf.sprintf "%s with types %s" s (String.concat " " (List.map (fun (lid, _) -> String.concat "." (Longident.flatten lid)) l))
        in
        DT_abstract(s, List.map (fun (_, ty) -> dyn rec_types ty) l)
    | Tfield(_, _, _, _) | Tnil | Tlink _ -> assert false
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list -> DT_list(dyn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_option -> DT_option(dyn rec_types ty_arg)
    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array -> DT_array(dyn rec_types ty_arg)
    | Tconstr(path, [], _) when Path.same path Predef.path_int -> DT_int
    | Tconstr(path, [], _) when Path.same path Predef.path_string -> DT_string
    | Tconstr(path, [], _) when Path.same path Predef.path_float -> DT_float
    | Tconstr(path, [], _) when Path.same path Predef.path_char -> DT_abstract ("char", [])
    | Tconstr(path, [], _) when Path.same path Predef.path_floatarray -> DT_abstract ("floatarray", [])
    | Tconstr(path, [], _) when Path.same path path_date -> DT_date
    | Tconstr(path, [], _) when path_is_contract path -> DT_abstract ("Mlfi_contract.contract", [])
    | Tconstr(path, [ty], _) when path_is_observable path -> DT_abstract ("Mlfi_contract.observable", [dyn rec_types ty])
    | Tconstr(path, tys, _) ->
        let tys = List.map Ctype.repr tys in
        let s = Path.name path in
        let decl =
          try Env.find_type path env
          with Not_found ->
            errstr loc ty ("cannot find definition for " ^ s)
        in
        let typexp ty =
          Btype.keeping_props
            (fun () -> Ctype.apply env decl.type_params ty tys)
        in

        let abstract_dynamic =
          List.exists (fun {Parsetree.attr_name = {txt; _}; _} -> txt = "mlfi.abstract_dynamic") decl.type_attributes
        in
        let type_name () =
          match decl with
          | {type_manifest = Some body} when abstract_dynamic ->
              (* This is used e.g. for type Ib_stdlib.variant, defined as Mlfi_isdatypes.variant, with constructors
                 exported. *)
              begin match (typexp body).desc with
              | Tconstr(path, _, _) -> Path.name path
              | _ ->
                  errstr loc ty ("dynamic-abstract type does not expand to path name: " ^ s)
              end
          | _ ->
              path_name loc env path
        in

        let try_st_rec set f =
          let args_key = List.map (fun x -> x.id) tys in
          let key = (path, args_key) in
          try Hashtbl.find memotbl key
          with Not_found ->
            let (depth, rtypes) = rec_types in
            if List.length (List.filter ((=) path) rtypes) >= 10 then errstr loc ty "non-regular recursion";
            let node = Internal.create_node (type_name ()) (List.map (dyn rec_types) tys) in
            let t = DT_node node in
            let props = List.flatten (Ast_helper.get_str_props decl.type_attributes) in
            let t = build_dt_prop props t in
            Hashtbl.replace memotbl key t;
            set node (f (dyn (depth, path :: rtypes)));
            t
        in
        match decl with
        | {type_kind = Type_abstract; type_manifest = None} ->
            begin try
              let vpath, vd = Env.find_value_by_name ~use:true (Untypeast.lident_of_path path) env in
              let ttype t =
                let p, _ = Env.find_type_by_name ~use:true (Longident.parse "Mlfi_types.ttype") env in
                Ctype.newty (Tconstr (p, [t], ref Mnil))
              in
              let et =
                List.fold_right
                  (fun arg res ->
                     Ctype.newty (Tarrow (Nolabel, ttype arg, res, Cok))
                  )
                  tys (ttype t)
              in
              let ok =
                Ctype.is_moregeneral env false et vd.val_type
              in

              if ok then begin
                (* Format.eprintf "Witness found for abstract type %s: %a@." type_name Location.print arg_exp_loc; *)
                if tys <> [] then raise Not_found; (* only non-parametrized type for now *)
                let addr =
                  try Env.find_value_address vpath env
                  with Not_found -> Misc.fatal_errorf "Cannot find address for: %s" (Path.name path)
                in
                existing_type vd.val_loc addr
              end else begin
                warning loc
                  (Warnings.Bad_witness_for_abstract_type (Printtyp.full_name_typ env path));
                raise Not_found;
              end
            with Not_found ->
              (* if (try ignore (String.index type_name '#'); false with Not_found -> true) then *)
              (* TODO: warning *)
              DT_abstract (type_name (), List.map (dyn rec_types) tys)
              (* else errstr "GADT existential variable" *) (* see #3480 *)
            end
        | {type_kind = Type_abstract; type_manifest = Some body} when abstract_dynamic ->
            begin match (typexp body).desc with
            | Tconstr(path, tys, _) ->
                let tys = List.map Ctype.repr tys in
                let ttys = List.map (dyn rec_types) tys in
                DT_abstract (Path.name path, ttys)
            | _ -> errstr loc ty ("dynamic-abstract type does not expand to path name: " ^ s)
            end
        | {type_kind = Type_abstract; type_manifest = Some body} ->
            assert (not abstract_dynamic);
            dyn rec_types (typexp body)
        | {type_kind = Type_variant (_, Variant_unboxed) | Type_record (_, Record_unboxed _)} ->
            errstr loc ty "Unboxed types are not supported for dynamic types"
        | {type_kind = Type_variant (constrs, Variant_regular)} ->
            try_st_rec Internal.set_node_variant begin fun dyn ->
              let nconst_tag = ref 0 in
              List.map
                (fun ({Types.cd_id = c; cd_args; cd_res = rt; cd_attributes} as cd) ->
                   let c = Ident.name c in
                   Env.mark_constructor_used Env.Positive cd;
                   if rt <> None then errstr loc ty "GADT not supported for dynamic types";
                   let ts =
                     match cd_args with
                     | Cstr_tuple [] -> C_tuple []
                     | Cstr_tuple ts ->
                         incr nconst_tag;
                         C_tuple (List.map dyn (List.map typexp ts))
                     | Cstr_record fields ->
                         let fields =
                           List.map
                             (fun {Types.ld_id=s; ld_type=t; ld_attributes} ->
                                (Ident.name s, List.flatten (Ast_helper.get_str_props ld_attributes), dyn (typexp t))
                             ) fields
                         in
                         let node = Internal.create_node (Printf.sprintf "%s.%s" (type_name ()) c) [] in
                         Internal.set_node_record node (fields, Record_inline !nconst_tag);
                         incr nconst_tag;
                         C_inline (DT_node node)
                   in
                   (c, List.flatten (Ast_helper.get_str_props cd_attributes), ts)

                ) constrs
            end
        | {type_kind = Type_record (fields, repr)} ->
            try_st_rec Internal.set_node_record begin fun dyn ->
              List.map
                (fun {Types.ld_id=s; ld_type=t; ld_attributes} ->
                   (Ident.name s, List.flatten (Ast_helper.get_str_props ld_attributes), dyn (typexp t))
                ) fields,
              match repr with
              | Types.Record_regular -> Record_regular
              | Types.Record_float -> Record_float
              | _ -> assert false
            end
        | {type_kind = Type_open} ->
            DT_abstract (s, List.map (dyn rec_types) tys)
  in
  let r = dyn (0, []) ty in
  r, List.rev !used_types

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_dyn_type(s, ty) ->
      fprintf ppf
        "The type@ %a@ cannot be a dynamic type (%s)" Printtyp.type_expr ty s

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
