(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Types

let empty =
  Ast_helper.Typ.mk Ptyp_any

let is_t_attr = function
  | {Parsetree.attr_name = {Location.txt = "t" | "lexifi.t"; _}; _} -> true
  | _ -> false

let is_empty (sty : Parsetree.core_type) =
  sty.ptyp_attributes = [] &&
  sty.ptyp_desc = empty.ptyp_desc

let core_type_of_payload = function
  | Parsetree.PTyp sty -> Some sty
  | _ -> None

let core_type_of_attribute (x : Parsetree.attribute) =
  match x.attr_name.txt with
  | "#props#" -> core_type_of_payload x.attr_payload
  | _ -> None

let core_type_of_attributes attrs =
  match List.find_map core_type_of_attribute attrs with
  | None -> empty
  | Some sty -> sty

let core_types_of_attributes tyl attrs =
  match List.find_map core_type_of_attribute attrs with
  | Some {ptyp_desc = Ptyp_tuple styl; _} -> styl
  | None -> List.map (fun _ -> empty) tyl
  | Some _ -> Misc.fatal_error __FUNCTION__

(* Propagation of constant expressions *)

let rec approx_expr env e =
  match e.Parsetree.pexp_desc with
  | Pexp_constant (Pconst_string (s, _, _)) -> Some s
  | Pexp_ident lid ->
      begin
        try
          let (_, desc) = Env.lookup_value ~loc:lid.loc lid.txt env in
          Types.val_approx desc
        with Not_found -> None (* More explicit error message? *)
      end
  | Pexp_apply ({pexp_desc = Pexp_ident{txt=Longident.Lident "^"}},
                [(Nolabel, e1); (Nolabel, e2)]) ->
      begin match approx_expr env e1 with
      | Some s1 ->
          begin match approx_expr env e2 with
          | Some s2 -> Some (s1 ^ s2)
          | _ -> None
          end
      | _ -> None
      end
  | Pexp_sequence (_, e2) -> approx_expr env e2
  | _ -> None

let really_approx_expr env e =
  match approx_expr env e with
  | Some s -> s
  | None ->
      Location.alert ~kind:"not_a_string_constant" e.Parsetree.pexp_loc "Not a string constant. Ignoring.";
      ""

let props_attributes env attrs =
  Ast_helper.map_props
    (fun e ->
       let s = really_approx_expr env e in
       {e with pexp_desc = Pexp_constant(Pconst_string (s, Location.none, None))}
    )
    attrs

let no_lid =
  Location.mknoloc (Longident.Lident "")

let rec prune_core_type env sty =
  let open Parsetree in
  let prune = prune_core_type env in
  let attrs = List.filter is_t_attr (props_attributes env sty.ptyp_attributes) in
  let mk desc = Ast_helper.Typ.mk ~attrs desc in
  let empty = mk empty.ptyp_desc in
  match sty.ptyp_desc with
  | Ptyp_any | Ptyp_var _ | Ptyp_extension _ ->
      empty
  | Ptyp_arrow (lab, sty1, sty2) ->
      let sty1 = prune sty1 in
      let sty2 = prune sty2 in
      if is_empty sty1 && is_empty sty2 then empty
      else mk (Ptyp_arrow (lab, sty1, sty2))
  | Ptyp_tuple styl ->
      let styl = List.map prune styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_tuple styl)
  | Ptyp_constr (_, styl) ->
      let styl = List.map prune styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_constr (no_lid, styl))
  | Ptyp_class (_, styl) ->
      let styl = List.map prune styl in
      if List.for_all is_empty styl then empty else mk (Ptyp_class (no_lid, styl))
  | Ptyp_alias (sty, _) ->
      let sty = prune sty in
      if is_empty sty then empty else mk (Ptyp_alias (sty, ""))
  | Ptyp_poly (_, sty) ->
      let sty = prune sty in
      if is_empty sty then empty else mk (Ptyp_poly ([], sty))
  | Ptyp_object (l, _) ->
      let l =
        let mk desc = {pof_desc = desc; pof_loc = Location.none; pof_attributes = []} in
        List.map (function
            | {pof_desc = Otag (lab, sty); _} ->
                mk (Otag ({txt = lab.txt; loc = Location.none}, prune sty))
            | {pof_desc = Oinherit sty; _} ->
                mk (Oinherit (prune sty))
          ) l
      in
      if List.for_all (fun {pof_desc = Otag (_, sty) | Oinherit sty} -> is_empty sty) l
      then empty
      else mk (Ptyp_object (l, Closed))
  | Ptyp_variant (l, _, _) ->
      let l =
        let mk desc = {prf_desc = desc; prf_loc = Location.none; prf_attributes = []} in
        List.map (function
            | {prf_desc = Rtag (lab, _, styl); _} ->
                mk (Rtag ({txt = lab.txt; loc = Location.none}, false, List.map prune styl))
            | {prf_desc = Rinherit sty; _} ->
                mk (Rinherit (prune sty))
          ) l
      in
      if
        List.for_all (function
            | {prf_desc = Rtag (_, _, styl); _} -> List.for_all is_empty styl
            | {prf_desc = Rinherit sty; _} -> is_empty sty
          ) l
      then empty
      else mk (Ptyp_variant (l, Closed, None))
  | Ptyp_package (_, l) ->
      let l = List.map (fun (_, sty) -> no_lid, prune sty) l in
      if List.for_all (fun (_, sty) -> is_empty sty) l then empty
      else mk (Ptyp_package (no_lid, l))

let store_props env sty attrs =
  let sty = prune_core_type env sty in
  if is_empty sty then attrs
  else Ast_helper.Attr.mk (Location.mknoloc "#props#") (PTyp sty) :: attrs

let store_props_tuple env styl attrs =
  store_props env (Ast_helper.Typ.tuple styl) attrs

let ident_props =
  Ident.create_persistent "Props:"

let encode_props props =
  Marshal.to_string props []

let decode_props s =
  Marshal.from_string s 0

let path_of_props props =
  Path.Pdot (Pident ident_props, encode_props props)

let props_of_path = function
  | Path.Pdot (Pident id, s) when Ident.same id ident_props -> Some (decode_props s)
  | _ -> None

let rec restore_props (sty : Parsetree.core_type) (ty : type_expr) : type_expr =
  let mk desc = newty2 ~level:(get_level ty) desc in
  let ty =
    match sty.ptyp_desc, get_desc ty with
    | Ptyp_arrow (_, sty1, sty2), Tarrow (lab, ty1, ty2, comm) ->
        let ty1' = restore_props sty1 ty1 and ty2' = restore_props sty2 ty2 in
        if ty1' == ty1 && ty2' == ty2 then ty else mk (Tarrow (lab, ty1', ty2', comm))
    | Ptyp_tuple styl, Ttuple tyl ->
        let tyl' = List.map2 restore_props styl tyl in
        if List.for_all2 (==) tyl' tyl then ty else mk (Ttuple tyl')
    | Ptyp_constr (_, styl), Tconstr (path, tyl, memo) ->
        let tyl' = List.map2 restore_props styl tyl in
        if List.for_all2 (==) tyl' tyl then ty else mk (Tconstr (path, tyl', memo))
    | Ptyp_poly (_, sty), _ ->
        restore_props sty ty
    | Ptyp_object (fields, _), Tobject (ty1, flag) ->
        let rec loop ty =
          match get_desc ty with
          | Tfield(s, k, ty1, ty2) ->
              let ty1' =
                match
                  List.find_map (fun (pof : Parsetree.object_field) ->
                      match pof with
                      | {pof_desc = Otag ({txt = s'}, sty1)} ->
                          if s' = s then Some (restore_props sty1 ty1) else None
                      | {pof_desc = Oinherit _} ->
                          None
                    ) fields
                with
                | None -> ty1
                | Some ty1' -> ty1'
              in
              let ty2' = loop ty2 in
              if ty1' == ty1 && ty2' == ty2 then ty else mk (Tfield(s, k, ty1', ty2'))
          | _ ->
              ty
        in
        let ty1' = loop ty1 in
        if ty1' == ty1 then ty else mk (Tobject (ty1', flag))
    | Ptyp_variant (fields, _, _), Tvariant trow ->
        let trow' =
          let tfields = row_fields trow in
          let tfields' =
            List.map (fun ((s, tfield) as f) ->
                match
                  List.find_map (fun (prf : Parsetree.row_field) ->
                      match prf with
                      | {prf_desc = Rtag ({txt = s'}, _, styl)} -> if s' = s then Some styl else None
                      | {prf_desc = Rinherit _} -> None
                    ) fields
                with
                | None -> f
                | Some styl ->
                    begin match styl, row_field_repr tfield with
                    | [sty], Rpresent (Some ty) ->
                        let ty' = restore_props sty ty in
                        if ty' == ty then f
                        else s, rf_present (Some (restore_props sty ty))
                    | styl, Reither (no_arg, tyl, matched) ->
                        let tyl' = List.map2 restore_props styl tyl in
                        if List.for_all2 (==) tyl' tyl then f
                        else s, rf_either ~no_arg tyl' ~matched
                    | _ ->
                        f
                    end
              ) tfields
          in
          if List.for_all2 (==) tfields' tfields then trow
          else
            create_row
              ~fields:tfields'
              ~more:(row_more trow)
              ~closed:(row_closed trow)
              ~fixed:(row_fixed trow)
              ~name:(row_name trow)
        in
        if trow' == trow then ty else mk (Tvariant trow')
    | Ptyp_package (_, fields), Tpackage (path, tfields) ->
        let tfields' = List.map2 (fun (_, sty) (lid, ty) -> lid, restore_props sty ty) fields tfields in
        if List.for_all2 (fun (_, ty') (_, ty) -> ty' == ty) tfields' tfields then ty
        else mk (Tpackage (path, tfields'))
    | _ ->
        ty
  in
  List.fold_left (fun ty props ->
      let rec insert ty =
        match get_desc ty with
        | Tpoly (ty1, tyl) -> mk (Tpoly (insert ty1, tyl))
        | _ -> mk (Tconstr (path_of_props props, [ty], ref Mnil))
      in
      if props = [] then ty else insert ty
    ) ty (Ast_helper.get_str_props sty.ptyp_attributes)

(* let restore_props sty ty = *)
(*   let ty' = restore_props sty ty in *)
(*   if ty' != ty then begin *)
(*     Format.printf "=> %a@." (Printast.payload 0) (Parsetree.PTyp sty); *)
(*     Format.printf "=> %a@." !Btype.print_raw ty; *)
(*     Format.printf "<= %a@." !Btype.print_raw ty' *)
(*   end; *)
(*   ty' *)

let restore_props_tuple attrs tyl =
  let styl = core_types_of_attributes tyl attrs in
  List.map2 restore_props styl tyl

let restore_props attrs ty =
  let sty = core_type_of_attributes attrs in
  restore_props sty ty
