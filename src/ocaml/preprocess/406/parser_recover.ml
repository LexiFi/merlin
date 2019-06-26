open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper
  let default_loc = ref Location.none
  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))
  let default_pattern () = Pat.any ~loc:!default_loc ()
  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SHARP -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT_OBS -> 0
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT_OBS -> ("0.")
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DATE_OBS -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DATE -> 0
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_propss -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_props -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_prop -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_typevar_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_path_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_path_expr_core -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_path_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_props -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_plus_props -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_pattern_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonrec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_alias -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_label -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_inherit_field_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_const_obs -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_structure -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_descriptions -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_amper_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;1;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;1;2;1;1;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;5;6;1;7;1;1;2;3;1;2;1;1;2;1;1;1;1;1;1;2;3;2;1;2;3;4;1;2;3;1;3;4;1;2;1;2;3;2;3;5;6;7;8;1;4;5;6;2;3;1;2;1;1;2;3;1;2;3;1;2;1;1;2;3;4;5;4;5;1;2;2;3;4;3;4;8;1;2;1;2;3;3;3;3;3;1;3;2;3;1;1;1;2;3;4;2;5;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;2;3;1;2;4;5;4;5;6;2;1;1;2;3;2;3;1;1;2;3;4;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;2;1;3;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;3;1;1;2;3;1;1;1;2;3;4;1;2;1;1;1;2;3;2;3;2;1;2;1;1;2;3;4;5;2;3;2;3;2;3;3;4;2;2;3;3;4;1;3;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;1;1;1;1;2;1;2;1;1;1;1;1;1;1;1;2;3;1;2;3;4;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;2;1;1;1;1;2;3;1;1;2;1;1;2;3;4;1;1;4;5;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;8;9;1;1;1;1;1;1;1;1;1;2;3;2;1;3;1;4;1;1;2;1;2;1;2;2;3;2;3;1;2;1;1;1;1;2;2;3;4;5;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;5;6;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;3;4;2;3;1;2;3;1;2;3;3;4;1;2;3;4;5;6;3;4;5;6;2;1;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;1;2;3;3;4;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;5;4;5;2;3;4;2;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;4;5;6;3;4;5;6;7;8;2;3;4;5;6;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;4;5;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;4;5;3;4;5;6;1;2;3;4;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;1;1;2;2;2;3;2;3;1;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;2;3;1;1;1;7;2;3;4;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;2;3;1;2;3;4;5;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;1;2;3;4;5;6;7;1;2;3;4;8;1;2;3;4;5;5;6;2;1;1;2;3;4;1;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;7;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;1;2;3;4;3;4;5;6;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;2;3;4;9;2;2;1;1;1;2;3;4;2;3;1;1;3;4;5;6;7;2;3;3;4;5;3;4;3;4;5;2;3;2;3;4;4;2;3;5;2;3;4;3;4;4;5;6;2;1;2;1;1;2;1;2;3;4;5;6;2;3;1;2;1;2;1;3;4;5;2;3;4;5;4;1;3;3;3;4;2;2;3;4;5;6;2;3;4;5;6;7;8;5;5;6;2;3;6;7;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;7;8;6;3;4;2;2;3;4;5;5;6;7;2;3;2;3;3;4;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;3;4;5;1;2;1;0;1;2;1;0;1;2;1;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SHARP -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 489] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 141] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 619] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 210] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 453] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 163] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 322] in
  let r25 = [R 137] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 157] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 394 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 596] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 582] in
  let r35 = [R 49] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 584] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 50] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 323] in
  let r42 = [R 209] in
  let r43 = [R 18] in
  let r44 = [R 105] in
  let r45 = [R 594] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = [R 758] in
  let r48 = [R 211] in
  let r49 = S (T T_RBRACKET) :: r48 in
  let r50 = Sub (r15) :: r49 in
  let r51 = S (T T_LIDENT) :: r47 in
  let r52 = [R 525] in
  let r53 = S (T T_UNDERSCORE) :: r52 in
  let r54 = [R 522] in
  let r55 = Sub (r53) :: r54 in
  let r56 = [R 543] in
  let r57 = Sub (r55) :: r56 in
  let r58 = [R 121] in
  let r59 = Sub (r57) :: r58 in
  let r60 = [R 130] in
  let r61 = Sub (r59) :: r60 in
  let r62 = [R 119] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 766] in
  let r65 = R 463 :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = S (T T_COLON) :: r66 in
  let r68 = Sub (r51) :: r67 in
  let r69 = [R 388] in
  let r70 = S (T T_AMPERAMPER) :: r69 in
  let r71 = [R 759] in
  let r72 = S (T T_RPAREN) :: r71 in
  let r73 = Sub (r70) :: r72 in
  let r74 = [R 369] in
  let r75 = S (T T_RPAREN) :: r74 in
  let r76 = [R 371] in
  let r77 = [R 373] in
  let r78 = [R 319] in
  let r79 = [R 531] in
  let r80 = [R 238] in
  let r81 = S (T T_LIDENT) :: r80 in
  let r82 = [R 524] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 122] in
  let r85 = Sub (r59) :: r84 in
  let r86 = S (T T_MINUSGREATER) :: r85 in
  let r87 = Sub (r59) :: r86 in
  let r88 = S (T T_COLON) :: r87 in
  let r89 = [R 123] in
  let r90 = Sub (r59) :: r89 in
  let r91 = S (T T_MINUSGREATER) :: r90 in
  let r92 = [R 418] in
  let r93 = S (N N_module_type) :: r92 in
  let r94 = [R 541] in
  let r95 = S (T T_RPAREN) :: r94 in
  let r96 = Sub (r93) :: r95 in
  let r97 = R 208 :: r96 in
  let r98 = [R 342] in
  let r99 = S (T T_END) :: r98 in
  let r100 = R 499 :: r99 in
  let r101 = [R 719] in
  let r102 = R 463 :: r101 in
  let r103 = R 112 :: r102 in
  let r104 = R 722 :: r103 in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = R 411 :: r105 in
  let r107 = R 360 :: r106 in
  let r108 = R 208 :: r107 in
  let r109 = [R 415] in
  let r110 = S (T T_UNDERSCORE) :: r109 in
  let r111 = [R 408] in
  let r112 = Sub (r110) :: r111 in
  let r113 = R 753 :: r112 in
  let r114 = [R 409] in
  let r115 = Sub (r113) :: r114 in
  let r116 = [R 413] in
  let r117 = S (T T_RPAREN) :: r116 in
  let r118 = [R 414] in
  let r119 = [R 410] in
  let r120 = [R 728] in
  let r121 = [R 729] in
  let r122 = [R 98] in
  let r123 = [R 736] in
  let r124 = [R 124] in
  let r125 = Sub (r59) :: r124 in
  let r126 = S (T T_MINUSGREATER) :: r125 in
  let r127 = [R 530] in
  let r128 = [R 486] in
  let r129 = Sub (r55) :: r128 in
  let r130 = [R 487] in
  let r131 = Sub (r129) :: r130 in
  let r132 = [R 539] in
  let r133 = S (T T_RBRACKET) :: r132 in
  let r134 = Sub (r131) :: r133 in
  let r135 = [R 538] in
  let r136 = [R 537] in
  let r137 = S (T T_RBRACKET) :: r136 in
  let r138 = [R 535] in
  let r139 = S (T T_RBRACKET) :: r138 in
  let r140 = Sub (r131) :: r139 in
  let r141 = [R 357] in
  let r142 = Sub (r81) :: r141 in
  let r143 = [R 532] in
  let r144 = [R 737] in
  let r145 = S (T T_LIDENT) :: r144 in
  let r146 = S (T T_DOT) :: r145 in
  let r147 = S (T T_UIDENT) :: r78 in
  let r148 = [R 321] in
  let r149 = S (T T_RPAREN) :: r148 in
  let r150 = [R 320] in
  let r151 = [R 488] in
  let r152 = [R 708] in
  let r153 = [R 5] in
  let r154 = Sub (r61) :: r153 in
  let r155 = [R 707] in
  let r156 = R 17 :: r155 in
  let r157 = Sub (r154) :: r156 in
  let r158 = [R 128] in
  let r159 = Sub (r55) :: r158 in
  let r160 = [R 544] in
  let r161 = [R 129] in
  let r162 = [R 131] in
  let r163 = S (T T_RBRACKET) :: r162 in
  let r164 = R 783 :: r163 in
  let r165 = [R 779] in
  let r166 = [R 76] in
  let r167 = R 42 :: r166 in
  let r168 = R 53 :: r167 in
  let r169 = [R 201] in
  let r170 = S (T T_END) :: r169 in
  let r171 = Sub (r168) :: r170 in
  let r172 = [R 51] in
  let r173 = S (T T_RPAREN) :: r172 in
  let r174 = [R 601] in
  let r175 = S (T T_LIDENT) :: r123 in
  let r176 = [R 606] in
  let r177 = [R 520] in
  let r178 = [R 518] in
  let r179 = [R 612] in
  let r180 = S (T T_RPAREN) :: r179 in
  let r181 = [R 614] in
  let r182 = S (T T_RPAREN) :: r181 in
  let r183 = S (T T_UIDENT) :: r182 in
  let r184 = [R 615] in
  let r185 = S (T T_RPAREN) :: r184 in
  let r186 = [R 346] in
  let r187 = S (N N_module_expr) :: r186 in
  let r188 = R 17 :: r187 in
  let r189 = S (T T_OF) :: r188 in
  let r190 = [R 334] in
  let r191 = S (T T_END) :: r190 in
  let r192 = S (N N_structure) :: r191 in
  let r193 = [R 100] in
  let r194 = S (T T_FALSE) :: r193 in
  let r195 = [R 212] in
  let r196 = R 17 :: r195 in
  let r197 = R 233 :: r196 in
  let r198 = Sub (r194) :: r197 in
  let r199 = [R 670] in
  let r200 = Sub (r198) :: r199 in
  let r201 = [R 677] in
  let r202 = R 463 :: r201 in
  let r203 = Sub (r200) :: r202 in
  let r204 = R 468 :: r203 in
  let r205 = [R 99] in
  let r206 = [R 97] in
  let r207 = [R 20] in
  let r208 = R 17 :: r207 in
  let r209 = R 233 :: r208 in
  let r210 = Sub (r194) :: r209 in
  let r211 = [R 107] in
  let r212 = S (T T_FALSE) :: r211 in
  let r213 = [R 21] in
  let r214 = R 17 :: r213 in
  let r215 = Sub (r212) :: r214 in
  let r216 = S (T T_EQUAL) :: r215 in
  let r217 = [R 113] in
  let r218 = Sub (r159) :: r217 in
  let r219 = [R 234] in
  let r220 = [R 259] in
  let r221 = S (T T_LIDENT) :: r220 in
  let r222 = [R 246] in
  let r223 = R 17 :: r222 in
  let r224 = R 400 :: r223 in
  let r225 = Sub (r221) :: r224 in
  let r226 = R 355 :: r225 in
  let r227 = [R 249] in
  let r228 = Sub (r226) :: r227 in
  let r229 = [R 114] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 248] in
  let r232 = R 17 :: r231 in
  let r233 = S (T T_SEMI) :: r232 in
  let r234 = R 17 :: r233 in
  let r235 = R 400 :: r234 in
  let r236 = [R 260] in
  let r237 = S (T T_LIDENT) :: r236 in
  let r238 = S (T T_DOT) :: r237 in
  let r239 = [R 401] in
  let r240 = S (T T_RBRACKET) :: r239 in
  let r241 = R 783 :: r240 in
  let r242 = [R 460] in
  let r243 = Sub (r61) :: r242 in
  let r244 = [R 247] in
  let r245 = R 17 :: r244 in
  let r246 = S (T T_SEMI) :: r245 in
  let r247 = R 17 :: r246 in
  let r248 = [R 756] in
  let r249 = [R 461] in
  let r250 = Sub (r61) :: r249 in
  let r251 = [R 757] in
  let r252 = [R 106] in
  let r253 = [R 104] in
  let r254 = [R 103] in
  let r255 = S (T T_RPAREN) :: r254 in
  let r256 = S (T T_COLONCOLON) :: r255 in
  let r257 = [R 236] in
  let r258 = [R 235] in
  let r259 = Sub (r55) :: r258 in
  let r260 = [R 462] in
  let r261 = S (T T_RBRACKET) :: r260 in
  let r262 = Sub (r15) :: r261 in
  let r263 = [R 709] in
  let r264 = [R 703] in
  let r265 = S (T T_UIDENT) :: r24 in
  let r266 = [R 362] in
  let r267 = R 463 :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = R 208 :: r268 in
  let r270 = [R 464] in
  let r271 = [R 326] in
  let r272 = S (N N_module_expr) :: r271 in
  let r273 = S (T T_EQUAL) :: r272 in
  let r274 = [R 477] in
  let r275 = R 463 :: r274 in
  let r276 = Sub (r273) :: r275 in
  let r277 = S (T T_UIDENT) :: r276 in
  let r278 = S (T T_REC) :: r277 in
  let r279 = [R 350] in
  let r280 = R 463 :: r279 in
  let r281 = R 351 :: r280 in
  let r282 = Sub (r81) :: r281 in
  let r283 = R 208 :: r282 in
  let r284 = [R 352] in
  let r285 = [R 347] in
  let r286 = S (T T_RPAREN) :: r285 in
  let r287 = [R 343] in
  let r288 = S (N N_module_type) :: r287 in
  let r289 = S (T T_MINUSGREATER) :: r288 in
  let r290 = S (N N_functor_args) :: r289 in
  let r291 = [R 227] in
  let r292 = [R 228] in
  let r293 = S (T T_RPAREN) :: r292 in
  let r294 = S (N N_module_type) :: r293 in
  let r295 = [R 775] in
  let r296 = Sub (r147) :: r295 in
  let r297 = S (T T_EQUAL) :: r296 in
  let r298 = Sub (r265) :: r297 in
  let r299 = S (T T_MODULE) :: r298 in
  let r300 = [R 777] in
  let r301 = Sub (r299) :: r300 in
  let r302 = [R 345] in
  let r303 = [R 774] in
  let r304 = Sub (r61) :: r303 in
  let r305 = S (T T_COLONEQUAL) :: r304 in
  let r306 = Sub (r221) :: r305 in
  let r307 = [R 773] in
  let r308 = R 112 :: r307 in
  let r309 = [R 109] in
  let r310 = Sub (r63) :: r309 in
  let r311 = S (T T_EQUAL) :: r310 in
  let r312 = Sub (r63) :: r311 in
  let r313 = [R 111] in
  let r314 = [R 776] in
  let r315 = [R 778] in
  let r316 = [R 344] in
  let r317 = [R 354] in
  let r318 = Sub (r81) :: r317 in
  let r319 = [R 325] in
  let r320 = R 463 :: r319 in
  let r321 = Sub (r273) :: r320 in
  let r322 = [R 420] in
  let r323 = S (T T_RPAREN) :: r322 in
  let r324 = [R 421] in
  let r325 = S (T T_RPAREN) :: r324 in
  let r326 = S (N N_expr) :: r325 in
  let r327 = [R 136] in
  let r328 = S (N N_match_cases) :: r327 in
  let r329 = R 394 :: r328 in
  let r330 = S (T T_WITH) :: r329 in
  let r331 = Sub (r1) :: r330 in
  let r332 = [R 156] in
  let r333 = S (N N_match_cases) :: r332 in
  let r334 = R 394 :: r333 in
  let r335 = S (T T_WITH) :: r334 in
  let r336 = Sub (r1) :: r335 in
  let r337 = [R 438] in
  let r338 = S (N N_pattern) :: r337 in
  let r339 = Sub (r212) :: r338 in
  let r340 = [R 446] in
  let r341 = Sub (r339) :: r340 in
  let r342 = [R 288] in
  let r343 = Sub (r1) :: r342 in
  let r344 = S (T T_EQUAL) :: r343 in
  let r345 = Sub (r341) :: r344 in
  let r346 = [R 297] in
  let r347 = R 463 :: r346 in
  let r348 = Sub (r345) :: r347 in
  let r349 = R 475 :: r348 in
  let r350 = [R 548] in
  let r351 = [R 449] in
  let r352 = S (N N_pattern) :: r351 in
  let r353 = [R 546] in
  let r354 = S (T T_RBRACKET) :: r353 in
  let r355 = R 404 :: r354 in
  let r356 = [R 278] in
  let r357 = R 399 :: r356 in
  let r358 = Sub (r221) :: r357 in
  let r359 = [R 279] in
  let r360 = Sub (r358) :: r359 in
  let r361 = [R 545] in
  let r362 = S (T T_RBRACE) :: r361 in
  let r363 = [R 281] in
  let r364 = [R 398] in
  let r365 = [R 277] in
  let r366 = S (T T_UNDERSCORE) :: r174 in
  let r367 = [R 600] in
  let r368 = Sub (r366) :: r367 in
  let r369 = [R 440] in
  let r370 = Sub (r368) :: r369 in
  let r371 = [R 92] in
  let r372 = S (T T_INT) :: r371 in
  let r373 = [R 517] in
  let r374 = Sub (r372) :: r373 in
  let r375 = [R 603] in
  let r376 = [R 609] in
  let r377 = S (T T_RBRACKET) :: r376 in
  let r378 = S (T T_LBRACKET) :: r377 in
  let r379 = [R 610] in
  let r380 = [R 432] in
  let r381 = S (N N_pattern) :: r380 in
  let r382 = [R 435] in
  let r383 = [R 430] in
  let r384 = [R 439] in
  let r385 = [R 611] in
  let r386 = [R 436] in
  let r387 = [R 431] in
  let r388 = [R 428] in
  let r389 = [R 547] in
  let r390 = S (T T_BARRBRACKET) :: r389 in
  let r391 = [R 678] in
  let r392 = Sub (r1) :: r391 in
  let r393 = S (T T_EQUAL) :: r392 in
  let r394 = [R 284] in
  let r395 = [R 261] in
  let r396 = S (T T_LIDENT) :: r395 in
  let r397 = [R 269] in
  let r398 = [R 257] in
  let r399 = Sub (r396) :: r398 in
  let r400 = [R 268] in
  let r401 = S (T T_RPAREN) :: r400 in
  let r402 = [R 258] in
  let r403 = [R 265] in
  let r404 = [R 264] in
  let r405 = S (T T_RPAREN) :: r404 in
  let r406 = R 396 :: r405 in
  let r407 = [R 397] in
  let r408 = [R 148] in
  let r409 = Sub (r1) :: r408 in
  let r410 = S (T T_IN) :: r409 in
  let r411 = Sub (r1) :: r410 in
  let r412 = S (T T_EQUAL) :: r411 in
  let r413 = [R 576] in
  let r414 = [R 206] in
  let r415 = S (N N_expr) :: r414 in
  let r416 = [R 579] in
  let r417 = S (T T_RBRACKET) :: r416 in
  let r418 = R 404 :: r417 in
  let r419 = [R 586] in
  let r420 = [R 244] in
  let r421 = [R 216] in
  let r422 = [R 215] in
  let r423 = [R 273] in
  let r424 = R 407 :: r423 in
  let r425 = Sub (r221) :: r424 in
  let r426 = [R 274] in
  let r427 = Sub (r425) :: r426 in
  let r428 = [R 484] in
  let r429 = Sub (r427) :: r428 in
  let r430 = [R 573] in
  let r431 = S (T T_RBRACE) :: r430 in
  let r432 = [R 550] in
  let r433 = [R 549] in
  let r434 = S (T T_GREATERDOT) :: r433 in
  let r435 = [R 200] in
  let r436 = Sub (r33) :: r435 in
  let r437 = [R 283] in
  let r438 = R 463 :: r437 in
  let r439 = Sub (r345) :: r438 in
  let r440 = R 475 :: r439 in
  let r441 = [R 289] in
  let r442 = Sub (r1) :: r441 in
  let r443 = S (T T_EQUAL) :: r442 in
  let r444 = [R 162] in
  let r445 = S (N N_expr) :: r444 in
  let r446 = S (T T_THEN) :: r445 in
  let r447 = Sub (r1) :: r446 in
  let r448 = [R 153] in
  let r449 = S (N N_match_cases) :: r448 in
  let r450 = R 394 :: r449 in
  let r451 = [R 300] in
  let r452 = Sub (r1) :: r451 in
  let r453 = S (T T_MINUSGREATER) :: r452 in
  let r454 = [R 301] in
  let r455 = Sub (r1) :: r454 in
  let r456 = S (T T_MINUSGREATER) :: r455 in
  let r457 = [R 271] in
  let r458 = Sub (r368) :: r457 in
  let r459 = [R 223] in
  let r460 = Sub (r1) :: r459 in
  let r461 = S (T T_MINUSGREATER) :: r460 in
  let r462 = [R 154] in
  let r463 = Sub (r461) :: r462 in
  let r464 = Sub (r458) :: r463 in
  let r465 = [R 452] in
  let r466 = S (T T_UNDERSCORE) :: r465 in
  let r467 = [R 267] in
  let r468 = [R 266] in
  let r469 = S (T T_RPAREN) :: r468 in
  let r470 = R 396 :: r469 in
  let r471 = [R 294] in
  let r472 = [R 295] in
  let r473 = S (T T_LIDENT) :: r472 in
  let r474 = [R 155] in
  let r475 = Sub (r461) :: r474 in
  let r476 = S (T T_RPAREN) :: r475 in
  let r477 = [R 143] in
  let r478 = S (T T_DONE) :: r477 in
  let r479 = Sub (r1) :: r478 in
  let r480 = S (T T_DO) :: r479 in
  let r481 = Sub (r1) :: r480 in
  let r482 = S (T T_IN) :: r481 in
  let r483 = S (N N_pattern) :: r482 in
  let r484 = [R 134] in
  let r485 = S (T T_DOWNTO) :: r484 in
  let r486 = [R 164] in
  let r487 = S (T T_DONE) :: r486 in
  let r488 = Sub (r1) :: r487 in
  let r489 = S (T T_DO) :: r488 in
  let r490 = Sub (r1) :: r489 in
  let r491 = Sub (r485) :: r490 in
  let r492 = Sub (r1) :: r491 in
  let r493 = S (T T_EQUAL) :: r492 in
  let r494 = S (N N_pattern) :: r493 in
  let r495 = [R 749] in
  let r496 = Sub (r212) :: r495 in
  let r497 = [R 741] in
  let r498 = [R 748] in
  let r499 = S (T T_BARRBRACKET) :: r498 in
  let r500 = [R 559] in
  let r501 = S (T T_END) :: r500 in
  let r502 = [R 583] in
  let r503 = [R 563] in
  let r504 = S (T T_RPAREN) :: r503 in
  let r505 = S (T T_LPAREN) :: r504 in
  let r506 = S (T T_DOT) :: r505 in
  let r507 = [R 593] in
  let r508 = S (T T_RPAREN) :: r507 in
  let r509 = Sub (r93) :: r508 in
  let r510 = S (T T_COLON) :: r509 in
  let r511 = S (N N_module_expr) :: r510 in
  let r512 = [R 335] in
  let r513 = S (N N_module_expr) :: r512 in
  let r514 = S (T T_MINUSGREATER) :: r513 in
  let r515 = S (N N_functor_args) :: r514 in
  let r516 = [R 337] in
  let r517 = [R 419] in
  let r518 = S (T T_RPAREN) :: r517 in
  let r519 = [R 199] in
  let r520 = Sub (r33) :: r519 in
  let r521 = [R 318] in
  let r522 = S (T T_LIDENT) :: r521 in
  let r523 = [R 588] in
  let r524 = [R 262] in
  let r525 = Sub (r33) :: r524 in
  let r526 = [R 597] in
  let r527 = Sub (r525) :: r526 in
  let r528 = [R 589] in
  let r529 = S (T T_RPAREN) :: r528 in
  let r530 = Sub (r527) :: r529 in
  let r531 = [R 256] in
  let r532 = S (T T_LIDENT) :: r531 in
  let r533 = [R 253] in
  let r534 = [R 254] in
  let r535 = [R 255] in
  let r536 = [R 590] in
  let r537 = [R 566] in
  let r538 = S (T T_RBRACKET) :: r537 in
  let r539 = S (N N_expr) :: r538 in
  let r540 = S (T T_LBRACKET) :: r539 in
  let r541 = [R 567] in
  let r542 = S (T T_RPAREN) :: r541 in
  let r543 = S (N N_expr) :: r542 in
  let r544 = [R 186] in
  let r545 = [R 252] in
  let r546 = [R 572] in
  let r547 = S (T T_RBRACE) :: r546 in
  let r548 = S (N N_expr) :: r547 in
  let r549 = S (T T_LBRACE) :: r548 in
  let r550 = [R 564] in
  let r551 = S (T T_RPAREN) :: r550 in
  let r552 = Sub (r1) :: r551 in
  let r553 = [R 135] in
  let r554 = Sub (r1) :: r553 in
  let r555 = [R 146] in
  let r556 = Sub (r1) :: r555 in
  let r557 = [R 198] in
  let r558 = S (N N_expr) :: r557 in
  let r559 = [R 204] in
  let r560 = [R 176] in
  let r561 = [R 170] in
  let r562 = [R 187] in
  let r563 = [R 173] in
  let r564 = [R 177] in
  let r565 = [R 169] in
  let r566 = [R 172] in
  let r567 = [R 171] in
  let r568 = [R 181] in
  let r569 = [R 175] in
  let r570 = [R 174] in
  let r571 = [R 179] in
  let r572 = [R 168] in
  let r573 = [R 167] in
  let r574 = [R 165] in
  let r575 = [R 166] in
  let r576 = [R 180] in
  let r577 = [R 178] in
  let r578 = [R 182] in
  let r579 = [R 183] in
  let r580 = [R 184] in
  let r581 = [R 205] in
  let r582 = [R 185] in
  let r583 = [R 492] in
  let r584 = Sub (r1) :: r583 in
  let r585 = [R 10] in
  let r586 = R 463 :: r585 in
  let r587 = Sub (r345) :: r586 in
  let r588 = [R 447] in
  let r589 = [R 448] in
  let r590 = [R 443] in
  let r591 = [R 444] in
  let r592 = [R 441] in
  let r593 = [R 565] in
  let r594 = S (T T_RBRACKET) :: r593 in
  let r595 = Sub (r1) :: r594 in
  let r596 = [R 569] in
  let r597 = S (T T_RBRACKET) :: r596 in
  let r598 = S (N N_expr) :: r597 in
  let r599 = S (T T_LBRACKET) :: r598 in
  let r600 = [R 570] in
  let r601 = S (T T_RPAREN) :: r600 in
  let r602 = S (N N_expr) :: r601 in
  let r603 = [R 571] in
  let r604 = S (T T_RBRACE) :: r603 in
  let r605 = S (N N_expr) :: r604 in
  let r606 = [R 193] in
  let r607 = [R 192] in
  let r608 = [R 568] in
  let r609 = S (T T_RBRACE) :: r608 in
  let r610 = S (N N_expr) :: r609 in
  let r611 = [R 194] in
  let r612 = [R 189] in
  let r613 = [R 190] in
  let r614 = [R 191] in
  let r615 = [R 196] in
  let r616 = [R 195] in
  let r617 = [R 197] in
  let r618 = [R 188] in
  let r619 = [R 562] in
  let r620 = [R 578] in
  let r621 = [R 577] in
  let r622 = S (T T_BARRBRACKET) :: r621 in
  let r623 = [R 581] in
  let r624 = [R 580] in
  let r625 = S (T T_RBRACKET) :: r624 in
  let r626 = S (T T_LIDENT) :: r420 in
  let r627 = Sub (r626) :: r421 in
  let r628 = [R 217] in
  let r629 = R 404 :: r628 in
  let r630 = Sub (r627) :: r629 in
  let r631 = [R 587] in
  let r632 = S (T T_GREATERRBRACE) :: r631 in
  let r633 = [R 574] in
  let r634 = S (T T_RBRACE) :: r633 in
  let r635 = [R 483] in
  let r636 = Sub (r427) :: r635 in
  let r637 = [R 718] in
  let r638 = [R 716] in
  let r639 = Sub (r63) :: r638 in
  let r640 = [R 717] in
  let r641 = [R 272] in
  let r642 = [R 558] in
  let r643 = [R 747] in
  let r644 = S (T T_RBRACKET) :: r643 in
  let r645 = [R 746] in
  let r646 = S (T T_INT) :: r645 in
  let r647 = [R 742] in
  let r648 = [R 743] in
  let r649 = S (T T_RPAREN) :: r648 in
  let r650 = [R 744] in
  let r651 = S (T T_RPAREN) :: r650 in
  let r652 = [R 142] in
  let r653 = S (T T_DONE) :: r652 in
  let r654 = Sub (r1) :: r653 in
  let r655 = S (T T_DO) :: r654 in
  let r656 = Sub (r1) :: r655 in
  let r657 = Sub (r485) :: r656 in
  let r658 = [R 226] in
  let r659 = Sub (r461) :: r658 in
  let r660 = S (T T_RPAREN) :: r659 in
  let r661 = [R 270] in
  let r662 = [R 224] in
  let r663 = Sub (r1) :: r662 in
  let r664 = S (T T_MINUSGREATER) :: r663 in
  let r665 = [R 225] in
  let r666 = [R 613] in
  let r667 = S (T T_RPAREN) :: r666 in
  let r668 = S (N N_pattern) :: r453 in
  let r669 = [R 304] in
  let r670 = [R 161] in
  let r671 = [R 147] in
  let r672 = Sub (r1) :: r671 in
  let r673 = [R 585] in
  let r674 = [R 575] in
  let r675 = S (T T_BARRBRACKET) :: r674 in
  let r676 = [R 149] in
  let r677 = Sub (r1) :: r676 in
  let r678 = S (T T_IN) :: r677 in
  let r679 = Sub (r1) :: r678 in
  let r680 = S (T T_EQUAL) :: r679 in
  let r681 = [R 152] in
  let r682 = Sub (r1) :: r681 in
  let r683 = S (T T_IN) :: r682 in
  let r684 = Sub (r265) :: r683 in
  let r685 = R 208 :: r684 in
  let r686 = [R 150] in
  let r687 = Sub (r1) :: r686 in
  let r688 = S (T T_IN) :: r687 in
  let r689 = Sub (r273) :: r688 in
  let r690 = S (T T_UIDENT) :: r689 in
  let r691 = [R 327] in
  let r692 = S (N N_module_expr) :: r691 in
  let r693 = S (T T_EQUAL) :: r692 in
  let r694 = [R 328] in
  let r695 = [R 292] in
  let r696 = R 17 :: r695 in
  let r697 = R 233 :: r696 in
  let r698 = Sub (r194) :: r697 in
  let r699 = [R 151] in
  let r700 = Sub (r1) :: r699 in
  let r701 = S (T T_IN) :: r700 in
  let r702 = Sub (r698) :: r701 in
  let r703 = [R 221] in
  let r704 = Sub (r393) :: r703 in
  let r705 = [R 680] in
  let r706 = Sub (r704) :: r705 in
  let r707 = S (T T_RPAREN) :: r706 in
  let r708 = Sub (r473) :: r707 in
  let r709 = [R 222] in
  let r710 = Sub (r1) :: r709 in
  let r711 = [R 679] in
  let r712 = [R 287] in
  let r713 = Sub (r1) :: r712 in
  let r714 = S (T T_EQUAL) :: r713 in
  let r715 = Sub (r63) :: r714 in
  let r716 = S (T T_DOT) :: r715 in
  let r717 = [R 286] in
  let r718 = Sub (r1) :: r717 in
  let r719 = S (T T_EQUAL) :: r718 in
  let r720 = Sub (r63) :: r719 in
  let r721 = [R 285] in
  let r722 = Sub (r1) :: r721 in
  let r723 = [R 424] in
  let r724 = S (T T_RPAREN) :: r723 in
  let r725 = [R 422] in
  let r726 = S (T T_RPAREN) :: r725 in
  let r727 = [R 423] in
  let r728 = S (T T_RPAREN) :: r727 in
  let r729 = [R 699] in
  let r730 = Sub (r1) :: r729 in
  let r731 = S (T T_EQUAL) :: r730 in
  let r732 = S (T T_TYPE) :: r731 in
  let r733 = [R 700] in
  let r734 = Sub (r1) :: r733 in
  let r735 = S (T T_EQUAL) :: r734 in
  let r736 = Sub (r63) :: r735 in
  let r737 = [R 243] in
  let r738 = S (T T_RBRACKET) :: r737 in
  let r739 = Sub (r15) :: r738 in
  let r740 = [R 456] in
  let r741 = [R 457] in
  let r742 = [R 220] in
  let r743 = S (T T_RBRACKET) :: r742 in
  let r744 = Sub (r15) :: r743 in
  let r745 = [R 676] in
  let r746 = R 463 :: r745 in
  let r747 = S (N N_module_expr) :: r746 in
  let r748 = [R 466] in
  let r749 = S (T T_STRING) :: r748 in
  let r750 = [R 465] in
  let r751 = R 463 :: r750 in
  let r752 = Sub (r749) :: r751 in
  let r753 = S (T T_EQUAL) :: r752 in
  let r754 = Sub (r63) :: r753 in
  let r755 = S (T T_COLON) :: r754 in
  let r756 = Sub (r51) :: r755 in
  let r757 = [R 669] in
  let r758 = R 463 :: r757 in
  let r759 = R 17 :: r758 in
  let r760 = Sub (r212) :: r759 in
  let r761 = S (T T_EQUAL) :: r760 in
  let r762 = Sub (r194) :: r761 in
  let r763 = [R 493] in
  let r764 = R 463 :: r763 in
  let r765 = R 17 :: r764 in
  let r766 = R 233 :: r765 in
  let r767 = Sub (r194) :: r766 in
  let r768 = R 208 :: r767 in
  let r769 = [R 454] in
  let r770 = [R 500] in
  let r771 = [R 480] in
  let r772 = R 463 :: r771 in
  let r773 = S (N N_module_type) :: r772 in
  let r774 = S (T T_COLON) :: r773 in
  let r775 = S (T T_UIDENT) :: r774 in
  let r776 = S (T T_REC) :: r775 in
  let r777 = [R 330] in
  let r778 = S (N N_module_type) :: r777 in
  let r779 = S (T T_COLON) :: r778 in
  let r780 = [R 329] in
  let r781 = R 463 :: r780 in
  let r782 = [R 332] in
  let r783 = Sub (r779) :: r782 in
  let r784 = [R 331] in
  let r785 = Sub (r779) :: r784 in
  let r786 = S (T T_RPAREN) :: r785 in
  let r787 = S (N N_module_type) :: r786 in
  let r788 = [R 324] in
  let r789 = R 463 :: r788 in
  let r790 = [R 497] in
  let r791 = R 463 :: r790 in
  let r792 = S (N N_module_type) :: r791 in
  let r793 = [R 87] in
  let r794 = S (T T_LIDENT) :: r793 in
  let r795 = [R 66] in
  let r796 = Sub (r794) :: r795 in
  let r797 = [R 82] in
  let r798 = R 463 :: r797 in
  let r799 = Sub (r796) :: r798 in
  let r800 = S (T T_EQUAL) :: r799 in
  let r801 = S (T T_LIDENT) :: r800 in
  let r802 = R 85 :: r801 in
  let r803 = R 771 :: r802 in
  let r804 = R 208 :: r803 in
  let r805 = [R 752] in
  let r806 = Sub (r81) :: r805 in
  let r807 = S (T T_QUOTE) :: r806 in
  let r808 = [R 738] in
  let r809 = Sub (r807) :: r808 in
  let r810 = R 753 :: r809 in
  let r811 = [R 739] in
  let r812 = Sub (r810) :: r811 in
  let r813 = [R 86] in
  let r814 = S (T T_RBRACKET) :: r813 in
  let r815 = [R 740] in
  let r816 = [R 56] in
  let r817 = R 63 :: r816 in
  let r818 = R 55 :: r817 in
  let r819 = [R 67] in
  let r820 = S (T T_END) :: r819 in
  let r821 = Sub (r818) :: r820 in
  let r822 = [R 54] in
  let r823 = S (T T_RPAREN) :: r822 in
  let r824 = [R 770] in
  let r825 = Sub (r63) :: r824 in
  let r826 = S (T T_COLON) :: r825 in
  let r827 = Sub (r626) :: r826 in
  let r828 = [R 58] in
  let r829 = R 463 :: r828 in
  let r830 = Sub (r827) :: r829 in
  let r831 = [R 768] in
  let r832 = Sub (r63) :: r831 in
  let r833 = S (T T_COLON) :: r832 in
  let r834 = Sub (r626) :: r833 in
  let r835 = [R 769] in
  let r836 = Sub (r63) :: r835 in
  let r837 = S (T T_COLON) :: r836 in
  let r838 = Sub (r626) :: r837 in
  let r839 = [R 458] in
  let r840 = Sub (r63) :: r839 in
  let r841 = [R 59] in
  let r842 = R 463 :: r841 in
  let r843 = Sub (r840) :: r842 in
  let r844 = S (T T_COLON) :: r843 in
  let r845 = Sub (r522) :: r844 in
  let r846 = R 470 :: r845 in
  let r847 = [R 459] in
  let r848 = Sub (r63) :: r847 in
  let r849 = [R 57] in
  let r850 = R 463 :: r849 in
  let r851 = Sub (r796) :: r850 in
  let r852 = [R 70] in
  let r853 = Sub (r796) :: r852 in
  let r854 = S (T T_IN) :: r853 in
  let r855 = Sub (r265) :: r854 in
  let r856 = R 17 :: r855 in
  let r857 = R 416 :: r856 in
  let r858 = [R 126] in
  let r859 = Sub (r63) :: r858 in
  let r860 = [R 65] in
  let r861 = Sub (r794) :: r860 in
  let r862 = S (T T_RBRACKET) :: r861 in
  let r863 = [R 88] in
  let r864 = S (T T_LIDENT) :: r863 in
  let r865 = [R 127] in
  let r866 = [R 110] in
  let r867 = Sub (r63) :: r866 in
  let r868 = S (T T_EQUAL) :: r867 in
  let r869 = Sub (r63) :: r868 in
  let r870 = [R 60] in
  let r871 = R 463 :: r870 in
  let r872 = Sub (r869) :: r871 in
  let r873 = [R 61] in
  let r874 = [R 77] in
  let r875 = Sub (r796) :: r874 in
  let r876 = [R 25] in
  let r877 = R 463 :: r876 in
  let r878 = Sub (r875) :: r877 in
  let r879 = S (T T_COLON) :: r878 in
  let r880 = S (T T_LIDENT) :: r879 in
  let r881 = R 85 :: r880 in
  let r882 = [R 78] in
  let r883 = Sub (r875) :: r882 in
  let r884 = S (T T_MINUSGREATER) :: r883 in
  let r885 = Sub (r57) :: r884 in
  let r886 = S (T T_COLON) :: r885 in
  let r887 = [R 79] in
  let r888 = Sub (r875) :: r887 in
  let r889 = S (T T_MINUSGREATER) :: r888 in
  let r890 = [R 80] in
  let r891 = Sub (r875) :: r890 in
  let r892 = S (T T_MINUSGREATER) :: r891 in
  let r893 = [R 534] in
  let r894 = [R 536] in
  let r895 = S (T T_RBRACKET) :: r894 in
  let r896 = Sub (r131) :: r895 in
  let r897 = [R 81] in
  let r898 = Sub (r875) :: r897 in
  let r899 = [R 13] in
  let r900 = R 463 :: r899 in
  let r901 = R 112 :: r900 in
  let r902 = R 722 :: r901 in
  let r903 = S (T T_LIDENT) :: r902 in
  let r904 = R 411 :: r903 in
  let r905 = [R 724] in
  let r906 = [R 115] in
  let r907 = R 17 :: r906 in
  let r908 = R 233 :: r907 in
  let r909 = R 400 :: r908 in
  let r910 = Sub (r194) :: r909 in
  let r911 = [R 116] in
  let r912 = Sub (r910) :: r911 in
  let r913 = [R 732] in
  let r914 = R 402 :: r913 in
  let r915 = Sub (r912) :: r914 in
  let r916 = [R 735] in
  let r917 = R 402 :: r916 in
  let r918 = S (T T_RBRACE) :: r917 in
  let r919 = [R 403] in
  let r920 = S (T T_RBRACKET) :: r919 in
  let r921 = R 783 :: r920 in
  let r922 = [R 19] in
  let r923 = R 17 :: r922 in
  let r924 = R 233 :: r923 in
  let r925 = R 400 :: r924 in
  let r926 = [R 501] in
  let r927 = [R 12] in
  let r928 = R 463 :: r927 in
  let r929 = S (N N_module_type) :: r928 in
  let r930 = S (T T_COLON) :: r929 in
  let r931 = S (T T_UIDENT) :: r930 in
  let r932 = [R 515] in
  let r933 = [R 9] in
  let r934 = R 463 :: r933 in
  let r935 = Sub (r796) :: r934 in
  let r936 = S (T T_EQUAL) :: r935 in
  let r937 = S (T T_LIDENT) :: r936 in
  let r938 = R 85 :: r937 in
  let r939 = R 771 :: r938 in
  let r940 = [R 8] in
  let r941 = R 463 :: r940 in
  let r942 = Sub (r875) :: r941 in
  let r943 = S (T T_COLON) :: r942 in
  let r944 = S (T T_LIDENT) :: r943 in
  let r945 = R 85 :: r944 in
  let r946 = R 771 :: r945 in
  let r947 = [R 72] in
  let r948 = Sub (r36) :: r947 in
  let r949 = [R 28] in
  let r950 = Sub (r948) :: r949 in
  let r951 = [R 44] in
  let r952 = Sub (r950) :: r951 in
  let r953 = S (T T_EQUAL) :: r952 in
  let r954 = [R 22] in
  let r955 = R 463 :: r954 in
  let r956 = Sub (r953) :: r955 in
  let r957 = S (T T_LIDENT) :: r956 in
  let r958 = R 85 :: r957 in
  let r959 = [R 73] in
  let r960 = S (T T_END) :: r959 in
  let r961 = Sub (r168) :: r960 in
  let r962 = [R 764] in
  let r963 = Sub (r1) :: r962 in
  let r964 = S (T T_EQUAL) :: r963 in
  let r965 = Sub (r626) :: r964 in
  let r966 = R 355 :: r965 in
  let r967 = R 17 :: r966 in
  let r968 = R 416 :: r967 in
  let r969 = [R 36] in
  let r970 = R 463 :: r969 in
  let r971 = [R 763] in
  let r972 = Sub (r63) :: r971 in
  let r973 = S (T T_COLON) :: r972 in
  let r974 = Sub (r626) :: r973 in
  let r975 = [R 762] in
  let r976 = Sub (r63) :: r975 in
  let r977 = S (T T_COLON) :: r976 in
  let r978 = [R 765] in
  let r979 = Sub (r1) :: r978 in
  let r980 = [R 314] in
  let r981 = Sub (r393) :: r980 in
  let r982 = Sub (r522) :: r981 in
  let r983 = R 468 :: r982 in
  let r984 = R 17 :: r983 in
  let r985 = R 416 :: r984 in
  let r986 = [R 37] in
  let r987 = R 463 :: r986 in
  let r988 = [R 313] in
  let r989 = Sub (r840) :: r988 in
  let r990 = S (T T_COLON) :: r989 in
  let r991 = Sub (r522) :: r990 in
  let r992 = [R 312] in
  let r993 = Sub (r840) :: r992 in
  let r994 = S (T T_COLON) :: r993 in
  let r995 = [R 315] in
  let r996 = Sub (r1) :: r995 in
  let r997 = S (T T_EQUAL) :: r996 in
  let r998 = [R 316] in
  let r999 = Sub (r1) :: r998 in
  let r1000 = S (T T_EQUAL) :: r999 in
  let r1001 = Sub (r63) :: r1000 in
  let r1002 = S (T T_DOT) :: r1001 in
  let r1003 = [R 39] in
  let r1004 = R 463 :: r1003 in
  let r1005 = Sub (r1) :: r1004 in
  let r1006 = [R 35] in
  let r1007 = R 463 :: r1006 in
  let r1008 = R 426 :: r1007 in
  let r1009 = Sub (r950) :: r1008 in
  let r1010 = R 17 :: r1009 in
  let r1011 = [R 75] in
  let r1012 = S (T T_RPAREN) :: r1011 in
  let r1013 = [R 32] in
  let r1014 = Sub (r950) :: r1013 in
  let r1015 = S (T T_IN) :: r1014 in
  let r1016 = Sub (r265) :: r1015 in
  let r1017 = R 17 :: r1016 in
  let r1018 = R 416 :: r1017 in
  let r1019 = [R 71] in
  let r1020 = Sub (r36) :: r1019 in
  let r1021 = S (T T_RBRACKET) :: r1020 in
  let r1022 = [R 47] in
  let r1023 = Sub (r950) :: r1022 in
  let r1024 = S (T T_MINUSGREATER) :: r1023 in
  let r1025 = Sub (r458) :: r1024 in
  let r1026 = [R 29] in
  let r1027 = Sub (r1025) :: r1026 in
  let r1028 = [R 31] in
  let r1029 = Sub (r950) :: r1028 in
  let r1030 = [R 74] in
  let r1031 = S (T T_RPAREN) :: r1030 in
  let r1032 = [R 425] in
  let r1033 = [R 38] in
  let r1034 = R 463 :: r1033 in
  let r1035 = Sub (r869) :: r1034 in
  let r1036 = [R 40] in
  let r1037 = [R 45] in
  let r1038 = Sub (r950) :: r1037 in
  let r1039 = S (T T_EQUAL) :: r1038 in
  let r1040 = [R 46] in
  let r1041 = [R 682] in
  let r1042 = [R 704] in
  let r1043 = [R 11] in
  let r1044 = R 463 :: r1043 in
  let r1045 = Sub (r273) :: r1044 in
  let r1046 = S (T T_UIDENT) :: r1045 in
  let r1047 = [R 698] in
  let r1048 = [R 7] in
  let r1049 = R 463 :: r1048 in
  let r1050 = Sub (r953) :: r1049 in
  let r1051 = S (T T_LIDENT) :: r1050 in
  let r1052 = R 85 :: r1051 in
  let r1053 = R 771 :: r1052 in
  let r1054 = [R 681] in
  let r1055 = R 702 :: r1054 in
  let r1056 = [R 213] in
  let r1057 = R 17 :: r1056 in
  let r1058 = Sub (r212) :: r1057 in
  let r1059 = [R 52] in
  let r1060 = S (T T_RPAREN) :: r1059 in
  let r1061 = [R 125] in
  let r1062 = [R 132] in
  let r1063 = Sub (r81) :: r1062 in
  let r1064 = [R 6] in
  let r1065 = S (T T_BACKQUOTE) :: r142 in
  let r1066 = [R 358] in
  let r1067 = Sub (r1065) :: r1066 in
  let r1068 = [R 540] in
  let r1069 = S (T T_RBRACKET) :: r1068 in
  let r1070 = [R 219] in
  let r1071 = R 17 :: r1070 in
  let r1072 = S (T T_SEMI) :: r1071 in
  let r1073 = R 17 :: r1072 in
  let r1074 = Sub (r243) :: r1073 in
  let r1075 = [R 529] in
  let r1076 = [R 523] in
  let r1077 = [R 533] in
  let r1078 = [R 727] in
  let r1079 = [R 730] in
  let r1080 = R 402 :: r1079 in
  let r1081 = S (T T_RBRACE) :: r1080 in
  let r1082 = Sub (r228) :: r1081 in
  let r1083 = [R 733] in
  let r1084 = S (T T_DOTDOT) :: r1083 in
  let r1085 = [R 734] in
  let r1086 = R 402 :: r1085 in
  let r1087 = S (T T_RBRACE) :: r1086 in
  let r1088 = [R 731] in
  let r1089 = [R 726] in
  let r1090 = [R 494] in
  let r1091 = Sub (r198) :: r1090 in
  let r1092 = [R 498] in
  let r1093 = R 463 :: r1092 in
  let r1094 = Sub (r1091) :: r1093 in
  let r1095 = R 468 :: r1094 in
  let r1096 = [R 767] in
  let r1097 = R 463 :: r1096 in
  let r1098 = [R 591] in
  let r1099 = S (T T_RPAREN) :: r1098 in
  let r1100 = S (N N_module_expr) :: r1099 in
  let r1101 = [R 592] in
  let r1102 = S (T T_RPAREN) :: r1101 in
  let r1103 = [R 557] in
  let r1104 = [R 560] in
  let r1105 = [R 138] in
  let r1106 = S (N N_match_cases) :: r1105 in
  let r1107 = [R 140] in
  let r1108 = [R 139] in
  let r1109 = [R 240] in
  let r1110 = S (T T_EOF) :: r1109 in
  let r1111 = S (N N_structure) :: r1110 in
  let r1112 = S (T T_END) :: r1111 in
  let r1113 = [R 239] in
  let r1114 = [R 242] in
  let r1115 = [R 427] in
  function
  | 0 | 1737 | 1741 -> Nothing
  | 1736 -> One ([R 0])
  | 1740 -> One ([R 1])
  | 1744 -> One ([R 2])
  | 226 -> One ([R 3])
  | 225 -> One ([R 4])
  | 79 -> One (R 17 :: r42)
  | 81 -> One (R 17 :: r43)
  | 145 -> One (R 17 :: r100)
  | 207 -> One (R 17 :: r152)
  | 250 -> One (R 17 :: r192)
  | 361 -> One (R 17 :: r290)
  | 424 -> One (R 17 :: r326)
  | 603 -> One (R 17 :: r515)
  | 733 -> One (R 17 :: r587)
  | 1180 -> One (R 17 :: r821)
  | 1189 -> One (R 17 :: r830)
  | 1206 -> One (R 17 :: r846)
  | 1221 -> One (R 17 :: r851)
  | 1246 -> One (R 17 :: r872)
  | 1300 -> One (R 17 :: r904)
  | 1341 -> One (R 17 :: r931)
  | 1358 -> One (R 17 :: r939)
  | 1369 -> One (R 17 :: r946)
  | 1388 -> One (R 17 :: r961)
  | 1444 -> One (R 17 :: r1005)
  | 1462 -> One (R 17 :: r1027)
  | 1488 -> One (R 17 :: r1035)
  | 1524 -> One (R 17 :: r1046)
  | 1542 -> One (R 17 :: r1053)
  | 1550 -> One ([R 23])
  | 1549 -> One ([R 24])
  | 1378 -> One ([R 26])
  | 1377 -> One ([R 27])
  | 1470 -> One ([R 30])
  | 1473 -> One ([R 33])
  | 1468 -> One ([R 34])
  | 1494 -> One ([R 41])
  | 1495 -> One ([R 43])
  | 1475 -> One ([R 48])
  | 1255 -> One ([R 62])
  | 1256 -> One ([R 64])
  | 1243 -> One ([R 68])
  | 1240 -> One ([R 69])
  | 1367 -> One ([R 83])
  | 1366 -> One ([R 84])
  | 536 -> One ([R 89])
  | 537 -> One ([R 90])
  | 586 -> One ([R 91])
  | 457 -> One ([R 93])
  | 68 -> One ([R 94])
  | 456 -> One ([R 95])
  | 167 | 260 -> One ([R 96])
  | 168 -> One ([R 101])
  | 317 -> One ([R 102])
  | 67 -> One ([R 108])
  | 1329 -> One ([R 117])
  | 1323 -> One ([R 118])
  | 387 -> One ([R 120])
  | 914 -> One ([R 133])
  | 655 -> One ([R 144])
  | 825 -> One ([R 145])
  | 674 -> One ([R 158])
  | 683 -> One ([R 159])
  | 663 -> One ([R 160])
  | 651 -> One ([R 202])
  | 681 -> One ([R 203])
  | 848 -> One ([R 207])
  | 1 -> One (R 208 :: r6)
  | 60 -> One (R 208 :: r23)
  | 63 -> One (R 208 :: r26)
  | 65 -> One (R 208 :: r31)
  | 71 -> One (R 208 :: r38)
  | 90 -> One (R 208 :: r68)
  | 227 -> One (R 208 :: r171)
  | 241 -> One (R 208 :: r183)
  | 428 -> One (R 208 :: r331)
  | 430 -> One (R 208 :: r336)
  | 433 -> One (R 208 :: r349)
  | 453 -> One (R 208 :: r370)
  | 471 -> One (R 208 :: r381)
  | 540 -> One (R 208 :: r436)
  | 541 -> One (R 208 :: r440)
  | 548 -> One (R 208 :: r447)
  | 550 -> One (R 208 :: r450)
  | 555 -> One (R 208 :: r464)
  | 575 -> One (R 208 :: r483)
  | 579 -> One (R 208 :: r494)
  | 588 -> One (R 208 :: r501)
  | 601 -> One (R 208 :: r511)
  | 632 -> One (R 208 :: r520)
  | 1009 -> One (R 208 :: r690)
  | 1021 -> One (R 208 :: r702)
  | 1106 -> One (R 208 :: r747)
  | 1110 -> One (R 208 :: r756)
  | 1132 -> One (R 208 :: r776)
  | 1155 -> One (R 208 :: r792)
  | 1692 -> One (R 208 :: r1100)
  | 862 -> One ([R 218])
  | 365 -> One ([R 229])
  | 364 -> One ([R 230])
  | 413 -> One ([R 231])
  | 414 -> One ([R 232])
  | 134 | 406 -> One ([R 237])
  | 1615 -> One ([R 241])
  | 305 -> One ([R 250])
  | 306 -> One ([R 251])
  | 828 -> One ([R 263])
  | 870 -> One ([R 275])
  | 869 -> One ([R 276])
  | 443 -> One ([R 280])
  | 447 -> One ([R 282])
  | 671 -> One ([R 290])
  | 753 -> One ([R 291])
  | 560 -> One ([R 293])
  | 571 -> One ([R 296])
  | 667 -> One ([R 298])
  | 754 -> One ([R 299])
  | 960 -> One ([R 302])
  | 965 -> One ([R 303])
  | 1627 -> One ([R 305])
  | 1625 -> One ([R 306])
  | 1626 -> One ([R 307])
  | 1628 -> One ([R 308])
  | 1624 -> One ([R 309])
  | 1614 -> One ([R 310])
  | 1613 -> One ([R 311])
  | 636 -> One ([R 317])
  | 618 -> One ([R 333])
  | 616 -> One ([R 336])
  | 607 -> One ([R 338])
  | 617 -> One ([R 339])
  | 619 -> One ([R 340])
  | 369 -> One ([R 341])
  | 409 -> One ([R 348])
  | 403 -> One ([R 349])
  | 408 -> One ([R 353])
  | 1191 -> One (R 355 :: r834)
  | 1399 -> One (R 355 :: r974)
  | 271 | 1404 -> One ([R 356])
  | 1610 -> One ([R 359])
  | 149 -> One ([R 361])
  | 86 | 93 -> One ([R 363])
  | 106 -> One ([R 364])
  | 105 -> One ([R 365])
  | 104 -> One ([R 366])
  | 103 -> One ([R 367])
  | 102 -> One ([R 368])
  | 113 -> One ([R 370])
  | 116 -> One ([R 372])
  | 119 -> One ([R 374])
  | 107 -> One ([R 375])
  | 122 | 627 -> One ([R 376])
  | 96 | 240 | 600 -> One ([R 377])
  | 95 | 599 -> One ([R 378])
  | 100 | 626 | 947 -> One ([R 379])
  | 99 | 625 -> One ([R 380])
  | 84 -> One ([R 381])
  | 109 -> One ([R 382])
  | 101 -> One ([R 383])
  | 108 -> One ([R 384])
  | 98 -> One ([R 385])
  | 121 -> One ([R 386])
  | 123 -> One ([R 387])
  | 120 -> One ([R 389])
  | 94 -> One ([R 390])
  | 97 -> One ([R 391])
  | 209 -> One ([R 392])
  | 208 -> One (R 393 :: r157)
  | 177 -> One (R 394 :: r134)
  | 1710 -> One (R 394 :: r1106)
  | 178 -> One ([R 395])
  | 1640 -> One (R 402 :: r1078)
  | 1656 -> One (R 402 :: r1088)
  | 1658 -> One (R 402 :: r1089)
  | 444 -> One (R 404 :: r363)
  | 507 -> One (R 404 :: r390)
  | 846 -> One (R 404 :: r622)
  | 854 -> One (R 404 :: r625)
  | 991 -> One (R 404 :: r675)
  | 445 | 498 | 847 | 861 -> One ([R 405])
  | 878 -> One ([R 406])
  | 372 -> One (R 411 :: r306)
  | 389 -> One ([R 412])
  | 346 -> One (R 416 :: r269)
  | 1003 -> One (R 416 :: r685)
  | 1448 -> One (R 416 :: r1010)
  | 347 -> One ([R 417])
  | 475 -> One ([R 429])
  | 480 -> One ([R 433])
  | 474 -> One ([R 434])
  | 473 -> One ([R 437])
  | 735 -> One ([R 442])
  | 749 -> One ([R 445])
  | 499 -> One ([R 450])
  | 566 -> One ([R 451])
  | 1381 -> One ([R 455])
  | 353 -> One (R 463 :: r270)
  | 1253 -> One (R 463 :: r873)
  | 1354 -> One (R 463 :: r932)
  | 1492 -> One (R 463 :: r1036)
  | 1537 -> One (R 463 :: r1047)
  | 1552 -> One (R 463 :: r1055)
  | 1117 -> One ([R 467])
  | 1306 -> One (R 468 :: r915)
  | 1419 -> One (R 468 :: r991)
  | 1649 -> One (R 468 :: r1084)
  | 258 | 1424 -> One ([R 469])
  | 1210 -> One ([R 471])
  | 1208 -> One ([R 472])
  | 1211 -> One ([R 473])
  | 1209 -> One ([R 474])
  | 435 -> One ([R 476])
  | 1530 -> One ([R 478])
  | 1529 -> One ([R 479])
  | 1348 -> One ([R 481])
  | 1347 -> One ([R 482])
  | 189 -> One ([R 485])
  | 728 -> One ([R 490])
  | 732 -> One ([R 491])
  | 1673 -> One ([R 495])
  | 1670 -> One ([R 496])
  | 1130 -> One (R 499 :: r769)
  | 1131 -> One (R 499 :: r770)
  | 1335 -> One (R 499 :: r926)
  | 1729 -> One (R 499 :: r1112)
  | 1298 -> One ([R 502])
  | 1349 -> One ([R 503])
  | 1299 -> One ([R 504])
  | 1337 -> One ([R 505])
  | 1339 -> One ([R 506])
  | 1352 -> One ([R 507])
  | 1353 -> One ([R 508])
  | 1340 -> One ([R 509])
  | 1351 -> One ([R 510])
  | 1350 -> One ([R 511])
  | 1338 -> One ([R 512])
  | 1368 -> One ([R 513])
  | 1357 -> One ([R 514])
  | 1356 -> One ([R 516])
  | 238 -> One ([R 519])
  | 235 -> One ([R 521])
  | 188 -> One ([R 526])
  | 193 -> One ([R 527])
  | 1636 -> One ([R 528])
  | 215 | 1290 -> One ([R 542])
  | 592 -> One ([R 551])
  | 587 -> One ([R 552])
  | 630 -> One ([R 553])
  | 631 -> One ([R 554])
  | 629 | 682 -> One ([R 555])
  | 594 | 662 -> One ([R 556])
  | 772 | 822 -> One ([R 561])
  | 628 -> One ([R 595])
  | 827 -> One ([R 598])
  | 458 -> One ([R 599])
  | 462 -> One ([R 602])
  | 504 -> One ([R 604])
  | 466 -> One ([R 605])
  | 461 -> One ([R 607])
  | 503 -> One ([R 608])
  | 483 -> One ([R 616])
  | 28 -> One ([R 617])
  | 8 -> One ([R 618])
  | 52 -> One ([R 620])
  | 51 -> One ([R 621])
  | 50 -> One ([R 622])
  | 49 -> One ([R 623])
  | 48 -> One ([R 624])
  | 47 -> One ([R 625])
  | 46 -> One ([R 626])
  | 45 -> One ([R 627])
  | 44 -> One ([R 628])
  | 43 -> One ([R 629])
  | 42 -> One ([R 630])
  | 41 -> One ([R 631])
  | 40 -> One ([R 632])
  | 39 -> One ([R 633])
  | 38 -> One ([R 634])
  | 37 -> One ([R 635])
  | 36 -> One ([R 636])
  | 35 -> One ([R 637])
  | 34 -> One ([R 638])
  | 33 -> One ([R 639])
  | 32 -> One ([R 640])
  | 31 -> One ([R 641])
  | 30 -> One ([R 642])
  | 29 -> One ([R 643])
  | 27 -> One ([R 644])
  | 26 -> One ([R 645])
  | 25 -> One ([R 646])
  | 24 -> One ([R 647])
  | 23 -> One ([R 648])
  | 22 -> One ([R 649])
  | 21 -> One ([R 650])
  | 20 -> One ([R 651])
  | 19 -> One ([R 652])
  | 18 -> One ([R 653])
  | 17 -> One ([R 654])
  | 16 -> One ([R 655])
  | 15 -> One ([R 656])
  | 14 -> One ([R 657])
  | 13 -> One ([R 658])
  | 12 -> One ([R 659])
  | 11 -> One ([R 660])
  | 10 -> One ([R 661])
  | 9 -> One ([R 662])
  | 7 -> One ([R 663])
  | 6 -> One ([R 664])
  | 5 -> One ([R 665])
  | 4 -> One ([R 666])
  | 3 -> One ([R 667])
  | 1522 -> One ([R 668])
  | 1577 -> One ([R 671])
  | 1568 -> One ([R 672])
  | 1576 -> One ([R 673])
  | 1567 -> One ([R 674])
  | 1566 -> One ([R 675])
  | 1508 -> One ([R 683])
  | 1535 | 1555 -> One ([R 684])
  | 1536 | 1556 -> One ([R 685])
  | 1531 -> One ([R 686])
  | 1505 -> One ([R 687])
  | 1506 -> One ([R 688])
  | 1519 -> One ([R 689])
  | 1521 -> One ([R 690])
  | 1534 -> One ([R 691])
  | 1523 -> One ([R 692])
  | 1533 -> One ([R 693])
  | 1532 -> One ([R 694])
  | 1541 -> One ([R 695])
  | 1540 -> One ([R 696])
  | 1520 -> One ([R 697])
  | 1539 -> One ([R 701])
  | 1509 -> One (R 702 :: r1042)
  | 427 -> One ([R 705])
  | 426 -> One ([R 706])
  | 337 -> One ([R 710])
  | 338 -> One ([R 711])
  | 340 -> One ([R 712])
  | 342 -> One ([R 713])
  | 339 -> One ([R 714])
  | 336 -> One ([R 715])
  | 1334 -> One ([R 720])
  | 1333 -> One ([R 721])
  | 1648 -> One ([R 723])
  | 1639 -> One ([R 725])
  | 902 | 911 -> One ([R 745])
  | 653 -> One ([R 750])
  | 652 -> One ([R 751])
  | 151 -> One ([R 754])
  | 152 -> One ([R 755])
  | 341 -> One ([R 760])
  | 344 -> One ([R 761])
  | 1196 -> One (R 771 :: r838)
  | 1259 -> One (R 771 :: r881)
  | 1383 -> One (R 771 :: r958)
  | 1165 -> One ([R 772])
  | 223 -> One ([R 780])
  | 285 -> One ([R 781])
  | 284 -> One ([R 782])
  | 282 -> One ([R 784])
  | 283 -> One ([R 785])
  | 375 -> One ([R 786])
  | 376 -> One ([R 787])
  | 865 -> One (S (T T_WITH) :: r636)
  | 343 | 351 -> One (S (T T_UIDENT) :: r41)
  | 198 -> One (S (T T_UIDENT) :: r150)
  | 246 -> One (S (T T_TYPE) :: r189)
  | 524 -> One (S (T T_TYPE) :: r412)
  | 1033 -> One (S (T T_TYPE) :: r708)
  | 1162 | 1382 -> One (S (T T_TYPE) :: r804)
  | 311 | 584 -> One (S (T T_RPAREN) :: r44)
  | 170 | 261 -> One (S (T T_RPAREN) :: r122)
  | 262 -> One (S (T T_RPAREN) :: r205)
  | 312 -> One (S (T T_RPAREN) :: r252)
  | 363 -> One (S (T T_RPAREN) :: r291)
  | 469 -> One (S (T T_RPAREN) :: r379)
  | 487 -> One (S (T T_RPAREN) :: r385)
  | 609 -> One (S (T T_RPAREN) :: r516)
  | 842 -> One (S (T T_RPAREN) :: r619)
  | 898 -> One (S (T T_RPAREN) :: r647)
  | 1135 -> One (S (T T_RPAREN) :: r783)
  | 1632 -> One (S (T T_RPAREN) :: r1076)
  | 1701 -> One (S (T T_RPAREN) :: r1103)
  | 1703 -> One (S (T T_RPAREN) :: r1104)
  | 114 -> One (S (T T_RBRACKET) :: r76)
  | 181 -> One (S (T T_RBRACKET) :: r135)
  | 264 | 1637 -> One (S (T T_RBRACKET) :: r206)
  | 314 -> One (S (T T_RBRACKET) :: r253)
  | 852 -> One (S (T T_RBRACKET) :: r623)
  | 1277 -> One (S (T T_RBRACKET) :: r893)
  | 117 -> One (S (T T_RBRACE) :: r77)
  | 1595 -> One (S (T T_QUOTE) :: r1063)
  | 256 -> One (S (T T_PLUSEQ) :: r204)
  | 1663 -> One (S (T T_PLUSEQ) :: r1095)
  | 1224 -> One (S (T T_OPEN) :: r857)
  | 1452 -> One (S (T T_OPEN) :: r1018)
  | 141 -> One (S (T T_MODULE) :: r97)
  | 326 -> One (S (T T_MINUSGREATER) :: r259)
  | 1285 -> One (S (T T_MINUSGREATER) :: r898)
  | 110 -> One (S (T T_LPAREN) :: r75)
  | 318 -> One (S (T T_LPAREN) :: r256)
  | 137 -> One (S (T T_LIDENT) :: r88)
  | 1264 -> One (S (T T_LIDENT) :: r886)
  | 1484 -> One (S (T T_LIDENT) :: r1032)
  | 672 -> One (S (T T_LESSMINUS) :: r558)
  | 221 -> One (S (T T_LBRACKET) :: r164)
  | 278 -> One (S (T T_LBRACKET) :: r241)
  | 1311 -> One (S (T T_LBRACKET) :: r921)
  | 1643 -> One (S (T T_LBRACE) :: r1082)
  | 233 -> One (S (T T_INT) :: r177)
  | 236 -> One (S (T T_INT) :: r178)
  | 895 -> One (S (T T_INFIXOP3) :: r646)
  | 664 -> One (S (T T_IN) :: r554)
  | 668 -> One (S (T T_IN) :: r556)
  | 974 -> One (S (T T_IN) :: r672)
  | 1466 -> One (S (T T_IN) :: r1029)
  | 529 -> One (S (T T_GREATERRBRACE) :: r419)
  | 985 -> One (S (T T_GREATERRBRACE) :: r673)
  | 174 -> One (S (T T_GREATER) :: r127)
  | 1622 -> One (S (T T_GREATER) :: r1075)
  | 1039 -> One (S (T T_EQUAL) :: r710)
  | 1058 -> One (S (T T_EQUAL) :: r722)
  | 1413 -> One (S (T T_EQUAL) :: r979)
  | 1570 -> One (S (T T_EQUAL) :: r1058)
  | 1734 -> One (S (T T_EOF) :: r1113)
  | 1738 -> One (S (T T_EOF) :: r1114)
  | 1742 -> One (S (T T_EOF) :: r1115)
  | 888 -> One (S (T T_END) :: r642)
  | 166 -> One (S (T T_DOTDOT) :: r120)
  | 169 -> One (S (T T_DOTDOT) :: r121)
  | 74 -> One (S (T T_DOT) :: r40)
  | 290 -> One (S (T T_DOT) :: r250)
  | 404 -> One (S (T T_DOT) :: r318)
  | 467 -> One (S (T T_DOT) :: r378)
  | 1053 -> One (S (T T_DOT) :: r720)
  | 1215 -> One (S (T T_DOT) :: r848)
  | 1233 -> One (S (T T_DOT) :: r864)
  | 367 -> One (S (T T_COLON) :: r294)
  | 1136 -> One (S (T T_COLON) :: r787)
  | 1616 -> One (S (T T_COLON) :: r1074)
  | 437 -> One (S (T T_BARRBRACKET) :: r350)
  | 527 -> One (S (T T_BARRBRACKET) :: r413)
  | 844 -> One (S (T T_BARRBRACKET) :: r620)
  | 184 | 1276 -> One (S (T T_BAR) :: r140)
  | 1280 -> One (S (T T_BAR) :: r896)
  | 345 -> One (S (N N_structure) :: r264)
  | 1507 -> One (S (N N_structure) :: r1041)
  | 229 -> One (S (N N_pattern) :: r173)
  | 568 | 933 -> One (S (N N_pattern) :: r180)
  | 452 -> One (S (N N_pattern) :: r365)
  | 476 -> One (S (N N_pattern) :: r382)
  | 478 -> One (S (N N_pattern) :: r383)
  | 481 -> One (S (N N_pattern) :: r384)
  | 489 -> One (S (N N_pattern) :: r386)
  | 491 -> One (S (N N_pattern) :: r387)
  | 736 -> One (S (N N_pattern) :: r588)
  | 741 -> One (S (N N_pattern) :: r589)
  | 743 -> One (S (N N_pattern) :: r590)
  | 745 -> One (S (N N_pattern) :: r591)
  | 1100 -> One (S (N N_pattern) :: r740)
  | 359 -> One (S (N N_module_type) :: r284)
  | 360 -> One (S (N N_module_type) :: r286)
  | 401 -> One (S (N N_module_type) :: r316)
  | 613 -> One (S (N N_module_type) :: r518)
  | 1012 -> One (S (N N_module_type) :: r693)
  | 423 -> One (S (N N_module_expr) :: r323)
  | 559 -> One (S (N N_let_pattern) :: r470)
  | 224 -> One (S (N N_expr) :: r165)
  | 533 -> One (S (N N_expr) :: r422)
  | 539 -> One (S (N N_expr) :: r434)
  | 654 -> One (S (N N_expr) :: r544)
  | 675 -> One (S (N N_expr) :: r559)
  | 677 -> One (S (N N_expr) :: r560)
  | 679 -> One (S (N N_expr) :: r561)
  | 684 -> One (S (N N_expr) :: r562)
  | 686 -> One (S (N N_expr) :: r563)
  | 688 -> One (S (N N_expr) :: r564)
  | 690 -> One (S (N N_expr) :: r565)
  | 692 -> One (S (N N_expr) :: r566)
  | 694 -> One (S (N N_expr) :: r567)
  | 696 -> One (S (N N_expr) :: r568)
  | 698 -> One (S (N N_expr) :: r569)
  | 700 -> One (S (N N_expr) :: r570)
  | 702 -> One (S (N N_expr) :: r571)
  | 704 -> One (S (N N_expr) :: r572)
  | 706 -> One (S (N N_expr) :: r573)
  | 708 -> One (S (N N_expr) :: r574)
  | 710 -> One (S (N N_expr) :: r575)
  | 712 -> One (S (N N_expr) :: r576)
  | 714 -> One (S (N N_expr) :: r577)
  | 716 -> One (S (N N_expr) :: r578)
  | 718 -> One (S (N N_expr) :: r579)
  | 720 -> One (S (N N_expr) :: r580)
  | 723 -> One (S (N N_expr) :: r581)
  | 725 -> One (S (N N_expr) :: r582)
  | 777 -> One (S (N N_expr) :: r606)
  | 782 -> One (S (N N_expr) :: r607)
  | 787 -> One (S (N N_expr) :: r611)
  | 793 -> One (S (N N_expr) :: r612)
  | 798 -> One (S (N N_expr) :: r613)
  | 803 -> One (S (N N_expr) :: r614)
  | 810 -> One (S (N N_expr) :: r615)
  | 815 -> One (S (N N_expr) :: r616)
  | 820 -> One (S (N N_expr) :: r617)
  | 823 -> One (S (N N_expr) :: r618)
  | 880 -> One (S (N N_expr) :: r641)
  | 969 -> One (S (N N_expr) :: r670)
  | 1685 -> One (S (N N_expr) :: r1097)
  | 523 -> One (Sub (r1) :: r407)
  | 554 -> One (Sub (r1) :: r456)
  | 925 -> One (Sub (r1) :: r657)
  | 1102 -> One (Sub (r1) :: r741)
  | 1713 -> One (Sub (r1) :: r1107)
  | 1715 -> One (Sub (r1) :: r1108)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 88 -> One (Sub (r10) :: r50)
  | 332 -> One (Sub (r10) :: r262)
  | 729 -> One (Sub (r10) :: r584)
  | 1098 -> One (Sub (r10) :: r739)
  | 1104 -> One (Sub (r10) :: r744)
  | 70 -> One (Sub (r33) :: r34)
  | 538 -> One (Sub (r33) :: r432)
  | 585 -> One (Sub (r33) :: r499)
  | 591 -> One (Sub (r33) :: r502)
  | 645 -> One (Sub (r33) :: r535)
  | 647 -> One (Sub (r33) :: r536)
  | 656 -> One (Sub (r33) :: r545)
  | 892 -> One (Sub (r33) :: r644)
  | 131 -> One (Sub (r36) :: r79)
  | 191 -> One (Sub (r36) :: r143)
  | 1634 -> One (Sub (r36) :: r1077)
  | 493 -> One (Sub (r51) :: r388)
  | 747 -> One (Sub (r51) :: r592)
  | 217 -> One (Sub (r55) :: r161)
  | 324 -> One (Sub (r55) :: r257)
  | 939 -> One (Sub (r55) :: r664)
  | 1269 -> One (Sub (r57) :: r889)
  | 1273 -> One (Sub (r57) :: r892)
  | 140 -> One (Sub (r59) :: r91)
  | 173 -> One (Sub (r59) :: r126)
  | 1593 -> One (Sub (r59) :: r1061)
  | 379 -> One (Sub (r61) :: r308)
  | 1599 -> One (Sub (r61) :: r1064)
  | 449 -> One (Sub (r63) :: r364)
  | 515 -> One (Sub (r63) :: r402)
  | 545 -> One (Sub (r63) :: r443)
  | 561 -> One (Sub (r63) :: r471)
  | 872 -> One (Sub (r63) :: r637)
  | 876 -> One (Sub (r63) :: r640)
  | 903 -> One (Sub (r63) :: r649)
  | 907 -> One (Sub (r63) :: r651)
  | 950 -> One (Sub (r63) :: r667)
  | 997 -> One (Sub (r63) :: r680)
  | 1182 -> One (Sub (r63) :: r823)
  | 1237 -> One (Sub (r63) :: r865)
  | 1304 -> One (Sub (r63) :: r905)
  | 1585 -> One (Sub (r63) :: r1060)
  | 156 -> One (Sub (r81) :: r118)
  | 291 -> One (Sub (r81) :: r251)
  | 334 -> One (Sub (r81) :: r263)
  | 245 -> One (Sub (r93) :: r185)
  | 1074 -> One (Sub (r93) :: r724)
  | 1077 -> One (Sub (r93) :: r726)
  | 1080 -> One (Sub (r93) :: r728)
  | 1696 -> One (Sub (r93) :: r1102)
  | 161 -> One (Sub (r113) :: r119)
  | 153 -> One (Sub (r115) :: r117)
  | 205 -> One (Sub (r129) :: r151)
  | 183 -> One (Sub (r131) :: r137)
  | 195 -> One (Sub (r147) :: r149)
  | 394 -> One (Sub (r147) :: r314)
  | 213 -> One (Sub (r159) :: r160)
  | 231 -> One (Sub (r175) :: r176)
  | 1316 -> One (Sub (r194) :: r925)
  | 269 -> One (Sub (r218) :: r219)
  | 272 -> One (Sub (r221) :: r235)
  | 270 -> One (Sub (r228) :: r230)
  | 1308 -> One (Sub (r228) :: r918)
  | 1651 -> One (Sub (r228) :: r1087)
  | 287 -> One (Sub (r243) :: r247)
  | 1145 -> One (Sub (r265) :: r789)
  | 1019 -> One (Sub (r273) :: r694)
  | 397 -> One (Sub (r299) :: r315)
  | 371 -> One (Sub (r301) :: r302)
  | 382 -> One (Sub (r312) :: r313)
  | 438 -> One (Sub (r352) :: r355)
  | 439 -> One (Sub (r360) :: r362)
  | 937 -> One (Sub (r368) :: r661)
  | 463 -> One (Sub (r374) :: r375)
  | 510 -> One (Sub (r393) :: r394)
  | 511 -> One (Sub (r396) :: r397)
  | 520 -> One (Sub (r396) :: r403)
  | 512 -> One (Sub (r399) :: r401)
  | 521 -> One (Sub (r399) :: r406)
  | 534 -> One (Sub (r429) :: r431)
  | 864 -> One (Sub (r429) :: r634)
  | 944 -> One (Sub (r461) :: r665)
  | 557 -> One (Sub (r466) :: r467)
  | 569 -> One (Sub (r473) :: r476)
  | 934 -> One (Sub (r473) :: r660)
  | 1047 -> One (Sub (r473) :: r716)
  | 1432 -> One (Sub (r473) :: r1002)
  | 583 -> One (Sub (r496) :: r497)
  | 635 -> One (Sub (r522) :: r523)
  | 637 -> One (Sub (r522) :: r530)
  | 1425 -> One (Sub (r522) :: r994)
  | 640 -> One (Sub (r532) :: r533)
  | 643 -> One (Sub (r532) :: r534)
  | 1405 -> One (Sub (r626) :: r977)
  | 857 -> One (Sub (r630) :: r632)
  | 963 -> One (Sub (r668) :: r669)
  | 1043 -> One (Sub (r704) :: r711)
  | 1134 -> One (Sub (r779) :: r781)
  | 1174 -> One (Sub (r810) :: r815)
  | 1167 -> One (Sub (r812) :: r814)
  | 1431 -> One (Sub (r840) :: r997)
  | 1230 -> One (Sub (r859) :: r862)
  | 1458 -> One (Sub (r859) :: r1021)
  | 1480 -> One (Sub (r875) :: r1031)
  | 1497 -> One (Sub (r875) :: r1039)
  | 1451 -> One (Sub (r950) :: r1012)
  | 1501 -> One (Sub (r953) :: r1040)
  | 1394 -> One (Sub (r968) :: r970)
  | 1416 -> One (Sub (r985) :: r987)
  | 1607 -> One (Sub (r1067) :: r1069)
  | 727 -> One (r0)
  | 1728 -> One (r2)
  | 1727 -> One (r3)
  | 1726 -> One (r4)
  | 1725 -> One (r5)
  | 1724 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1723 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1557 -> One (r14)
  | 1722 -> One (r16)
  | 1721 -> One (r17)
  | 59 -> One (r18)
  | 1720 -> One (r19)
  | 1719 -> One (r20)
  | 1718 -> One (r21)
  | 1717 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1709 -> One (r25)
  | 64 -> One (r26)
  | 1708 -> One (r27)
  | 1707 -> One (r28)
  | 1706 -> One (r29)
  | 1705 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1691 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 85 -> One (r44)
  | 1700 -> One (r45)
  | 1699 -> One (r46)
  | 87 | 432 | 535 | 886 -> One (r47)
  | 1690 -> One (r48)
  | 1689 -> One (r49)
  | 89 -> One (r50)
  | 129 -> One (r52)
  | 190 -> One (r54)
  | 212 -> One (r56)
  | 211 -> One (r58)
  | 220 -> One (r60)
  | 383 -> One (r62)
  | 1688 -> One (r64)
  | 1684 -> One (r65)
  | 128 -> One (r66)
  | 127 -> One (r67)
  | 91 -> One (r68)
  | 124 -> One (r69)
  | 126 -> One (r71)
  | 125 -> One (r72)
  | 92 -> One (r73)
  | 112 -> One (r74)
  | 111 -> One (r75)
  | 115 -> One (r76)
  | 118 -> One (r77)
  | 130 | 144 | 175 -> One (r78)
  | 132 -> One (r79)
  | 135 -> One (r80)
  | 136 -> One (r82)
  | 133 -> One (r83)
  | 1683 -> One (r84)
  | 1682 -> One (r85)
  | 1681 -> One (r86)
  | 139 -> One (r87)
  | 138 -> One (r88)
  | 1680 -> One (r89)
  | 1679 -> One (r90)
  | 1678 -> One (r91)
  | 624 -> One (r92)
  | 1677 -> One (r94)
  | 1676 -> One (r95)
  | 143 -> One (r96)
  | 142 -> One (r97)
  | 1675 -> One (r98)
  | 1674 -> One (r99)
  | 146 -> One (r100)
  | 1662 -> One (r101)
  | 1661 -> One (r102)
  | 1660 -> One (r103)
  | 165 -> One (r104)
  | 164 | 255 -> One (r105)
  | 150 | 254 -> One (r106)
  | 148 | 253 -> One (r107)
  | 147 | 252 -> One (r108)
  | 155 -> One (r109)
  | 158 -> One (r111)
  | 154 -> One (r112)
  | 163 -> One (r114)
  | 160 -> One (r116)
  | 159 -> One (r117)
  | 157 -> One (r118)
  | 162 -> One (r119)
  | 1642 -> One (r120)
  | 1638 -> One (r121)
  | 171 -> One (r122)
  | 172 | 176 | 180 | 1272 -> One (r123)
  | 1631 -> One (r124)
  | 1630 -> One (r125)
  | 1629 -> One (r126)
  | 1612 -> One (r127)
  | 202 | 1279 -> One (r128)
  | 1284 -> One (r130)
  | 1606 -> One (r132)
  | 1605 -> One (r133)
  | 179 -> One (r134)
  | 182 -> One (r135)
  | 1604 -> One (r136)
  | 1603 -> One (r137)
  | 204 -> One (r138)
  | 203 -> One (r139)
  | 185 -> One (r140)
  | 187 -> One (r141)
  | 186 -> One (r142)
  | 192 -> One (r143)
  | 201 | 1289 -> One (r144)
  | 200 | 1288 -> One (r145)
  | 194 | 1287 -> One (r146)
  | 197 -> One (r148)
  | 196 -> One (r149)
  | 199 -> One (r150)
  | 206 -> One (r151)
  | 1602 -> One (r152)
  | 219 -> One (r153)
  | 1601 -> One (r155)
  | 1598 -> One (r156)
  | 210 -> One (r157)
  | 214 -> One (r158)
  | 216 -> One (r160)
  | 218 -> One (r161)
  | 1592 -> One (r162)
  | 1591 -> One (r163)
  | 222 -> One (r164)
  | 1590 -> One (r165)
  | 1393 -> One (r166)
  | 1392 -> One (r167)
  | 1589 -> One (r169)
  | 1588 -> One (r170)
  | 228 -> One (r171)
  | 1584 -> One (r172)
  | 1583 -> One (r173)
  | 230 -> One (r174)
  | 232 -> One (r176)
  | 234 -> One (r177)
  | 237 -> One (r178)
  | 949 -> One (r179)
  | 948 -> One (r180)
  | 244 -> One (r181)
  | 243 -> One (r182)
  | 242 -> One (r183)
  | 1582 -> One (r184)
  | 1581 -> One (r185)
  | 1580 -> One (r186)
  | 249 -> One (r187)
  | 248 -> One (r188)
  | 247 -> One (r189)
  | 1579 -> One (r190)
  | 1578 -> One (r191)
  | 251 -> One (r192)
  | 266 -> One (r193)
  | 1575 -> One (r195)
  | 1574 -> One (r196)
  | 1672 -> One (r197)
  | 1569 -> One (r199)
  | 1565 -> One (r201)
  | 331 -> One (r202)
  | 259 -> One (r203)
  | 257 -> One (r204)
  | 263 -> One (r205)
  | 265 -> One (r206)
  | 330 -> One (r207)
  | 329 -> One (r208)
  | 1667 -> One (r209)
  | 1666 -> One (r210)
  | 316 -> One (r211)
  | 323 -> One (r213)
  | 322 -> One (r214)
  | 310 -> One (r215)
  | 308 -> One (r217)
  | 309 -> One (r219)
  | 273 -> One (r220)
  | 307 -> One (r227)
  | 304 -> One (r229)
  | 303 -> One (r230)
  | 302 -> One (r231)
  | 301 -> One (r232)
  | 300 -> One (r233)
  | 286 -> One (r234)
  | 277 -> One (r235)
  | 276 -> One (r236)
  | 275 | 901 -> One (r237)
  | 274 | 761 | 805 | 900 -> One (r238)
  | 281 -> One (r239)
  | 280 -> One (r240)
  | 279 -> One (r241)
  | 299 -> One (r242)
  | 298 -> One (r244)
  | 297 -> One (r245)
  | 296 -> One (r246)
  | 295 -> One (r247)
  | 294 -> One (r249)
  | 293 -> One (r250)
  | 292 -> One (r251)
  | 313 -> One (r252)
  | 315 -> One (r253)
  | 321 -> One (r254)
  | 320 -> One (r255)
  | 319 -> One (r256)
  | 325 -> One (r257)
  | 328 -> One (r258)
  | 327 -> One (r259)
  | 1564 -> One (r260)
  | 1563 -> One (r261)
  | 333 -> One (r262)
  | 335 -> One (r263)
  | 1562 -> One (r264)
  | 352 -> One (r266)
  | 350 -> One (r267)
  | 349 -> One (r268)
  | 348 -> One (r269)
  | 354 -> One (r270)
  | 1083 -> One (r271)
  | 422 -> One (r272)
  | 1089 -> One (r274)
  | 1088 -> One (r275)
  | 1087 -> One (r276)
  | 1086 -> One (r277)
  | 419 -> One (r279)
  | 418 -> One (r280)
  | 358 -> One (r281)
  | 357 -> One (r282)
  | 356 -> One (r283)
  | 417 -> One (r284)
  | 416 -> One (r285)
  | 415 -> One (r286)
  | 412 -> One (r287)
  | 411 -> One (r288)
  | 410 -> One (r289)
  | 362 -> One (r290)
  | 366 -> One (r291)
  | 400 -> One (r292)
  | 370 -> One (r293)
  | 368 -> One (r294)
  | 393 -> One (r295)
  | 392 -> One (r296)
  | 391 -> One (r297)
  | 390 -> One (r298)
  | 399 -> One (r300)
  | 396 -> One (r302)
  | 378 -> One (r303)
  | 377 -> One (r304)
  | 374 -> One (r305)
  | 373 -> One (r306)
  | 381 -> One (r307)
  | 380 -> One (r308)
  | 386 -> One (r309)
  | 385 -> One (r310)
  | 384 -> One (r311)
  | 388 -> One (r313)
  | 395 -> One (r314)
  | 398 -> One (r315)
  | 402 -> One (r316)
  | 407 -> One (r317)
  | 405 -> One (r318)
  | 1085 -> One (r319)
  | 1084 -> One (r320)
  | 421 -> One (r321)
  | 612 -> One (r322)
  | 611 -> One (r323)
  | 1073 -> One (r324)
  | 1072 -> One (r325)
  | 425 -> One (r326)
  | 1071 -> One (r327)
  | 1070 -> One (r328)
  | 1069 -> One (r329)
  | 1068 -> One (r330)
  | 429 -> One (r331)
  | 1067 -> One (r332)
  | 1066 -> One (r333)
  | 1065 -> One (r334)
  | 1064 -> One (r335)
  | 431 -> One (r336)
  | 485 -> One (r337)
  | 484 -> One (r338)
  | 750 -> One (r340)
  | 740 -> One (r342)
  | 739 -> One (r343)
  | 738 -> One (r344)
  | 1063 -> One (r346)
  | 1062 -> One (r347)
  | 436 -> One (r348)
  | 434 -> One (r349)
  | 506 -> One (r350)
  | 502 -> One (r351)
  | 501 -> One (r353)
  | 500 -> One (r354)
  | 497 -> One (r355)
  | 451 -> One (r356)
  | 448 -> One (r357)
  | 442 -> One (r359)
  | 441 -> One (r361)
  | 440 -> One (r362)
  | 446 -> One (r363)
  | 450 -> One (r364)
  | 505 -> One (r365)
  | 459 | 544 -> One (r367)
  | 460 -> One (r369)
  | 454 -> One (r370)
  | 455 -> One (r371)
  | 465 -> One (r373)
  | 464 -> One (r375)
  | 496 -> One (r376)
  | 495 -> One (r377)
  | 468 -> One (r378)
  | 470 -> One (r379)
  | 486 -> One (r380)
  | 472 -> One (r381)
  | 477 -> One (r382)
  | 479 -> One (r383)
  | 482 -> One (r384)
  | 488 -> One (r385)
  | 490 -> One (r386)
  | 492 -> One (r387)
  | 494 -> One (r388)
  | 509 -> One (r389)
  | 508 -> One (r390)
  | 1038 -> One (r391)
  | 1037 -> One (r392)
  | 1061 -> One (r394)
  | 513 -> One (r395)
  | 519 -> One (r397)
  | 514 -> One (r398)
  | 518 -> One (r400)
  | 517 -> One (r401)
  | 516 -> One (r402)
  | 1032 -> One (r403)
  | 1031 -> One (r404)
  | 1030 -> One (r405)
  | 522 -> One (r406)
  | 1029 -> One (r407)
  | 996 -> One (r408)
  | 995 -> One (r409)
  | 994 -> One (r410)
  | 526 -> One (r411)
  | 525 -> One (r412)
  | 990 -> One (r413)
  | 851 -> One (r414)
  | 989 -> One (r416)
  | 988 -> One (r417)
  | 987 -> One (r418)
  | 531 -> One (r419)
  | 530 -> One (r420)
  | 532 -> One (r421)
  | 984 -> One (r422)
  | 879 -> One (r423)
  | 871 -> One (r424)
  | 868 -> One (r426)
  | 887 -> One (r428)
  | 983 -> One (r430)
  | 982 -> One (r431)
  | 981 -> One (r432)
  | 980 -> One (r433)
  | 979 -> One (r434)
  | 978 -> One (r435)
  | 977 -> One (r436)
  | 973 -> One (r437)
  | 972 -> One (r438)
  | 543 -> One (r439)
  | 542 -> One (r440)
  | 971 -> One (r441)
  | 547 -> One (r442)
  | 546 -> One (r443)
  | 968 -> One (r444)
  | 967 -> One (r445)
  | 966 -> One (r446)
  | 549 -> One (r447)
  | 962 -> One (r448)
  | 552 -> One (r449)
  | 551 -> One (r450)
  | 961 -> One (r451)
  | 959 -> One (r452)
  | 553 -> One (r453)
  | 958 -> One (r454)
  | 957 -> One (r455)
  | 956 -> One (r456)
  | 943 -> One (r457)
  | 932 -> One (r459)
  | 574 -> One (r460)
  | 955 -> One (r462)
  | 954 -> One (r463)
  | 556 -> One (r464)
  | 558 -> One (r465)
  | 567 -> One (r467)
  | 565 -> One (r468)
  | 564 -> One (r469)
  | 563 -> One (r470)
  | 562 -> One (r471)
  | 570 -> One (r472)
  | 953 -> One (r474)
  | 573 -> One (r475)
  | 572 -> One (r476)
  | 924 -> One (r477)
  | 923 -> One (r478)
  | 922 -> One (r479)
  | 921 -> One (r480)
  | 578 -> One (r481)
  | 577 -> One (r482)
  | 576 -> One (r483)
  | 915 -> One (r484)
  | 920 -> One (r486)
  | 919 -> One (r487)
  | 918 -> One (r488)
  | 917 -> One (r489)
  | 916 -> One (r490)
  | 913 -> One (r491)
  | 582 -> One (r492)
  | 581 -> One (r493)
  | 580 -> One (r494)
  | 906 | 912 -> One (r495)
  | 910 -> One (r497)
  | 891 -> One (r498)
  | 890 -> One (r499)
  | 590 -> One (r500)
  | 589 -> One (r501)
  | 593 -> One (r502)
  | 598 -> One (r503)
  | 597 -> One (r504)
  | 596 | 885 -> One (r505)
  | 884 -> One (r506)
  | 623 -> One (r507)
  | 622 -> One (r508)
  | 621 -> One (r509)
  | 620 -> One (r510)
  | 602 -> One (r511)
  | 608 -> One (r512)
  | 606 -> One (r513)
  | 605 -> One (r514)
  | 604 -> One (r515)
  | 610 -> One (r516)
  | 615 -> One (r517)
  | 614 -> One (r518)
  | 634 -> One (r519)
  | 633 -> One (r520)
  | 638 -> One (r521)
  | 841 -> One (r523)
  | 826 -> One (r524)
  | 829 -> One (r526)
  | 840 -> One (r528)
  | 839 -> One (r529)
  | 639 -> One (r530)
  | 641 -> One (r531)
  | 642 -> One (r533)
  | 644 -> One (r534)
  | 646 -> One (r535)
  | 648 -> One (r536)
  | 781 | 835 -> One (r537)
  | 780 | 834 -> One (r538)
  | 779 | 833 -> One (r539)
  | 649 | 773 -> One (r540)
  | 776 | 832 -> One (r541)
  | 775 | 831 -> One (r542)
  | 650 | 774 -> One (r543)
  | 830 -> One (r544)
  | 657 -> One (r545)
  | 760 | 802 -> One (r546)
  | 759 | 801 -> One (r547)
  | 758 | 800 -> One (r548)
  | 658 | 789 -> One (r549)
  | 661 | 792 -> One (r550)
  | 660 | 791 -> One (r551)
  | 659 | 790 -> One (r552)
  | 666 -> One (r553)
  | 665 -> One (r554)
  | 670 -> One (r555)
  | 669 -> One (r556)
  | 722 -> One (r557)
  | 673 -> One (r558)
  | 676 -> One (r559)
  | 678 -> One (r560)
  | 680 -> One (r561)
  | 685 -> One (r562)
  | 687 -> One (r563)
  | 689 -> One (r564)
  | 691 -> One (r565)
  | 693 -> One (r566)
  | 695 -> One (r567)
  | 697 -> One (r568)
  | 699 -> One (r569)
  | 701 -> One (r570)
  | 703 -> One (r571)
  | 705 -> One (r572)
  | 707 -> One (r573)
  | 709 -> One (r574)
  | 711 -> One (r575)
  | 713 -> One (r576)
  | 715 -> One (r577)
  | 717 -> One (r578)
  | 719 -> One (r579)
  | 721 -> One (r580)
  | 724 -> One (r581)
  | 726 -> One (r582)
  | 731 -> One (r583)
  | 730 -> One (r584)
  | 752 -> One (r585)
  | 751 -> One (r586)
  | 734 -> One (r587)
  | 737 -> One (r588)
  | 742 -> One (r589)
  | 744 -> One (r590)
  | 746 -> One (r591)
  | 748 -> One (r592)
  | 757 | 797 -> One (r593)
  | 756 | 796 -> One (r594)
  | 755 | 795 -> One (r595)
  | 768 | 814 -> One (r596)
  | 767 | 813 -> One (r597)
  | 766 | 812 -> One (r598)
  | 762 | 806 -> One (r599)
  | 765 | 809 -> One (r600)
  | 764 | 808 -> One (r601)
  | 763 | 807 -> One (r602)
  | 771 | 819 -> One (r603)
  | 770 | 818 -> One (r604)
  | 769 | 817 -> One (r605)
  | 778 -> One (r606)
  | 783 -> One (r607)
  | 786 | 838 -> One (r608)
  | 785 | 837 -> One (r609)
  | 784 | 836 -> One (r610)
  | 788 -> One (r611)
  | 794 -> One (r612)
  | 799 -> One (r613)
  | 804 -> One (r614)
  | 811 -> One (r615)
  | 816 -> One (r616)
  | 821 -> One (r617)
  | 824 -> One (r618)
  | 843 -> One (r619)
  | 845 -> One (r620)
  | 850 -> One (r621)
  | 849 -> One (r622)
  | 853 -> One (r623)
  | 856 -> One (r624)
  | 855 -> One (r625)
  | 863 -> One (r628)
  | 860 -> One (r629)
  | 859 -> One (r631)
  | 858 -> One (r632)
  | 883 -> One (r633)
  | 882 -> One (r634)
  | 867 -> One (r635)
  | 866 -> One (r636)
  | 873 -> One (r637)
  | 875 -> One (r638)
  | 874 | 1046 -> One (r639)
  | 877 -> One (r640)
  | 881 -> One (r641)
  | 889 -> One (r642)
  | 894 -> One (r643)
  | 893 -> One (r644)
  | 897 -> One (r645)
  | 896 -> One (r646)
  | 899 -> One (r647)
  | 905 -> One (r648)
  | 904 -> One (r649)
  | 909 -> One (r650)
  | 908 -> One (r651)
  | 931 -> One (r652)
  | 930 -> One (r653)
  | 929 -> One (r654)
  | 928 -> One (r655)
  | 927 -> One (r656)
  | 926 -> One (r657)
  | 946 -> One (r658)
  | 936 -> One (r659)
  | 935 -> One (r660)
  | 938 -> One (r661)
  | 942 -> One (r662)
  | 941 -> One (r663)
  | 940 -> One (r664)
  | 945 -> One (r665)
  | 952 -> One (r666)
  | 951 -> One (r667)
  | 964 -> One (r669)
  | 970 -> One (r670)
  | 976 -> One (r671)
  | 975 -> One (r672)
  | 986 -> One (r673)
  | 993 -> One (r674)
  | 992 -> One (r675)
  | 1002 -> One (r676)
  | 1001 -> One (r677)
  | 1000 -> One (r678)
  | 999 -> One (r679)
  | 998 -> One (r680)
  | 1008 -> One (r681)
  | 1007 -> One (r682)
  | 1006 -> One (r683)
  | 1005 -> One (r684)
  | 1004 -> One (r685)
  | 1018 -> One (r686)
  | 1017 -> One (r687)
  | 1016 -> One (r688)
  | 1011 -> One (r689)
  | 1010 -> One (r690)
  | 1015 -> One (r691)
  | 1014 -> One (r692)
  | 1013 -> One (r693)
  | 1020 -> One (r694)
  | 1028 -> One (r695)
  | 1027 -> One (r696)
  | 1026 -> One (r697)
  | 1025 -> One (r699)
  | 1024 -> One (r700)
  | 1023 -> One (r701)
  | 1022 -> One (r702)
  | 1042 -> One (r703)
  | 1045 -> One (r705)
  | 1036 -> One (r706)
  | 1035 -> One (r707)
  | 1034 -> One (r708)
  | 1041 -> One (r709)
  | 1040 -> One (r710)
  | 1044 -> One (r711)
  | 1052 -> One (r712)
  | 1051 -> One (r713)
  | 1050 -> One (r714)
  | 1049 -> One (r715)
  | 1048 -> One (r716)
  | 1057 -> One (r717)
  | 1056 -> One (r718)
  | 1055 -> One (r719)
  | 1054 -> One (r720)
  | 1060 -> One (r721)
  | 1059 -> One (r722)
  | 1076 -> One (r723)
  | 1075 -> One (r724)
  | 1079 -> One (r725)
  | 1078 -> One (r726)
  | 1082 -> One (r727)
  | 1081 -> One (r728)
  | 1093 | 1513 -> One (r729)
  | 1092 | 1512 -> One (r730)
  | 1091 | 1511 -> One (r731)
  | 1090 | 1510 -> One (r732)
  | 1097 | 1517 -> One (r733)
  | 1096 | 1516 -> One (r734)
  | 1095 | 1515 -> One (r735)
  | 1094 | 1514 -> One (r736)
  | 1561 -> One (r737)
  | 1560 -> One (r738)
  | 1099 -> One (r739)
  | 1101 -> One (r740)
  | 1103 -> One (r741)
  | 1559 -> One (r742)
  | 1558 -> One (r743)
  | 1105 -> One (r744)
  | 1109 -> One (r745)
  | 1108 -> One (r746)
  | 1107 -> One (r747)
  | 1116 -> One (r748)
  | 1119 -> One (r750)
  | 1118 -> One (r751)
  | 1115 -> One (r752)
  | 1114 -> One (r753)
  | 1113 -> One (r754)
  | 1112 -> One (r755)
  | 1111 -> One (r756)
  | 1126 -> One (r757)
  | 1125 -> One (r758)
  | 1124 -> One (r759)
  | 1123 -> One (r760)
  | 1129 -> One (r763)
  | 1128 -> One (r764)
  | 1127 -> One (r765)
  | 1161 -> One (r766)
  | 1160 -> One (r767)
  | 1159 -> One (r768)
  | 1380 -> One (r769)
  | 1379 -> One (r770)
  | 1154 -> One (r771)
  | 1153 -> One (r772)
  | 1152 -> One (r773)
  | 1151 -> One (r774)
  | 1150 -> One (r775)
  | 1133 -> One (r776)
  | 1141 -> One (r777)
  | 1140 -> One (r778)
  | 1149 -> One (r780)
  | 1148 -> One (r781)
  | 1144 -> One (r782)
  | 1143 -> One (r783)
  | 1142 -> One (r784)
  | 1139 -> One (r785)
  | 1138 -> One (r786)
  | 1137 -> One (r787)
  | 1147 -> One (r788)
  | 1146 -> One (r789)
  | 1158 -> One (r790)
  | 1157 -> One (r791)
  | 1156 -> One (r792)
  | 1223 -> One (r793)
  | 1241 -> One (r795)
  | 1258 -> One (r797)
  | 1257 -> One (r798)
  | 1179 -> One (r799)
  | 1178 -> One (r800)
  | 1177 -> One (r801)
  | 1166 -> One (r802)
  | 1164 -> One (r803)
  | 1163 -> One (r804)
  | 1170 -> One (r805)
  | 1169 -> One (r806)
  | 1171 -> One (r808)
  | 1168 -> One (r809)
  | 1176 -> One (r811)
  | 1173 -> One (r813)
  | 1172 -> One (r814)
  | 1175 -> One (r815)
  | 1188 -> One (r816)
  | 1187 -> One (r817)
  | 1186 -> One (r819)
  | 1185 -> One (r820)
  | 1181 -> One (r821)
  | 1184 -> One (r822)
  | 1183 -> One (r823)
  | 1205 -> One (r824)
  | 1204 -> One (r825)
  | 1203 -> One (r826)
  | 1202 -> One (r828)
  | 1201 -> One (r829)
  | 1190 -> One (r830)
  | 1195 -> One (r831)
  | 1194 -> One (r832)
  | 1193 -> One (r833)
  | 1192 -> One (r834)
  | 1200 -> One (r835)
  | 1199 -> One (r836)
  | 1198 -> One (r837)
  | 1197 -> One (r838)
  | 1220 -> One (r839)
  | 1219 -> One (r841)
  | 1218 -> One (r842)
  | 1214 -> One (r843)
  | 1213 -> One (r844)
  | 1212 -> One (r845)
  | 1207 -> One (r846)
  | 1217 -> One (r847)
  | 1216 -> One (r848)
  | 1245 -> One (r849)
  | 1244 -> One (r850)
  | 1222 -> One (r851)
  | 1242 -> One (r852)
  | 1229 -> One (r853)
  | 1228 -> One (r854)
  | 1227 -> One (r855)
  | 1226 -> One (r856)
  | 1225 -> One (r857)
  | 1239 -> One (r858)
  | 1236 -> One (r860)
  | 1232 -> One (r861)
  | 1231 -> One (r862)
  | 1235 -> One (r863)
  | 1234 -> One (r864)
  | 1238 -> One (r865)
  | 1250 -> One (r866)
  | 1249 -> One (r867)
  | 1248 -> One (r868)
  | 1252 -> One (r870)
  | 1251 -> One (r871)
  | 1247 -> One (r872)
  | 1254 -> One (r873)
  | 1292 -> One (r874)
  | 1297 -> One (r876)
  | 1296 -> One (r877)
  | 1263 -> One (r878)
  | 1262 -> One (r879)
  | 1261 -> One (r880)
  | 1260 -> One (r881)
  | 1295 -> One (r882)
  | 1268 -> One (r883)
  | 1267 -> One (r884)
  | 1266 -> One (r885)
  | 1265 -> One (r886)
  | 1294 -> One (r887)
  | 1271 -> One (r888)
  | 1270 -> One (r889)
  | 1293 -> One (r890)
  | 1275 -> One (r891)
  | 1274 -> One (r892)
  | 1278 -> One (r893)
  | 1283 -> One (r894)
  | 1282 -> One (r895)
  | 1281 -> One (r896)
  | 1291 -> One (r897)
  | 1286 -> One (r898)
  | 1332 -> One (r899)
  | 1331 -> One (r900)
  | 1330 -> One (r901)
  | 1303 -> One (r902)
  | 1302 -> One (r903)
  | 1301 -> One (r904)
  | 1305 -> One (r905)
  | 1328 -> One (r906)
  | 1327 -> One (r907)
  | 1326 -> One (r908)
  | 1325 -> One (r909)
  | 1324 -> One (r911)
  | 1322 -> One (r913)
  | 1321 -> One (r914)
  | 1307 -> One (r915)
  | 1315 -> One (r916)
  | 1310 -> One (r917)
  | 1309 -> One (r918)
  | 1314 -> One (r919)
  | 1313 -> One (r920)
  | 1312 -> One (r921)
  | 1320 -> One (r922)
  | 1319 -> One (r923)
  | 1318 -> One (r924)
  | 1317 -> One (r925)
  | 1336 -> One (r926)
  | 1346 -> One (r927)
  | 1345 -> One (r928)
  | 1344 -> One (r929)
  | 1343 -> One (r930)
  | 1342 -> One (r931)
  | 1355 -> One (r932)
  | 1365 -> One (r933)
  | 1364 -> One (r934)
  | 1363 -> One (r935)
  | 1362 -> One (r936)
  | 1361 -> One (r937)
  | 1360 -> One (r938)
  | 1359 -> One (r939)
  | 1376 -> One (r940)
  | 1375 -> One (r941)
  | 1374 -> One (r942)
  | 1373 -> One (r943)
  | 1372 -> One (r944)
  | 1371 -> One (r945)
  | 1370 -> One (r946)
  | 1471 -> One (r947)
  | 1469 -> One (r949)
  | 1496 -> One (r951)
  | 1387 -> One (r952)
  | 1504 -> One (r954)
  | 1503 -> One (r955)
  | 1386 -> One (r956)
  | 1385 -> One (r957)
  | 1384 -> One (r958)
  | 1391 -> One (r959)
  | 1390 -> One (r960)
  | 1389 -> One (r961)
  | 1412 -> One (r962)
  | 1411 -> One (r963)
  | 1410 -> One (r964)
  | 1409 -> One (r965)
  | 1398 -> One (r966)
  | 1397 -> One (r967)
  | 1396 -> One (r969)
  | 1395 -> One (r970)
  | 1403 -> One (r971)
  | 1402 -> One (r972)
  | 1401 -> One (r973)
  | 1400 -> One (r974)
  | 1408 -> One (r975)
  | 1407 -> One (r976)
  | 1406 -> One (r977)
  | 1415 -> One (r978)
  | 1414 -> One (r979)
  | 1441 -> One (r980)
  | 1430 -> One (r981)
  | 1429 -> One (r982)
  | 1418 -> One (r983)
  | 1417 -> One (r984)
  | 1443 -> One (r986)
  | 1442 -> One (r987)
  | 1423 -> One (r988)
  | 1422 -> One (r989)
  | 1421 -> One (r990)
  | 1420 -> One (r991)
  | 1428 -> One (r992)
  | 1427 -> One (r993)
  | 1426 -> One (r994)
  | 1440 -> One (r995)
  | 1439 -> One (r996)
  | 1438 -> One (r997)
  | 1437 -> One (r998)
  | 1436 -> One (r999)
  | 1435 -> One (r1000)
  | 1434 -> One (r1001)
  | 1433 -> One (r1002)
  | 1447 -> One (r1003)
  | 1446 -> One (r1004)
  | 1445 -> One (r1005)
  | 1487 -> One (r1006)
  | 1486 -> One (r1007)
  | 1483 -> One (r1008)
  | 1450 -> One (r1009)
  | 1449 -> One (r1010)
  | 1479 -> One (r1011)
  | 1478 -> One (r1012)
  | 1477 -> One (r1013)
  | 1457 -> One (r1014)
  | 1456 -> One (r1015)
  | 1455 -> One (r1016)
  | 1454 -> One (r1017)
  | 1453 -> One (r1018)
  | 1461 -> One (r1019)
  | 1460 -> One (r1020)
  | 1459 -> One (r1021)
  | 1474 -> One (r1022)
  | 1465 -> One (r1023)
  | 1464 -> One (r1024)
  | 1476 -> One (r1026)
  | 1463 -> One (r1027)
  | 1472 -> One (r1028)
  | 1467 -> One (r1029)
  | 1482 -> One (r1030)
  | 1481 -> One (r1031)
  | 1485 -> One (r1032)
  | 1491 -> One (r1033)
  | 1490 -> One (r1034)
  | 1489 -> One (r1035)
  | 1493 -> One (r1036)
  | 1500 -> One (r1037)
  | 1499 -> One (r1038)
  | 1498 -> One (r1039)
  | 1502 -> One (r1040)
  | 1551 -> One (r1041)
  | 1518 -> One (r1042)
  | 1528 -> One (r1043)
  | 1527 -> One (r1044)
  | 1526 -> One (r1045)
  | 1525 -> One (r1046)
  | 1538 -> One (r1047)
  | 1548 -> One (r1048)
  | 1547 -> One (r1049)
  | 1546 -> One (r1050)
  | 1545 -> One (r1051)
  | 1544 -> One (r1052)
  | 1543 -> One (r1053)
  | 1554 -> One (r1054)
  | 1553 -> One (r1055)
  | 1573 -> One (r1056)
  | 1572 -> One (r1057)
  | 1571 -> One (r1058)
  | 1587 -> One (r1059)
  | 1586 -> One (r1060)
  | 1594 -> One (r1061)
  | 1597 -> One (r1062)
  | 1596 -> One (r1063)
  | 1600 -> One (r1064)
  | 1611 -> One (r1066)
  | 1609 -> One (r1068)
  | 1608 -> One (r1069)
  | 1621 -> One (r1070)
  | 1620 -> One (r1071)
  | 1619 -> One (r1072)
  | 1618 -> One (r1073)
  | 1617 -> One (r1074)
  | 1623 -> One (r1075)
  | 1633 -> One (r1076)
  | 1635 -> One (r1077)
  | 1641 -> One (r1078)
  | 1647 -> One (r1079)
  | 1646 -> One (r1080)
  | 1645 -> One (r1081)
  | 1644 -> One (r1082)
  | 1655 -> One (r1083)
  | 1650 -> One (r1084)
  | 1654 -> One (r1085)
  | 1653 -> One (r1086)
  | 1652 -> One (r1087)
  | 1657 -> One (r1088)
  | 1659 -> One (r1089)
  | 1671 -> One (r1090)
  | 1669 -> One (r1092)
  | 1668 -> One (r1093)
  | 1665 -> One (r1094)
  | 1664 -> One (r1095)
  | 1687 -> One (r1096)
  | 1686 -> One (r1097)
  | 1695 -> One (r1098)
  | 1694 -> One (r1099)
  | 1693 -> One (r1100)
  | 1698 -> One (r1101)
  | 1697 -> One (r1102)
  | 1702 -> One (r1103)
  | 1704 -> One (r1104)
  | 1712 -> One (r1105)
  | 1711 -> One (r1106)
  | 1714 -> One (r1107)
  | 1716 -> One (r1108)
  | 1733 -> One (r1109)
  | 1732 -> One (r1110)
  | 1731 -> One (r1111)
  | 1730 -> One (r1112)
  | 1735 -> One (r1113)
  | 1739 -> One (r1114)
  | 1743 -> One (r1115)
  | 595 -> Select (function
    | -1 -> [R 102]
    | _ -> r506)
  | 355 -> Select (function
    | -1 -> S (T T_TYPE) :: r283
    | _ -> R 208 :: r278)
  | 1120 -> Select (function
    | -1 -> r768
    | _ -> R 208 :: r762)
  | 420 -> Select (function
    | -1 -> S (T T_UIDENT) :: r321
    | _ -> r278)
  | 83 -> Select (function
    | -1 -> S (T T_RPAREN) :: r44
    | _ -> S (T T_DOT) :: r46)
  | 528 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r253
    | _ -> Sub (r415) :: r418)
  | 239 -> Select (function
    | 436 | 543 | 734 -> r73
    | _ -> S (N N_pattern) :: r180)
  | 288 -> Select (function
    | 1046 -> r83
    | _ -> Sub (r81) :: r248)
  | 267 -> Select (function
    | 331 -> r210
    | _ -> Sub (r194) :: r216)
  | 289 -> Select (function
    | 1046 -> r82
    | _ -> r248)
  | 268 -> Select (function
    | 259 -> r216
    | _ -> r209)
  | 1122 -> Select (function
    | -1 -> r766
    | _ -> r761)
  | 1121 -> Select (function
    | -1 -> r767
    | _ -> r762)
  | _ -> raise Not_found
