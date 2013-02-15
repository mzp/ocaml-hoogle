open Base

let q = 
  let rev_args = ref [] in
  Arg.parse [] (fun x -> rev_args := x::!rev_args) 
    @@ Printf.sprintf "%s <query strings>" Sys.argv.(0);
  String.concat " " @@ List.rev !rev_args

let configs = Chconfig.read "modules.txt"

let modules = HList.concat_map (fun {Chconfig.modules=m} -> m) configs

let paths = filter_map (fun {Chconfig.path=p} -> p) configs

let results, stat =
  match Searchid.Stat.get (Search.raw_search q modules) paths with
  | `Error exn, _ -> raise exn
  | `Ok res, stat -> res, stat

open Format

let info_of_value id =
  let _name =
    match id with
    | Longident.Lident x -> x 
    | Longident.Ldot (_, x) -> x
    | _ -> "z"
  in
  let _, vd =
    Env.lookup_value id !Searchid.start_env
  in
  vd.Types.val_type, vd.Types.val_loc

let info_of_type id =
  let _path, td = Env.lookup_type id !Searchid.start_env in
  td

let analyze_pkind (id, pkind) = match pkind with
  | Searchid.Pvalue -> 
      `Value (id, info_of_value id)
  | Searchid.Ptype ->
      `Type (id, info_of_type id) 
  | Searchid.Pmodule ->
      `Module id
  | Searchid.Pmodtype ->
      `ModuleType id
  | Searchid.Pclass ->
      `Class id
  | Searchid.Pcltype ->
      `ClassType id
  | Searchid.Plabel ->
      `Label id
  | Searchid.Pconstructor ->
      `Constr id

(* module O = Outcometree          --- We cannot do this! Since outcometree is mli only...
*)
open Outcometree
open Types

let format ppf = function
  | `Value (id, (type_, _loc)) ->
(*
      fprintf ppf "%a@.val %a : %a@."
        Location.print_loc loc
        Printtyp.longident id
        Printtyp.type_scheme type_
*)
      fprintf ppf "@[<2>val %a :@ %a@]"
        Printtyp.longident id
        Printtyp.type_scheme type_
  | `Type (id, td) -> 
      (* To print the path name with module names,
         we hack outcometree *)
      let o = Printtyp.tree_of_type_declaration (Ident.create "z") td Types.Trec_first in
      let o = match o with
        | Osig_type (odecl, ors) ->
            let odecl = match odecl with 
              | _name, a, b, c, d ->
                  String.concat "." (Longident.flatten id), a, b, c, d
            in
            Osig_type (odecl, ors)
        | _ -> assert false
      in
      !Oprint.out_sig_item ppf o;
      if td.type_manifest = None && td.type_kind = Type_abstract then
        fprintf ppf " (* abstract *)"
  | `Module id ->
      fprintf ppf "module %a" Printtyp.longident id
  | `ModuleType id ->
      fprintf ppf "module type %a" Printtyp.longident id
  | `Class id ->
      fprintf ppf "class %a" Printtyp.longident id
  | `ClassType id ->
      fprintf ppf "class type %a" Printtyp.longident id
  | `Label id ->
      fprintf ppf "label %a" Printtyp.longident id
  | `Constr id ->
      fprintf ppf "constr %a" Printtyp.longident id

let () = 
  Format.printf "%a@.%a@." 
    (format_list "@." format) (List.map analyze_pkind results)
    Searchid.Stat.format stat

