(* This file is part of the Kind 2 model checker.

   Copyright (c) 2015 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)


module LA = LustreAst
module HMap = HString.HStringMap

let hstring_equal x y =
  HString.compare x y = 0

(* Collect non-parameterized type aliases from the original AST. *)
let type_aliases ast =
  List.fold_left
    (fun aliases -> function
       | LA.TypeDecl
           (_, LA.AliasType (_, name, [], ty)) ->
         HMap.add name ty aliases
       | _ ->
         aliases)
    HMap.empty
    ast


(* Expand aliases such as:

     type Digit = subrange [0,9] of int;
*)
let rec expand_type_alias aliases = function
  | LA.UserType (_, [], name) as ty ->
    begin
      match HMap.find_opt name aliases with
      | Some ty ->
        expand_type_alias aliases ty

      | None ->
        ty
    end
  | ty -> ty

let int_of_ast_expr = function
  | LA.Const (_, LA.Num n) ->
    int_of_string_opt
      (HString.string_of_hstring n)
  | LA.UnaryOp
      (_, LA.Uminus,
       LA.Const (_, LA.Num n)) ->
    Option.map
      (fun n -> -n)
      (int_of_string_opt
         (HString.string_of_hstring n))
  | _ -> None

let int_range_of_type aliases ty =
  let ty =
    expand_type_alias aliases ty
  in

  match ty with
  (* [lower, upper] *)
  | LA.RefinementType
      (_, (_, id, LA.Int _),
       LA.BinaryOp
         (_, LA.And,
          LA.CompOp
            (_, LA.Lte,
             lower,
             LA.Ident (_, id1)),
          LA.CompOp
            (_, LA.Lte,
             LA.Ident (_, id2),
             upper)))
    when hstring_equal id id1
      && hstring_equal id id2 ->
    begin
      match int_of_ast_expr lower, int_of_ast_expr upper with
      | Some lower, Some upper -> Some (Some lower, Some upper)
      | _ -> None
    end

  (* [lower, *] *)
  | LA.RefinementType
      (_, (_, id, LA.Int _),
       LA.CompOp
         (_, LA.Lte,
          lower,
          LA.Ident (_, id1)))
    when hstring_equal id id1 ->
    begin
      match int_of_ast_expr lower with
      | Some lower -> Some (Some lower, None)
      | None -> None
    end

  (* [*, upper] *)
  | LA.RefinementType
      (_, (_, id, LA.Int _),
       LA.CompOp
         (_, LA.Lte,
          LA.Ident (_, id1),
          upper))
    when hstring_equal id id1 ->
    begin
      match int_of_ast_expr upper with
      | Some upper -> Some (None, Some upper)
      | None -> None
    end
  | _ -> None

let source_inputs ast main_name =
  List.find_map
    (function
      | LA.NodeDecl
          (_, (node_id,
               _is_imported,
               _opacity,
               _params,
               inputs,
               _outputs,
               _locals,
               _items,
               _contract))
        when
          hstring_equal
            (NodeId.get_user_name node_id)
            main_name ->
        Some inputs
      | _ -> None)

    ast

let input_ranges input_sys trans_sys =
  let ast =
    InputSystem.lustre_source_ast input_sys
  in
  let aliases =
    type_aliases ast
  in
  let main_name =
    TransSys.scope_of_trans_sys trans_sys
    |> InputSystem.get_node_id input_sys
    |> NodeId.get_user_name
  in
  match source_inputs ast main_name with
  | None -> HMap.empty
  | Some inputs ->
    List.fold_left
      (fun ranges (_, name, ty, _, _) ->
         match int_range_of_type aliases ty with
         | Some range -> HMap.add name range ranges
         | None -> ranges)
      HMap.empty inputs
