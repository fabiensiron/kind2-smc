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

(*./kind2 --enable interpreter --debug smt --debug parse microwave.lus*)
open Lib


(* Solver instance if created *)
let ref_solver = ref None


(* Exit and terminate all processes here in case we are interrupted *)
let on_exit _ = 

  (* Delete solver instance if created *)
  (try 
     match !ref_solver with 
       | Some solver -> 
         SMTSolver.delete_instance solver;  
         ref_solver := None
       | None -> ()
   with 
     | e -> 
       KEvent.log L_error
         "Error deleting solver_init: %s" 
         (Printexc.to_string e))



module Rand = struct
  let default_int_min = -1000
  let default_int_max = 1000
  let default_real_min = -1000.0
  let default_real_max = 1000.0

  let init () = Random.self_init () (* TODO: add a --smc_seed option *)

  let random_real min max =
    min +. Random.float (max -. min)

  let random_int min max =
    Random.int_in_range ~min ~max

  let random_bool () =
    Random.bool ()

  let random_value ty =
    match Type.node_of_type ty with
    | Type.Bool -> random_bool () |> Term.mk_bool
    | Type.Int ->
      random_int default_int_min default_int_max |> Numeral.of_int |> Term.mk_num
    | Type.IntRange (Some lb, Some ub) ->
      random_int (Numeral.to_int lb) (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
    | Type.IntRange (None, Some ub) ->
      random_int default_int_min (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
    | Type.IntRange (Some lb, None) ->
      random_int (Numeral.to_int lb) default_int_max |> Numeral.of_int |> Term.mk_num
    | Type.Enum (lb, ub) ->
      random_int (Numeral.to_int lb) (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
    | Type.Real ->
      random_real default_real_min default_real_max
      |> Printf.sprintf "%.17g" |> Decimal.of_string |> Term.mk_dec
    | _ ->
      failwith
        (Format.asprintf
           "SMC: unsupported input type %a" Type.pp_print_type ty)

end

(* Assert transition relation for all steps below [i] *)
let rec assert_trans solver t i =
  (* Instant zero is base instant *)
  if Numeral.(i < one) then () else 
    begin
      (* Assert transition relation from [i-1] to [i] *)
      SMTSolver.assert_term solver
        (TransSys.trans_of_bound (Some (SMTSolver.declare_fun solver)) t i);
      (* Continue with for [i-2] and [i-1] *)
      assert_trans solver t Numeral.(i - one)
    end

let assert_input solver state_var instant value =
  let var =
    Var.mk_state_var_instance
      state_var
      (Numeral.of_int instant)
    |> Term.mk_var
  in

  Term.mk_eq [var; value] |> SMTSolver.assert_term solver

let assert_random_inputs solver inputs steps =
  List.iter
    (fun state_var ->
       if StateVar.is_const state_var then
         let value = state_var |> StateVar.type_of_state_var |> Rand.random_value in
         for instant = 0 to steps - 1 do
           assert_input solver state_var instant value
         done
       else
         for instant = 0 to steps - 1 do
           let value = state_var |> StateVar.type_of_state_var |> Rand.random_value in
           assert_input solver state_var instant value
         done
    )
    inputs

let invariant_properties trans_sys =
  TransSys.props_list_of_bound_no_skip
    trans_sys
    Numeral.zero
  |> List.filter
    (fun (name, _) ->
       match TransSys.get_prop_kind trans_sys name with
       | Property.Invariant -> true
       | _ -> false)

let evaluate_properties trans_sys model steps =
  let properties = invariant_properties trans_sys in
  let eval term = Eval.eval_term
      (TransSys.uf_defs trans_sys)
      model
      term
    |> Eval.bool_of_value
  in
  List.map
    (fun (name, property) ->
       let violated_at = ref None in

       for instant = 0 to steps - 1 do
         match !violated_at with
         | Some _ -> ()
         | None ->
           let property_at_instant =
             Term.bump_state (Numeral.of_int instant) property in
           if not @@ eval property_at_instant then
             violated_at := Some instant
       done ;
       (name, !violated_at))
  properties

let print_property_results results =
  List.iter
    (fun (name, violated_at) ->
       match violated_at with
       | None ->
         KEvent.log L_info
           "SMC property %s: satisfied on this trace" name
       | Some instant ->
         KEvent.log L_info
           "SMC property %s: violated at k=%d" name instant)
    results

(* Main entry point *)
let main  (* input_file *) input_sys _ trans_sys =

  KEvent.set_module `SMC;

  Rand.init ();

  let trans_svars = TransSys.state_vars trans_sys in

  let inputs = List.filter StateVar.is_input trans_svars in

  let steps = Flags.SMC.steps () in

  if steps <= 0 then
    invalid_arg "SMC: number of steps must be strictly positive";

  KEvent.log L_info "SMC running up to k=%d" steps;

  (* Determine logic for the SMT solver *)
  let logic = TransSys.get_logic trans_sys in

  (* Create solver instance *)
  let solver = 
    Flags.Smt.solver ()
    |> SMTSolver.create_instance ~produce_models:true logic
  in

  (* Create a reference for the solver. Only used in on_exit. *)
  ref_solver := Some solver;

  (* Defining uf's and declaring variables. *)
  TransSys.define_and_declare_of_bounds
    trans_sys
    (SMTSolver.define_fun solver)
    (SMTSolver.declare_fun solver)
    (SMTSolver.declare_sort solver)
    Numeral.(~- one) Numeral.(of_int steps) ;

  TransSys.assert_global_constraints trans_sys (SMTSolver.assert_term solver) ;

  (* Assert initial state constraint *)
    SMTSolver.assert_term solver
      (TransSys.init_of_bound (Some (SMTSolver.declare_fun solver))
         trans_sys Numeral.zero);

  (* Assert transition relation up to number of steps *)
  assert_trans solver trans_sys (Numeral.of_int steps);

  (* Assert random inputs *)
  assert_random_inputs solver inputs steps;

  if SMTSolver.check_sat solver then
    begin
      KEvent.log L_info "SMC: sampled trace accepted";

      let model = SMTSolver.get_var_values
          solver
          (TransSys.get_state_var_bounds trans_sys)
          (TransSys.vars_of_bounds trans_sys
             Numeral.zero (Numeral.of_int steps)) in

      let property_results = evaluate_properties trans_sys model steps in

      print_property_results property_results;

      (* Extract execution path from model *)
      let path = 
        Model.path_from_model 
          (TransSys.state_vars trans_sys)
          model
          Numeral.(pred (of_int steps))
      in

      (* Output execution path *)
      KEvent.execution_path
        ~full_contract:false (* contract_monitor *)
        input_sys
        trans_sys 
        (Model.path_to_list path);
    end
  else
    begin
      KEvent.log L_info "SMC: sampled trace rejected (infeasible)"
    end


(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
