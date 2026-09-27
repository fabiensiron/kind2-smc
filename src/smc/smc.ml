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
open Actlit

module Rand = SmcRand

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

(* Statistics environment and display *)
module Statistics = struct
  module V = Map.Make(String)
  type t =
    {
      mutable generated : int;
      mutable accepted : int;
      mutable rejected : int;
      mutable violations : int V.t;
    }

  let init props =
    let s =
      {
        generated = 0;
        accepted = 0;
        rejected = 0;
        violations = V.empty;
      }
    in
    List.fold_left (fun s (name, _, _) ->
        { s with violations = V.add name 0 s.violations }
      )
      s props

  let add_violations s vs =
    let vs =
      List.fold_left (fun vs (name, _) ->
          let update v = Some (succ @@ try Option.get v with _ -> 0) in
          V.update name update vs
        ) s.violations vs in
    s.violations <- vs

  let pp fmt s =
    Format.fprintf fmt "@[<v>- SMC samples: generated=%d accepted=%d rejected=%d"
      s.generated s.accepted s.rejected;
    if s.accepted > 0 then
      Format.fprintf fmt
        "@,%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt (name, cnt) ->
              let probability = float_of_int cnt /. float_of_int s.accepted in
              Format.fprintf fmt "- SMC property %s: violations=%d/%d, p~=%g"
                name cnt s.accepted probability))
        (V.to_list s.violations);
    Format.fprintf fmt "@]"
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

let build_input_equation state_var instant value =
  let var =
    Var.mk_state_var_instance
      state_var
      (Numeral.of_int instant)
    |> Term.mk_var
  in

  Term.mk_eq [var; value]

let build_random_input_equations inputs steps =
  let equations = ref [] in
  List.iter
    (fun state_var ->
       if StateVar.is_const state_var then
         let value = state_var |> StateVar.type_of_state_var |> Rand.random_value in
         for instant = 0 to steps - 1 do
           equations := build_input_equation state_var instant value :: !equations
         done
       else
         for instant = 0 to steps - 1 do
           let value = state_var |> StateVar.type_of_state_var |> Rand.random_value in
           equations := build_input_equation state_var instant value :: !equations
         done
    ) inputs;
  !equations

let invariant_properties trans_sys =
  TransSys.props_list_of_bound_no_skip
    trans_sys
    Numeral.zero
  |> List.filter
    (fun (name, _) ->
       match TransSys.get_prop_kind trans_sys name with
       | Property.Invariant -> true
       | _ -> false)

let build_property_terms trans_sys steps =
  invariant_properties trans_sys
  |> List.fold_left
    (fun acc (name, prop) ->
       let rec add_instant instant acc =
         if instant >= steps then acc
         else
           let term = Term.bump_state (Numeral.of_int instant) prop in
           add_instant
             (instant + 1)
             ((name, instant, term) :: acc)
       in
       add_instant 0 acc)
    []

module S = Set.Make(String)

let violations_of_values props values =
  let pset = ref S.empty in
  List.fold_left
    (fun acc (name, instant, term) ->
       let value_opt =
         List.find_opt
           (fun (queried_term, _) ->
              Term.equal queried_term term)
           values in
       let (_, value) =
         try Option.get value_opt
         with _ ->
           failwith (Format.asprintf "SMC: solver did not return a value for property term %a"
                       Term.pp_print_term term) in
       if Term.equal value Term.t_false then
         (* TODO: handle step violations *)
         if not @@ S.mem name !pset then
           (pset := S.add name !pset; (name, instant) :: acc)
         else acc
       else if Term.equal value Term.t_true then
         acc
       else
         failwith
           (Format.asprintf
              "SMC: property %s did not evaluate to a Boolean" name)
    )
    []
    props

type run_result =
  | Accepted of (string * int) list
  | Rejected

let run_one solver inputs steps properties =
  (* Build random input equations *)
  let input_equations = build_random_input_equations inputs steps in

  (* Build and assert input term using activation litterals *)
  let actlit_uf = fresh_actlit () in
  SMTSolver.declare_fun solver actlit_uf;
  let actlit = term_of_actlit actlit_uf in
  Term.mk_implies [actlit; Term.mk_and input_equations]
  |> SMTSolver.assert_term solver;

  (* Solver continuations *)
  let if_sat _solver values =
    let violations =
      violations_of_values
        properties
        values in
    Accepted violations
  in
  let if_unsat _solver = Rejected in
  let result =
    if properties = [] then
      SMTSolver.check_sat_assuming
        solver
        (fun _solver -> Accepted [])
        if_unsat
        [actlit]
    else
      SMTSolver.check_sat_assuming_and_get_term_values
        solver
        if_sat
        if_unsat
        [actlit]
        (List.map (fun (_, _, term) -> term) properties) in

  (* Deactivate input trace using action litteral *)
  Term.mk_not actlit |> SMTSolver.assert_term solver;
  result


(* Main entry point *)
let main  (* input_file *) input_sys _ trans_sys =

  KEvent.set_module `SMC;

  Rand.init ();

  let trans_svars = TransSys.state_vars trans_sys in

  let inputs = List.filter StateVar.is_input trans_svars in

  let steps = Flags.SMC.steps () in

  if steps <= 0 then
    invalid_arg "SMC: number of steps must be strictly positive";

  let runs = Flags.SMC.runs () in

  if runs <= 0 then
    invalid_arg "SMC: number of runs must be strictly positive";

  KEvent.log L_info "SMC: %d runs of %d steps" runs steps;

  let properties = build_property_terms trans_sys steps in
  let stats = Statistics.init properties in

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

  let run = ref 0 in
  while stats.accepted < runs do
    run := !run + 1;
    stats.generated <- stats.generated + 1;

    if !run mod 1000 = 0 then
      KEvent.progress stats.accepted;

    match run_one solver inputs steps properties with
    | Rejected ->
      (* Trace is rejected because of transition system constraints. *)
      stats.rejected <- stats.rejected + 1
    | Accepted [] ->
      (* Trace is accepted without any violation. *)
      stats.accepted <- stats.accepted + 1
    | Accepted violations ->
      (* Trace is accepted without having one or multiple property violation detected. *)
      stats.accepted <- stats.accepted + 1;
      Statistics.add_violations stats violations
  done;
  let log = Format.asprintf "@[%a@]\n" Statistics.pp stats in
  Printf.printf "Statistical Model-Checking Result:\n\n%s" log;
(*
   KEvent.log L_warn
     "@[<v>Statistical Model-Checking Result:@,@,%a@]"
     Statistics.pp stats
*)
(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
