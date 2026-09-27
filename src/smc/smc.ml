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

open Lib

module Rand = SmcRand
module Report = SmcReport
module Estimator = SmcEstimator
module Input = SmcInput
module Range = SmcRange

module Hmap = SmcRange.HMap
module Smap = Map.Make(String)
module S = Set.Make(String)

(* -------------------------------------------------------------------------- *)
(* Solver lifecycle                                                           *)
(* -------------------------------------------------------------------------- *)

let active_solver = ref None

let on_exit _ =
  (try
     match !active_solver with
       | Some solver ->
         SMTSolver.delete_instance solver;
         active_solver := None
       | None -> ()
   with
     | e ->
       KEvent.log L_error
         "SMC: error deleting solver: %s"
         (Printexc.to_string e))


let create_solver trans_sys =
  let logic = TransSys.get_logic trans_sys in
  Flags.Smt.solver ()
  |> SMTSolver.create_instance ~produce_models:false logic


(* Assert transition relations from instant [1] through [last_instant] *)
let rec assert_trans solver trans_sys last_instant =
  (* Instant zero is base instant *)
  if Numeral.(last_instant < one) then () else
    begin
      (* Assert transition relation from [last_instant-1] to [last_instant] *)
      SMTSolver.assert_term
        solver
        (TransSys.trans_of_bound
           (Some (SMTSolver.declare_fun solver))
           trans_sys last_instant);
      (* Continue with [last_instant-1] *)
      assert_trans solver trans_sys Numeral.(last_instant - one)
    end


let initialize_solver solver trans_sys last_instant =
  (* Defining uf's and declaring variables. *)
  TransSys.define_and_declare_of_bounds
    trans_sys
    (SMTSolver.define_fun solver)
    (SMTSolver.declare_fun solver)
    (SMTSolver.declare_sort solver)
    Numeral.(~- one)
    Numeral.(of_int last_instant);

  (* Global constraints. *)
  TransSys.assert_global_constraints
    trans_sys
    (SMTSolver.assert_term solver);

  (* Initial-state constraint *)
  SMTSolver.assert_term
    solver
    (TransSys.init_of_bound
       (Some (SMTSolver.declare_fun solver))
       trans_sys
       Numeral.zero);

  (* Transition relation up to the requested horizon *)
  assert_trans solver trans_sys (Numeral.of_int last_instant)


let with_solver trans_sys last_instant f =
  let solver = create_solver trans_sys in

  active_solver := Some solver;

  Fun.protect
    ~finally:(fun () ->
        active_solver := None;
        try
          SMTSolver.delete_instance solver;
        with
        | e ->
          KEvent.log L_error
            "SMC: error deleting solver: %s"
            (Printexc.to_string e))
    (fun () ->
       initialize_solver solver trans_sys last_instant;
       f solver)


let with_incremental_frame solver f =
  SMTSolver.push solver;

  Fun.protect
    ~finally:
      (fun () -> SMTSolver.pop solver)
    (fun () -> f solver)


(* -------------------------------------------------------------------------- *)
(* Input Sampling                                                             *)
(* -------------------------------------------------------------------------- *)


let build_input_equation state_var instant value =
  let var =
    Var.mk_state_var_instance
      state_var
      (Numeral.of_int instant)
    |> Term.mk_var
  in

  Term.mk_eq [var; value]


let random_input_value ranges input_distributions state_var =
  let name = StateVar.name_of_state_var state_var in
  let range =
    name
    |> HString.mk_hstring
    |> (fun name -> Hmap.find_opt name ranges) in

  let distribution = Input.find_opt name input_distributions in

  Rand.random_value
    ~range
    ?spec:distribution
    @@ StateVar.type_of_state_var state_var


let build_random_input_equations ranges input_distributions inputs steps =
  let equations = ref [] in
  List.iter
    (fun state_var ->
       let random_value () =
         random_input_value ranges input_distributions state_var in
       if StateVar.is_const state_var then
         let value = random_value () in
         for instant = 0 to steps - 1 do
           equations := build_input_equation state_var instant value :: !equations
         done
       else
         for instant = 0 to steps - 1 do
           let value = random_value () in
           equations := build_input_equation state_var instant value :: !equations
         done
    ) inputs;
  !equations


(* -------------------------------------------------------------------------- *)
(* Input Distributions                                                        *)
(* -------------------------------------------------------------------------- *)


let validate_input_distributions inputs input_distributions =
  let input_map =
    List.fold_left
      (fun map state_var ->
         let name = StateVar.name_of_state_var state_var in

         Smap.add name state_var map)

      Smap.empty
      inputs
  in

  Input.iter
    (fun name distribution ->
       match Smap.find_opt name input_map with
       | None ->
         KEvent.log
           L_error
           "SMC: input specification provided for unknown input %s"
           name;
         raise (Failure "main")
       | Some state_var ->
         begin
           try
             Input.validate
               ~name
               (StateVar.type_of_state_var state_var)
               distribution

           with Invalid_argument message ->
             KEvent.log
               L_error
               "%s"
               message;
             raise (Failure "main")
         end)
    input_distributions


let load_input_distributions () =
  match Flags.SMC.input_file () with
  | None -> Input.empty
  | Some file ->
    try
      Input.of_file file

    with Invalid_argument message ->
      KEvent.log
        L_error
        "%s"
        message;
      raise (Failure "main")


let log_input_distributions input_distributions =
  Input.iter
    (fun name spec ->
       KEvent.log
         L_info "SMC input %s: %a" name Input.pp spec)
    input_distributions


(* -------------------------------------------------------------------------- *)
(* Property handling                                                          *)
(* -------------------------------------------------------------------------- *)


type property_query = {
  name : string;
  term : Term.t;
}


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
  |> List.map
    (fun (name, prop) ->

       let rec add_instant instant acc =
         if instant >= steps then
           List.rev acc
         else
           let term = Term.bump_state (Numeral.of_int instant) prop in
           add_instant (instant + 1) (term :: acc)
       in
       let terms = add_instant 0 [] in
       {
         name = name ;
         term = Term.mk_and terms
       })


let value_of_term values term =
  match
    List.find_opt
      (fun (queried_term, _) ->
         Term.equal queried_term term)
      values
  with
  | Some (_, value) -> value
  | None ->
    KEvent.log L_error "SMC: solver did not return a value for property term %a"
      Term.pp_print_term
      term;
    raise (Failure "main")


let violations_of_values props values =
  List.fold_left
    (fun violations property ->
       let value = value_of_term values property.term in

       if Term.equal value Term.t_false then
         property.name :: violations
       else if Term.equal value Term.t_true then
         violations
       else
         begin
           KEvent.log L_error
             "SMC: property %s did not evaluate to a Boolean"
             property.name;
           raise (Failure "main")
         end
    )
    []
    props


(* -------------------------------------------------------------------------- *)
(* Estimators                                                                 *)
(* -------------------------------------------------------------------------- *)


let make_estimator_config ~runs ~precision ~confidence =
  match Flags.SMC.estimator () with
  | `FIXED -> Estimator.make_fixed ~runs ~precision
  | `APMC -> Estimator.make_apmc ~precision ~confidence


let build_estimators properties =
  List.fold_left
    (fun estimators property ->
       let name = property.name in
       if Smap.mem name estimators then
         estimators
       else
         let new_estimator = Estimator.create () in
         Smap.add name new_estimator estimators)
    Smap.empty
    properties


let estimators_update config estimators violations =
  let violated =
    List.fold_left
      (fun names name -> S.add name names)
      S.empty
      violations
  in

  Smap.iter
    (fun name estimator ->
       if not @@ Estimator.finished config estimator then
         Estimator.observe config estimator ~violation:(S.mem name violated))
    estimators


let estimators_finished config estimators =
  Smap.for_all
    (fun _ estimator ->
       Estimator.finished config estimator)
    estimators


let estimators_results estimators : (string * Estimator.result) list =
  estimators
  |> Smap.bindings
  |> List.map
    (fun (name, estimator) ->
       name,
       Estimator.result estimator)


(* -------------------------------------------------------------------------- *)
(* Trace Engine                                                               *)
(* -------------------------------------------------------------------------- *)


type run_result =
  | Accepted of string list
  | Rejected


let solve_trace solver input_equations properties =
  SMTSolver.assert_term
    solver
  @@ Term.mk_and input_equations;

  (* TODO: recover timing monitoring *)
  (*  let _start = Unix.gettimeofday () in
      let _stop = Unix.gettimeofday () in *)
(*
  KEvent.log L_debug
    "SMC run %d: %.6fs"
    run
    (_stop -. _start); *)


  (* Solver continuations *)
  let if_unsat _solver =
    Rejected
  in

  let if_sat _solver values =
    let violations =
      violations_of_values
        properties
        values in
    Accepted violations
  in

  (* Solver calls *)
  if properties = [] then
    SMTSolver.check_sat_assuming
      solver
      (fun _solver -> Accepted [])
      if_unsat
      [Term.mk_true ()]
  else
    SMTSolver.check_sat_and_get_term_values
      solver
      if_sat
      if_unsat
      (List.map (fun property -> property.term) properties)


(*
 * Random traces are sampled from the base input distribution and
 * rejected if they are incompatible with the transition-system
 * constraints.
 *)
let run_one
    ~solver
    ~trans_sys
    ~last_instant
    ~input_ranges
    ~input_distributions
    ~inputs
    ~steps
    ~properties =

  (* Build random input equations *)
  let input_equations =
    build_random_input_equations
      input_ranges
      input_distributions
      inputs
      steps
  in

  match Flags.SMC.solver_mode () with
  | `INCREMENTAL ->
    begin
      match solver with
      | Some solver ->
        with_incremental_frame
          solver
          (fun solver ->
             solve_trace
               solver
               input_equations
               properties)
      | None ->
        KEvent.log
          L_error
          "SMC: internal error: incremental solver not initialized";
        raise (Failure "main")
    end
  | `ONESHOT ->
    with_solver
      trans_sys
      last_instant
      (fun solver ->
         solve_trace
           solver
           input_equations
           properties)


(* -------------------------------------------------------------------------- *)
(* Main Routines                                                              *)
(* -------------------------------------------------------------------------- *)


let run
    ~solver
    ~estimator_config
    ~estimators
    ~statistics
    ~trans_sys
    ~last_instant
    ~input_ranges
    ~input_distributions
    ~inputs
    ~steps
    ~properties =
  let generated = ref 0 in

  (* TODO: add maximum iteration credit *)
  while not @@ estimators_finished estimator_config estimators do
    incr generated;
    statistics := Report.inc_generated !statistics;

    if !generated mod 10 = 0 then
      KEvent.progress @@ Report.accepted !statistics;

    let run_status =
      run_one
        ~solver
        ~trans_sys
        ~last_instant
        ~input_ranges
        ~input_distributions
        ~inputs
        ~steps
        ~properties in

    match run_status with
    | Rejected ->
      (* The sampled trace is incompatible with the transition-system
       * constraints and therefore does not count as an estimator sample.
      *)
      statistics := Report.inc_rejected !statistics

    | Accepted [] ->
      (* The sample trace is feasible, and does not violate any property. *)
      statistics := Report.inc_accepted !statistics;

      estimators_update estimator_config estimators []

    | Accepted violations ->
      (* The sampled trace is feasibla and does violate a property. *)
      statistics := Report.inc_accepted !statistics;

      estimators_update estimator_config estimators violations
  done


(* Main entry point *)
let main  input_sys _ trans_sys =
  KEvent.set_module `SMC;

  Rand.init ();

  let trans_svars = TransSys.state_vars trans_sys in

  let inputs = List.filter StateVar.is_input trans_svars in

  (******************************* Configuration ******************************)

  let steps = Flags.SMC.steps () in

  if steps <= 0 then
    begin
      KEvent.log L_error "SMC: number of steps must be strictly positive";
      raise (Failure "main")
    end;

  let runs = Flags.SMC.runs () in

  if runs <= 0 then
    begin
      KEvent.log L_error "SMC: number of runs must be strictly positive";
      raise (Failure "main")
    end;

  KEvent.log L_info "SMC: %d runs of %d steps" runs steps;

  let precision = Flags.SMC.precision () in
  let confidence = Flags.SMC.confidence () in

  let estimator_config =
    make_estimator_config
      ~runs
      ~precision
      ~confidence
  in

  let last_instant = steps - 1 in

  (********************************** Inputs **********************************)

  let input_ranges = Range.input_ranges input_sys trans_sys in

  let input_distributions = load_input_distributions () in

  validate_input_distributions inputs input_distributions;

  log_input_distributions input_distributions;

  (************************ Properties and Estimators *************************)

  let properties = build_property_terms trans_sys steps in

  let estimators = build_estimators properties in

  let statistics = ref
      (Report.make
         ~generated:0
         ~accepted:0
         ~rejected:0)
  in

  (******************************** Execution *********************************)

  begin
    match Flags.SMC.solver_mode () with
    | `INCREMENTAL ->
      (* Build the transition system once and reuse the solver *)
      with_solver
        trans_sys
        last_instant
        (fun solver ->
           run
             ~solver:(Some solver)
             ~estimator_config
             ~estimators
             ~statistics
             ~trans_sys
             ~last_instant
             ~input_ranges
             ~input_distributions
             ~inputs
             ~steps
             ~properties)

    | `ONESHOT ->
      (* Each trace receives a fresh solver. *)
      (run
         ~solver:None
         ~estimator_config
         ~estimators
         ~statistics
         ~trans_sys
         ~last_instant
         ~input_ranges
         ~input_distributions
         ~inputs
         ~steps
         ~properties)
  end;

  (* ******************************** Report ******************************** *)


  !statistics
  |> Report.render
    ~config:estimator_config
    ~estimates:(estimators_results estimators)
  |> KEvent.result

(*
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
