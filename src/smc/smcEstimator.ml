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

type config =
  | Fixed of {
      runs : int ;
      precision : float ;
    }
  | Apmc of {
      runs : int ;
      precision : float ;
      confidence : float ;
    }
  | Sprt of {
      max_runs : int ;
      threshold : float ;
      delta : float ;
      alpha : float ;
      beta : float ;
    }

type sprt_decision =
  | Below
  | Above
  | Inconclusive

type method_result =
  | Estimate
  | SprtResult of {
      decision : sprt_decision ;
      log_likelihood_ratio : float ;
    }

type t = {
  mutable samples : int ;
  mutable violations : int ;
}

type result = {
  samples : int ;
  violations : int ;
  probability : float ;
  method_result : method_result ;
}

let check_precision precision =
  if precision <= 0.0 || precision > 1.0 then
    begin
      KEvent.log L_error
        "SMC: precision must belong to ]0, 1]";
      raise (Failure "main")
    end

let check_confidence confidence =
  if confidence <= 0.0 || confidence >= 1.0 then
    begin
      KEvent.log L_error
        "SMC: confidence must belong to ]0, 1[";
      raise (Failure "main")
    end

let check_probability name probability =
  if probability <= 0.0 || probability >= 1.0 then
    begin
      KEvent.log L_error
        "SMC: %s must belong to ]0, 1["
        name;
      raise (Failure "main")
    end

(** Give the confidence using Chernoff-Hoeffding :
    \[
    confidence \ge 1 - 2 * \exp(-2 \times n \times \epsilon^2)
    \]
*)
let hoeffding_confidence ~runs ~precision =
  let n = float_of_int runs in
  let delta = 2.0 *. exp (-2.0 *. n *. precision *. precision) in
  max 0.0 (min 1.0 (1.0 -. delta))


(** Give the number of runs required for some confidence $\gamma$ using inverse Chernoff:
    \[
    N = \ceil{\frac{\ln{\frac{2}{1 - \gamma}} }{2 \times \epsilon^2}}
    \]
*)
let apmc_runs ~precision ~confidence =
  check_precision precision;
  check_confidence confidence;

  let delta = 1.0 -. confidence in
  let runs = log (2.0 /. delta) /. (2.0 *. precision *. precision) in

  int_of_float (ceil runs)

let make_fixed ~runs ~precision =
  if runs <= 0 then
    begin
      KEvent.log L_error
        "SMC: number of runs must be strictly positive";
      raise (Failure "main")
    end;

  check_precision precision;

  Fixed {
    runs ;
    precision ;
  }

let make_apmc ~precision ~confidence =
  let runs = apmc_runs ~precision ~confidence in

  Apmc {
    runs ;
    precision ;
    confidence ;
  }

let make_sprt ~max_runs ~threshold ~delta ~alpha ~beta =

  if max_runs <= 0 then
    begin
      KEvent.log L_error
        "SMC: SPRT maximum number of runs must be strictly positive";
      raise (Failure "main")
    end;

  check_probability "SPRT threshold" threshold;
  check_probability "SPRT alpha" alpha;
  check_probability "SPRT beta" beta;

  if delta <= 0.0 then
    begin
      KEvent.log L_error
        "SMC: SPRT delta must be strictly positive";
      raise (Failure "main")
    end;

  if alpha +. beta >= 1.0 then
    begin
      KEvent.log L_error
        "SMC: SPRT requires alpha + beta < 1";
      raise (Failure "main")
    end;

  let p_low = threshold -. delta in

  let p_high = threshold +. delta in

  if p_low <= 0.0 || p_high >= 1.0 then
    begin
      KEvent.log L_error
        "SMC: SPRT indifference interval \
         [threshold - delta, threshold + delta] \
         must be contained in ]0, 1[";
      raise (Failure "main")
    end;

  Sprt {
    max_runs ;
    threshold ;
    delta ;
    alpha ;
    beta ;
  }

let sprt_parameters = function
  | Sprt { threshold ; delta ; alpha ; beta ; _ ; } ->
    let p_low = threshold -. delta in
    let p_high = threshold +. delta in

    (* We use

         L_n = log (likelihood(samples | p_high) / likelihood(samples | p_low))

       Hence:

         L_n <= lower_bound  => accept H_low and L_n >= upper_bound  => accept H_high
    *)

    let lower_bound = log (beta /. (1.0 -. alpha)) in
    let upper_bound = log ((1.0 -. beta) /. alpha) in

    p_low, p_high, lower_bound, upper_bound

  | _ ->
    invalid_arg "SmcEstimator.sprt_parameters: not an SPRT configuration"


let create () =
  {
    samples = 0 ;
    violations = 0 ;
  }

let observe _config (estimator: t) ~violation =
  estimator.samples <- estimator.samples + 1;

  if violation then
    estimator.violations <- estimator.violations + 1

let runs = function
  | Fixed { runs ; _ }
  | Apmc { runs ; _ } -> runs
  | Sprt { max_runs ; _ } -> max_runs

let precision = function
  | Fixed { precision ; _ }
  | Apmc { precision ; _ } -> precision
  | Sprt _ ->
    invalid_arg "SmcEstimator.precision: precision is not defined for SPRT"

let confidence = function
  | Fixed { runs ; precision } -> hoeffding_confidence ~runs ~precision
  | Apmc { confidence ; _ } -> confidence
  | Sprt _ ->
    invalid_arg "SmcEstimator.confidence: precision is not defined for SPRT"

let sprt_log_likelihood_ratio config (estimator : t) =
  let p_low, p_high, _lower_bound, _upper_bound = sprt_parameters config in

  let violations = float_of_int estimator.violations in
  let non_violations = float_of_int (estimator.samples - estimator.violations) in

  (* Bernoulli log-likelihood ratio:

       log L(p_high) / L(p_low)

       = v       * log(p_high / p_low)
       + (n - v) * log((1 - p_high) / (1 - p_low))
  *)
  violations *. log (p_high /. p_low) +. non_violations *. log ((1.0 -. p_high) /. (1.0 -. p_low))


let sprt_decision config estimator =
  let _p_low, _p_high, lower_bound, upper_bound = sprt_parameters config in

  let likelihood_ratio = sprt_log_likelihood_ratio config estimator in

  if likelihood_ratio <= lower_bound then
    Some Below
  else if likelihood_ratio >= upper_bound then
    Some Above
  else
    None


let finished config (estimator: t) =
  match config with
  | Fixed { runs ; _ }
  | Apmc { runs ; _ } ->
    estimator.samples >= runs
  | Sprt { max_runs ; _ } ->
    match sprt_decision config estimator with
    | Some _ -> true
    | None -> estimator.samples >= max_runs

let result config (estimator: t) =
  let method_result =
    match config with
    | Fixed _
    | Apmc _ -> Estimate
    | Sprt _ ->
      let log_likelihood_ratio = sprt_log_likelihood_ratio config estimator in
      let decision =
        match sprt_decision config estimator with
        | Some decision -> decision
        | None -> Inconclusive
      in
      SprtResult {
        decision ;
        log_likelihood_ratio ;
      }
  in
  {
    samples = estimator.samples ;
    violations = estimator.violations ;
    probability =
      if estimator.samples = 0 then 0.0
      else float_of_int estimator.violations /. float_of_int estimator.samples ;
    method_result = method_result ;
  }

(* let confidence config = *)
(*   match config with *)
(*   | Fixed { runs ; precision } -> *)
(*     let n = float_of_int runs in *)
(*     let delta = 2.0 *. exp (-2.0 *. n *. precision *. precision) in *)
(*     max 0.0 (min 1.0 (1.0 -. delta)) *)
