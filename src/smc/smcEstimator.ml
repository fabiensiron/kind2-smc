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

type t = {
  mutable samples : int ;
  mutable violations : int ;
}

type result = {
  samples : int ;
  violations : int ;
  probability : float ;
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

let precision = function
  | Fixed { precision ; _ }
  | Apmc { precision ; _ } -> precision

let confidence = function
  | Fixed { runs ; precision } -> hoeffding_confidence ~runs ~precision
  | Apmc { confidence ; _ } -> confidence

let finished config (estimator: t) =
  estimator.samples >= runs config

let result (estimator: t) =
  {
    samples = estimator.samples ;
    violations = estimator.violations ;
    probability =
      if estimator.samples = 0 then 0.0
      else float_of_int estimator.violations /. float_of_int estimator.samples
  }

(* let confidence config = *)
(*   match config with *)
(*   | Fixed { runs ; precision } -> *)
(*     let n = float_of_int runs in *)
(*     let delta = 2.0 *. exp (-2.0 *. n *. precision *. precision) in *)
(*     max 0.0 (min 1.0 (1.0 -. delta)) *)
