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

(** Statistical estimators for SMC *)

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

(* estimator-specific results *)

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

(* mutable state *)
type t

type result = {
  samples : int ;
  violations : int ;
  probability : float ;
  method_result : method_result ;
}

(** Build the estimator for fixed runs *)
val make_fixed : runs:int -> precision:float -> config

(** Build the estimator for APMC *)
val make_apmc : precision:float -> confidence:float -> config

(** Build the estimator for SPRT property testing *)
val make_sprt :
  max_runs:int -> threshold:float -> delta:float -> alpha:float ->
  beta:float -> config

(** Build the estimator state (per property) *)
val create : unit -> t

(** Add one accepted execution *)
val observe : config -> t -> violation:bool -> unit

(** Checks whether the estimator received enough observations. *)
val finished : config -> t -> bool

(** Return the final estimat. *)
val result : config -> t -> result

(** Compute how many runs are necessary. *)
val runs : config -> int

(** Build the confidence using Chernoff-Hoeffding lower bound *)
val confidence : config -> float

(** Build the confidence using Chernoff-Hoeffding lower bound *)
val precision : config -> float

(** Compute runs for APMC *)
val apmc_runs : precision:float -> confidence:float -> int

(** Current SPRT log-likelihood ratio *)
val sprt_log_likelihood_ratio : config -> t -> float

val sprt_decision : config -> t -> sprt_decision option
