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

(* mutable state *)
type t

type result = {
  samples : int ;
  violations : int ;
  probability : float ;
}

(** Build the estimator for fixed runs *)
val make_fixed : runs:int -> precision:float -> config

(** Build the estimator for APMC *)
val make_apmc : precision:float -> confidence:float -> config

(** Build the estimator state (per property) *)
val create : unit -> t

(** Add one accepted execution *)
val observe : config -> t -> violation:bool -> unit

(** Checks whether the estimator received enough observations. *)
val finished : config -> t -> bool

(** Return the final estimat. *)
val result : t -> result

(** Compute how many runs are necessary. *)
val runs : config -> int

(** Build the confidence using Chernoff-Hoeffding lower bound *)
val confidence : config -> float

(** Build the confidence using Chernoff-Hoeffding lower bound *)
val precision : config -> float

(** Compute runs for APMC *)
val apmc_runs : precision:float -> confidence:float -> int
