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

module Smap = Map.Make(String)


(* -------------------------------------------------------------------------- *)
(* Global Statistics                                                          *)
(* -------------------------------------------------------------------------- *)


type property_result = {
  name : string ;
  violations : int ;
}

type t = {
  generated : int ;
  accepted : int ;
  rejected : int ;
}

let make ~generated ~accepted ~rejected =
  {
    generated ;
    accepted ;
    rejected ;
  }

let inc_generated stats =
  {
    stats with
    generated = stats.generated + 1
  }

let inc_accepted stats =
  {
    stats with
    accepted = stats.accepted + 1
  }

let inc_rejected stats =
  {
    stats with
    rejected = stats.rejected + 1
  }

let accepted stats = stats.accepted
let rejected stats = stats.rejected
let generated stats = stats.generated


(* -------------------------------------------------------------------------- *)
(* Progress Bar                                                               *)
(* -------------------------------------------------------------------------- *)


type progress = int -> unit

let progress_line total =
  let open Progress.Line in
  list
    [
      spinner ();
      const "SMC";
      bar
        ~style:`UTF8
        total;
      count_to total;
      percentage_of total;
      brackets
        (elapsed ());
      parens
        (const "eta: " ++ eta total);
    ]


let with_progress ~config f =
  let total = SmcEstimator.runs config in
  let progress_config =
    Progress.Config.v
      ~persistent:false
      ~min_interval:
        (Some (Progress.Duration.of_ms 100.0))
      ()
  in

  Progress.with_reporter
    ~config:progress_config
    (progress_line total)
    f


let progress_accepted progress =
  progress 1


let progress_rejected progress =
  progress 0


(* -------------------------------------------------------------------------- *)
(* Pretty Printing                                                            *)
(* -------------------------------------------------------------------------- *)


let pp_probability fmt p =
  Format.fprintf fmt
    "@{<b>%.6g@}" p

let pp_violations fmt n =
  if n = 0 then
    Format.fprintf fmt "@{<green_b>0@}"
  else
    Format.fprintf fmt "@{<red_b>%d@}" n

let pp_estimator_pt fmt config =
  let runs = SmcEstimator.runs config in
  let precision = SmcEstimator.precision config in
  let confidence = SmcEstimator.confidence config in
  match config with
  | SmcEstimator.Fixed _ ->
    Format.fprintf fmt
      "@[<v>\
       @{<b>Estimator@}@,\
       @[<h>  Method     : Fixed@]@,\
       @[<h>  Runs       : %d@]@,\
       @[<h>  Precision  : ±%.6g@]@,\
       @[<h>  Confidence : @{<b>>= %.4f%%@}@]\
       @]@,"
      runs precision (100.0 *. confidence)
  | SmcEstimator.Apmc _ ->
    Format.fprintf fmt
      "@[<v>\
       @{<b>Estimator@}@,\
       @[<h>  Method         : APMC@]@,\
       @[<h>  Runs           : %d (computed)@]@,\
       @[<h>  Precision      : ±%.6g@]@,\
       @[<h>  Confidence     : @{<b>>= %.4f%%@}@]\
       @]"
      runs
      precision
      (100.0 *. confidence)

let pp_property_pt precision fmt (name, estimate) =
  let lower = max 0.0 (estimate.SmcEstimator.probability -. precision) in
  let upper = min 1.0 (estimate.SmcEstimator.probability +. precision) in

  Format.fprintf fmt
    "@[<v 2>\
     @[<h>  Property @{<blue_b>%s@}:@]@,\
     @[<h>    Violations : %a / %d@]@,\
     @[<h>    Estimate   : %a@]@,\
     @[<h>    Interval   : [%.6g, %.6g]@]\
     @]"
    name
    pp_violations
    estimate.SmcEstimator.violations
    estimate.SmcEstimator.samples
    pp_probability estimate.SmcEstimator.probability
    lower
    upper

let pp_pt fmt config estimates result =
  let precision = SmcEstimator.precision config in
  Format.fprintf fmt
    "@[<v>\
    %a\
    @{<b>Statistical Model-Checking Result@}@,\
    %a\
    @,\
    %a\
    @,\
    @{<b>Samples@}@,\
    @[<h>  Generated : %d@]@,\
    @[<h>  Accepted  : @{<green_b>%d@}@]@,\
    @[<h>  Rejected  : @{<yellow_b>%d@}@]@,\
    @,\
    @{<b>Property estimates@}"
    Pretty.print_line ()
    Pretty.print_line ()
    pp_estimator_pt config
    result.generated
    result.accepted
    result.rejected;
  if estimates <> [] then
    Format.fprintf fmt
      "@,%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (pp_property_pt precision) )
      estimates;
  Format.fprintf fmt "@,@,@]"

let pp_xml = pp_pt (* TODO *)
let pp_json = pp_pt (* TODO *)

let render ~config ~estimates result : KEvent.rendered_result =
  {
    plain =  (fun fmt -> pp_pt fmt config estimates result) ;
    xml = (fun fmt -> pp_xml fmt config estimates result) ;
    json = (fun fmt -> pp_json fmt config estimates result) ;
  }
  
