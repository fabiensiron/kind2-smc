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

type property_result = {
  name : string ;
  violations : int ;
}

type t = {
  generated : int ;
  accepted : int ;
  rejected : int ;
  properties : int Smap.t ;
}

let make ~generated ~accepted ~rejected properties =
  {
    generated ;
    accepted ;
    rejected ;
    properties =
      List.fold_left (fun smap (name, _, _) ->
          Smap.add name 0 smap
      ) Smap.empty properties
  }

let property_results properties =
  List.map
    (fun (name, violations) ->
       { name ; violations })
  @@ Smap.to_list properties

let add_violations stats violations =
  {
    stats with
    properties =
      List.fold_left (fun vs (name, _) ->
          let update v = Some (succ @@ try Option.get v with _ -> 0) in
          Smap.update name update vs
        ) stats.properties violations
  }

let inc_generated stats =
  {
    stats with generated = stats.generated + 1
  }

let inc_accepted stats =
  {
    stats with accepted = stats.accepted + 1
  }

let inc_rejected stats =
  {
    stats with rejected = stats.rejected + 1
  }

let accepted stats = stats.accepted
let rejected stats = stats.rejected
let generated stats = stats.generated

let probability accepted violations =
  if accepted = 0 then
    None
  else
    Some (float_of_int violations /. float_of_int accepted)

let pp_probability fmt p =
  Format.fprintf fmt
    "@{<b>%.6g@}" p

let pp_violations fmt n =
  if n = 0 then
    Format.fprintf fmt "@{<green_b>0@}"
  else
    Format.fprintf fmt "@{<red_b>%d@}" n

let pp_property_pt accepted fmt p =
  match probability accepted p.violations with
  | None ->
    Format.fprintf fmt
      "@[<h>  Property @{<blue_b>%s@}: \
       violations = %d / 0, probability = @{<yellow_h>n/a@}@]"
      p.name
      p.violations
  | Some probability ->
    Format.fprintf fmt
      "@[<h>  Property @{<blue_b>%s@}: \
       violations = %a / %d, probability = %a@]"
      p.name
      pp_violations p.violations
      accepted
      pp_probability probability

let pp_pt fmt result =
  Format.fprintf fmt
    "@[<v>\
    %a\
    @{<b>Statistical Model-Checking Result@}@,\
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
    result.generated
    result.accepted
    result.rejected;
  if Smap.cardinal result.properties <> 0 then
    Format.fprintf fmt
      "@,%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (pp_property_pt result.accepted))
      (property_results result.properties);
  Format.fprintf fmt "@,@,@]"

let pp_xml = pp_pt (* TODO *)
let pp_json = pp_pt (* TODO *)

let render result : KEvent.rendered_result =
  {
    plain =  (fun fmt -> pp_pt fmt result) ;
    xml = (fun fmt -> pp_xml fmt result) ;
    json = (fun fmt -> pp_json fmt result) ;
  }
