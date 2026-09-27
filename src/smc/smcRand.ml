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

let int_min = ref (-1000)
let int_max = ref 1000
let real_min = ref (-1000.0)
let real_max = ref 1000.0

let init () =
  begin
    match Flags.SMC.seed () with
    | Some seed -> Random.init seed
    | None -> Random.self_init ()
  end;

  int_min := Flags.SMC.int_min ();
  int_max := Flags.SMC.int_max ();
  real_min := Flags.SMC.real_min ();
  real_max := Flags.SMC.real_max ();

  if !int_min > !int_max then
    begin
      KEvent.log L_error
        "SMC: --smc-int-min must be smaller than --smc-int-max";
      raise (Failure "main")
    end;

  if !real_min > !real_max then
    begin
      KEvent.log L_error
        "SMC: --smc-real-min must be smaller than --smc-real-max";
      raise (Failure "main")
    end


let random_real min max =
  if min > max then
    begin
      KEvent.log L_error
        "SMC: empty real sampling interval [%g, %g]" min max;
      raise (Failure "main")
    end
  else if min = max then min
  else
    min +. Random.float (max -. min)

let random_int min max =
  if min > max then
    begin
      KEvent.log L_error
        "SMC: empty integer sampling interval [%d, %d]" min max;
      raise (Failure "main")
    end
  else if min = max then min
  else
    Random.int_in_range ~min ~max

let random_bool () =
  Random.bool ()

let random_value ~range ty =
  match Type.node_of_type ty with
  | Type.Bool -> random_bool () |> Term.mk_bool
  | Type.Int ->
    let min, max =
      match range with
      | Some (Some min, Some max) -> min, max
      | Some (Some min, None) -> min, !int_max
      | Some (None, Some max) -> !int_min, max
      | Some (None, None)
      | None -> !int_min, !int_max
    in
    random_int min max |> Numeral.of_int |> Term.mk_num
  | Type.IntRange (Some lb, Some ub) ->
    random_int (Numeral.to_int lb) (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
  | Type.IntRange (None, Some ub) ->
    random_int !int_min (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
  | Type.IntRange (Some lb, None) ->
    random_int (Numeral.to_int lb) !int_max |> Numeral.of_int |> Term.mk_num
  | Type.IntRange (None, None) ->
    random_int !int_min !int_max |> Numeral.of_int |> Term.mk_num
  | Type.Enum (lb, ub) ->
    random_int (Numeral.to_int lb) (Numeral.to_int ub) |> Numeral.of_int |> Term.mk_num
  | Type.Real ->
    random_real !real_min !real_max
    |> Printf.sprintf "%.17g" |> Decimal.of_string |> Term.mk_dec
  | _ ->
    KEvent.log L_error
      "SMC: unsupported input type %a" Type.pp_print_type ty;
    raise (Failure "main")

