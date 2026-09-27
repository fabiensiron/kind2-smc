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

