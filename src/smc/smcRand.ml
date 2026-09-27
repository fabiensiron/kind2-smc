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

let decimal_of_string s =
  let len = String.length s in
  if len = 0 then
    invalid_arg "SMC: empty real value"
  else
    match s.[0] with
    | '-' ->
      Decimal.of_string (String.sub s 1 (len - 1)) |> Decimal.neg
    | '+' ->
      Decimal.of_string (String.sub s 1 (len - 1))
    | _ ->
      Decimal.of_string s

let random_uniform ~range ty =
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
    |> Printf.sprintf "%.17g" |> decimal_of_string |> Term.mk_dec
  | _ ->
    KEvent.log L_error
      "SMC: unsupported input type %a" Type.pp_print_type ty;
    raise (Failure "main")

let fixed_value ty value =
  match Type.node_of_type ty, value with
  | Type.Bool, `Bool value ->
    Term.mk_bool value
  | Type.Int, `Int value
  | Type.IntRange _, `Int value
  | Type.Enum _, `Int value ->
    value
    |> Numeral.of_int
    |> Term.mk_num
  | Type.Int, `Intlit value
  | Type.IntRange _, `Intlit value
  | Type.Enum _, `Intlit value
  | Type.Int, `String value
  | Type.IntRange _, `String value
  | Type.Enum _, `String value ->
    value
    |> Numeral.of_string
    |> Term.mk_num
  | Type.Real, `Float value ->
    value
    |> Printf.sprintf "%.17g"
    |> decimal_of_string
    |> Term.mk_dec
  | Type.Real, `Int value ->
    value
    |> string_of_int
    |> Decimal.of_string
    |> Term.mk_dec
  | Type.Real, `Intlit value
  | Type.Real, `String value ->
    value
    |> Decimal.of_string
    |> Term.mk_dec
  | _ ->
    invalid_arg
      (Format.asprintf
         "SMC: fixed input value %s incompatible with type %a"
         (Yojson.Safe.to_string value)
         Type.pp_print_type
         ty)

let random_distribution ~range ty = function
  | SmcInput.Uniform ->
    random_uniform ~range ty
  | SmcInput.Bernoulli p ->
    begin
      match Type.node_of_type ty with
      | Type.Bool -> Term.mk_bool (Random.float 1.0 < p)
      | _ ->
        invalid_arg
          "SMC: Bernoulli distribution on non-Boolean input"
    end
  | SmcInput.UniformInt (lower, upper) ->
    begin
      match Type.node_of_type ty with
      | Type.Int
      | Type.IntRange _
      | Type.Enum _ ->
        random_int lower upper
        |> Numeral.of_int
        |> Term.mk_num
      | _ ->
        invalid_arg
          "SMC: uniform_int distribution on non-integer input"
    end
  | SmcInput.UniformReal (lower, upper) ->
    begin
      match Type.node_of_type ty with
      | Type.Real ->
        random_real lower upper
        |> Printf.sprintf "%.17g"
        |> decimal_of_string
        |> Term.mk_dec
      | _ ->
        invalid_arg
          "SMC: uniform_real distribution on non-real input"
    end

let random_value ~range ?spec ty =
  match spec with
  | None ->
    random_uniform ~range ty
  | Some (SmcInput.Fixed value) ->
    fixed_value ty value
  | Some (SmcInput.Distribution distribution) ->
    random_distribution ~range ty distribution
