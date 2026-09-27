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

module SMap = Map.Make(String)

type distribution =
  | Uniform
  | Bernoulli of float
  | UniformInt of int * int
  | UniformReal of float * float

type t =
  | Fixed of Yojson.Safe.t
  | Distribution of distribution

type env = t SMap.t

let empty = SMap.empty

let find_opt = SMap.find_opt

let iter = SMap.iter

let bindings = SMap.bindings

let error file fmt =
  Format.kasprintf
    (fun message ->
       invalid_arg
         (Format.asprintf
            "SMC input file %s: %s"
            file
            message))
    fmt


let json_number_as_float file field = function
  | `Float value -> value
  | `Int value -> float_of_int value
  | `Intlit value
  | `String value ->
    begin
      try
        float_of_string value
      with Failure _ ->
        error file "field %S must be a floating-point number" field
    end
  | _ -> error file "field %S must be a floating-point number" field


let json_number_as_int file field = function
  | `Int value -> value
  | `Intlit value
  | `String value ->
    begin
      try
        int_of_string value
      with Failure _ ->
        error file "field %S must be an integer" field
    end
  | _ -> error file "field %S must be an integer" field


let field file name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> error file "missing field %S in distribution" name


let string_field file name fields =
  match field file name fields with
  | `String value -> value
  | _ -> error file "field %S must be a string" name


let check_probability file p =
  if p < 0.0 || p > 1.0 then
    error file "Bernoulli probability must belong to [0,1], got %g" p


let parse_distribution file fields =
  match string_field file "distribution" fields with
  | "uniform" ->
    Distribution Uniform

  | "bernoulli" ->
    let p = field file "p" fields |> json_number_as_float file "p" in
    check_probability file p;
    Distribution (Bernoulli p)

  | "uniform_int" ->
    let lower = field file "min" fields |> json_number_as_int file "min" in
    let upper = field file "max" fields |> json_number_as_int file "max" in

    if lower > upper then
      error file
        "invalid uniform_int interval [%d,%d]"
        lower upper;

    Distribution (UniformInt (lower, upper))

  | "uniform_real" ->
    let lower = field file "min" fields |> json_number_as_float file "min" in
    let upper = field file "max" fields |> json_number_as_float file "max" in

    if lower > upper then
      error file
        "invalid uniform_real interval [%g,%g]"
        lower upper;

    Distribution (UniformReal (lower, upper))

  | distribution ->
    error file
      "unknown distribution %S"
      distribution


let parse_spec file json_entry =
  match json_entry with
  (* A JSON object denotes a stochastic generator. *)
  | `Assoc fields ->
    parse_distribution file fields
  (* Scalar values denote fixed values. *)
  | (`Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as value ->
    Fixed value
  | value ->
    error file
      "invalid input specification %s"
      (Yojson.Safe.to_string value)


let of_file file =
  let json =
    try
      Yojson.Safe.from_file file
    with
    | Sys_error msg -> error file "%s" msg
    | Yojson.Json_error msg -> error file "%s" msg
  in

  match json with
  | `Assoc fields ->
    List.fold_left
      (fun env (name, json_entry) ->
         if SMap.mem name env then
           error file "input %S is specified more than once" name;

         let spec = parse_spec file json_entry in
         SMap.add name spec env)

      SMap.empty
      fields
  | _ -> error file "top-level JSON value must be an object"


let validate ~name ty spec =
  match spec, Type.node_of_type ty with
  | Fixed (`Bool _), Type.Bool -> ()
  | Fixed (`String _), Type.Int
  | Fixed (`String _), Type.IntRange _
  | Fixed (`String _), Type.Enum _
  | Fixed (`String _), Type.Real -> ()
  | Fixed (`Int _), Type.Int
  | Fixed (`Int _), Type.IntRange _
  | Fixed (`Int _), Type.Enum _ -> ()
  | Fixed (`Intlit _), Type.Int
  | Fixed (`Intlit _), Type.IntRange _
  | Fixed (`Intlit _), Type.Enum _ -> ()
  | Fixed (`Float _), Type.Real -> ()
  | Fixed _, _ ->
    invalid_arg
      (Format.asprintf
         "SMC: fixed value for input %s is incompatible with type %a"
         name
         Type.pp_print_type
         ty)
  | Distribution Uniform, _ -> ()
  | Distribution (Bernoulli p), Type.Bool ->
    if p < 0.0 || p > 1.0 then
      invalid_arg
        (Format.asprintf
           "SMC: invalid Bernoulli probability %g for input %s"
           p name)
  | Distribution (Bernoulli _), _ ->
    invalid_arg
      (Format.asprintf
         "SMC: Bernoulli distribution requires Boolean input %s"
         name)
  | Distribution (UniformInt (lower, upper)), Type.Int
  | Distribution (UniformInt (lower, upper)), Type.IntRange _
  | Distribution (UniformInt (lower, upper)), Type.Enum _ ->
    if lower > upper then
      invalid_arg
        (Format.asprintf
           "SMC: invalid integer distribution [%d,%d] for input %s"
           lower upper name)
  | Distribution (UniformInt _), _ ->
    invalid_arg
      (Format.asprintf
         "SMC: uniform_int requires integer input %s"
         name)
  | Distribution (UniformReal (lower, upper)), Type.Real ->
    if lower > upper then
      invalid_arg
        (Format.asprintf
           "SMC: invalid real distribution [%g,%g] for input %s"
           lower upper name)
  | Distribution (UniformReal _), _ ->
    invalid_arg
      (Format.asprintf
         "SMC: uniform_real requires real input %s"
         name)


let pp fmt = function
  | Fixed value ->
    Format.fprintf fmt "fixed(%s)" (Yojson.Safe.to_string value)
  | Distribution Uniform ->
    Format.fprintf fmt "uniform"
  | Distribution (Bernoulli p) ->
    Format.fprintf fmt "bernoulli(%g)" p
  | Distribution (UniformInt (lower, upper)) ->
    Format.fprintf fmt "uniform_int(%d,%d)" lower upper
  | Distribution (UniformReal (lower, upper)) ->
    Format.fprintf fmt "uniform_real(%g,%g)" lower upper
