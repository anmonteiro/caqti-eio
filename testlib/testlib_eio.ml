(* Copyright (C) 2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Eio.Std

type 'a future = 'a Promise.t
let return a = Eio.Promise.create_resolved a

let map: ('a -> 'b) -> 'a Promise.t -> 'b Promise.t = fun
  f t ->
  Promise.create_resolved (f (Promise.await t))

let bind : 'a Promise.t -> ('a -> 'b Promise.t) -> 'b Promise.t = fun p f ->
  f (Promise.await p)

module Result = struct
let map: ('a -> 'b) -> ('a, 'e) result Promise.t -> ('b, 'e) result Promise.t = fun
  f t ->
  match (Promise.await t) with
  | Ok t -> Promise.create_resolved (Ok (f t))
  | Error e -> Promise.create_resolved (Error e)

let bind : ('a, 'e) result Promise.t -> ('a -> ('b, 'e) result Promise.t) -> ('b, 'e) result Promise.t =
  fun p f ->
  match Promise.await p with
  | Ok t -> (f t)
  | Error e -> Promise.create_resolved (Error e)

end

let (>>=) = bind
let (>|=) t f = map f t


let catch: (unit -> 'a future) -> (exn -> 'a future) -> 'a future = fun f error_fn ->
  try f ()
  with
  | exn -> error_fn exn

let fail exn = Promise.create_resolved (raise exn)

let or_fail = Caqti_eio.or_fail

let (>>=?) = Result.bind
let (>|=?) t f = Result.map f t

module Caqti_sys = Caqti_eio

module Io = struct
  type 'a t = 'a future
  let return = return
  let bind = bind
  let catch = catch
end

module Alcotest_cli = Testlib.Make_alcotest_cli (Alcotest.Unix_platform) (Io)

module List_result_future = struct
  let rec iter_s f = function
   | [] -> return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end
