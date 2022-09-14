(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module Pool = Caqti_eio.Pool

module Resource = struct
  type t = {
    id: int;
    mutable use_count: int;
  }

  let alive = Hashtbl.create 17

  let latest_id = ref 0

  let create () =
    if Random.int 4 = 0 then Promise.create_resolved (Error (Failure "?")) else
    begin
      incr latest_id;
      Hashtbl.add alive !latest_id ();
      Promise.create_resolved (Ok {id = !latest_id; use_count = 0})
    end

  let free resource =
    assert (Hashtbl.mem alive resource.id);
    Hashtbl.remove alive resource.id;
    Promise.create_resolved ()
end

let test_n ~clock ~sw n =
  let max_idle_size = Random.int 11 in
  let max_size = max 1 (max_idle_size + Random.int 5) in
  let max_use_count =
    (match Random.bool () with
     | false -> None
     | true -> Some (1 + Random.int 8))
  in
  let pool =
    Pool.create ~max_idle_size ~max_size ~max_use_count
      Resource.create Resource.free
  in
  let wakers = Array.make n None in
  let wait_count = ref 0 in
  let wait_count_cond = Eio.Condition.create () in
  let wake j u = Promise.resolve u (); wakers.(j) <- None in
  for _ = 0 to 3 * n - 1 do
    let j = Random.int n in
    assert (Pool.size pool = Hashtbl.length Resource.alive);
    (match wakers.(j) with
     | None ->
        let waiter, waker = Promise.create () in
        incr wait_count;
        let task (resource : Resource.t) =
          (match max_use_count with
           | None -> ()
           | Some n -> assert (resource.use_count < n));
          resource.use_count <- resource.use_count + 1;
          Fiber.fork_promise ~sw (fun () ->
            Promise.await waiter;
            decr wait_count;
            Eio.Condition.broadcast wait_count_cond);
        in
        Fiber.fork ~sw (fun () ->
          match Promise.await (Pool.use task pool) with
          | Ok () -> ()
          | Error _exn ->
            Promise.await waiter;
            decr wait_count;
            Eio.Condition.broadcast wait_count_cond);
        wakers.(j) <- Some waker
     | Some u -> wake j u)
  done;
  for j = 0 to n - 1 do
    (match wakers.(j) with
     | None -> ()
     | Some u -> wake j u)
  done;
  let rec wait_for_all () =
    if !wait_count = 0 then Ok () else begin
      Eio.Condition.await_no_mutex wait_count_cond;
      wait_for_all ()
    end
  in
  let r = Eio.Time.with_timeout clock 2.0 (wait_for_all) in
  assert (Result.is_ok r);
  assert (Pool.size pool <= max_idle_size);
  assert (!wait_count = 0);
  Promise.await (Pool.drain pool);
  assert (Pool.size pool = 0);
  assert (Hashtbl.length Resource.alive = 0)

let test ~sw env () =
  let clock = Eio.Stdenv.clock env in
  let test_n = test_n ~clock ~sw in
  test_n 0;
  test_n 1;
  let rec loop n_it =
    if n_it = 0
    then ()
    else begin
      test_n (Random.int (1 lsl Random.int 12));
      loop (n_it - 1)
    end
  in
  loop 500

let test_case
    :  string -> Alcotest.speed_level
    -> (sw:Switch.t -> Eio.Stdenv.t -> unit -> unit)
    -> string * Alcotest.speed_level * (unit -> unit)
  =
 fun desc ty f ->
  ( desc
  , ty
  , fun () -> Eio_main.run (fun env -> Switch.run (fun sw -> f ~sw env ())) )

let test_cases =
  List.map
    (fun (desc, ty, f) -> test_case desc ty f)
    [ "basic usage", `Quick, test
    ;
    ]
