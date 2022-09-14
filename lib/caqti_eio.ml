(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
 * Copyright (C) 2022  Antonio Nuno Monteiro <anmonteiro@gmail.com>
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

open Caqti_common_priv

open Eio.Std

module System = struct

  let map: ('a -> 'b) -> 'a Promise.t -> 'b Promise.t = fun
    f t ->
    Promise.create_resolved (f (Promise.await t))

  let bind : 'a Promise.t -> ('a -> 'b Promise.t) -> 'b Promise.t = fun p f ->
    f (Promise.await p)


  type 'a future = 'a Promise.t
  let (>>=) = bind
  let (>|=) t f = map f t
  let return a = Eio.Promise.create_resolved a


  let finally
  : (unit -> 'a Promise.t) -> (unit -> unit Promise.t) -> 'a Promise.t = fun fn cleanup ->
    Fun.protect ~finally:(fun exn -> Promise.await (cleanup exn)) fn

  let cleanup f g =
    try f ()
    with
    | exn -> g () >>= fun () -> raise exn

  let join ps =
    Fiber.all (List.map (fun p () -> (Promise.await p)) ps) ;
    return ()

  module Mvar = struct
    type 'a t = 'a Eio.Stream.t
    let create () =
      (* From https://github.com/ocaml-multicore/eio/blob/355478d6/README.md#streams:
         A stream with a capacity of 1 acts like a mailbox. *)
      Eio.Stream.create 1
    let store v t =
      Eio.Stream.add t v
      (* Lwt.async (fun () -> Lwt_mvar.put v x) *)
    let fetch t =
      return (Eio.Stream.take t)
  end

  module Log = struct
    type 'a log = ('a, unit Promise.t) Logs.msgf -> unit Promise.t

    let kmsg k ?(src = Logs.default) level msgf = match Logs.Src.level src with
    | None -> k ()
    | Some level' when level > level' ->
        (if level = Logs.Error then Logs.incr_err_count () else
         if level = Logs.Warning then Logs.incr_warn_count () else ());
        (k ())
    | Some _ ->
        (if level = Logs.Error then Logs.incr_err_count () else
         if level = Logs.Warning then Logs.incr_warn_count () else ());
        let (ret, unblock) = Promise.create () in
        let k () = bind ret k in
        let over () = Promise.resolve unblock () in
        Logs.report src level ~over k msgf

    let kunit _ = return ()
    let err ?src msgf = kmsg kunit ?src Logs.Error msgf
    let warn ?src msgf = kmsg kunit ?src Logs.Warning msgf
    let info ?src msgf = kmsg kunit ?src Logs.Info msgf
    let debug ?src msgf = kmsg kunit ?src Logs.Debug msgf
  end

  module Unix = struct
    type file_descr = Unix.file_descr

    let wrap_fd f fd = f fd

    external unix_stub_readable : Unix.file_descr -> bool = "caqti_eio_unix_readable"
    external unix_stub_writable : Unix.file_descr -> bool = "caqti_eio_unix_writable"

  let rec unix_readable fd =
    try
      if Sys.win32 then
        Unix.select [fd] [] [] 0.0 <> ([], [], [])
      else
        unix_stub_readable fd
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      unix_readable fd

  let rec unix_writable fd =
    try
      if Sys.win32 then
        Unix.select [] [fd] [] 0.0 <> ([], [], [])
      else
        unix_stub_writable fd
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      unix_writable fd


    let poll ?(read = false) ?(write = false) ?timeout fd =
      let choices =
        [] |> (fun acc -> if read then (fun () -> Eio_unix.await_readable fd) :: acc else acc)
           |> (fun acc -> if write then (fun () -> Eio_unix.await_writable fd) :: acc else acc)
           |> Option.fold (fun t acc -> (fun () -> Eio_unix.sleep t; raise Eio.Time.Timeout) :: acc) timeout in
      if choices = [] then
        invalid_arg "Caqti_lwt.Unix.poll: No operation specified."
      else
        begin
          try
            Fiber.any choices;
            return false
          with
          | Eio.Time.Timeout -> return true
             | exn -> raise exn
        end >>= fun timed_out ->
        return (unix_readable fd, unix_writable fd, timed_out)
  end

  module Preemptive = struct
    let detach : ('a -> 'b) -> 'a -> 'b future = fun f x ->
      let p, u = Promise.create () in
      Eio_unix.run_in_systhread (fun () -> Promise.resolve u (f x));
      p

    let run_in_main : (unit -> 'a future) -> 'a = fun f ->
      Promise.await (f ())
  end

  module Stream = Caqti_stream.Make (struct
    type nonrec 'a future = 'a future
    let (>>=) = (>>=)
    let (>|=) = (>|=)
    let return = return
  end)

end

include Caqti_connect.Make_unix (System)

let or_fail = function
 | Ok x -> System.return x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
