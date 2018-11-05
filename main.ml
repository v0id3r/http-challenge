open Core
open Async

open Httpaf
open Httpaf_async

open Yojson.Basic.Util

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n";
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body
;;

let onBody req_body =
    let json = Yojson.Basic.from_string !req_body in
    let id = json |> member "id" |> to_string in
    let first_name = json |> member "first_name" |> to_string in
    let last_name = json |> member "last_name" |> to_string in
    let now = Time.now () in
    let  now_s = Time.format ~zone:Time.Zone.utc now "%F %T %z" in
    let json_resp = `Assoc [
        ("id", `String id);
        ("first_name", `String (first_name ^ (Md5.digest_string first_name |> Md5.to_hex)));
        ("last_name", `String (last_name ^ (Md5.digest_string last_name |> Md5.to_hex)));
        ("current_time", `String now_s);
        ] in
    Yojson.Basic.pretty_to_string json_resp

let request_handler _ reqd =
  match Reqd.request reqd  with
  | { Request.meth = `POST; _ } ->
    let response = Response.create ~headers:(Headers.of_list ["content-type", "application/json"; "connection", "close"]) `OK in
    let request_body  = Reqd.request_body reqd in
    let acc = ref "" in
    let rec on_read buffer ~off ~len =
        acc := ((!acc) ^ (Bigstring.to_string ~off ~len buffer));
      Body.schedule_read request_body ~on_eof ~on_read;
    and on_eof () =
        onBody acc |> Reqd.respond_with_string reqd response
    in
    Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ ->  
   Reqd.respond_with_string reqd (Response.create ~headers:(Headers.of_list ["connection", "close"]) `Method_not_allowed) ""
;;

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Tcp.(Server.create_sock ~on_handler_error:`Raise
      ~backlog:10_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    (Server.create_connection_handler ~request_handler ~error_handler)
  >>= fun _ ->
  Deferred.never ()

let () =
  Command.async_spec
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
      +>
      flag "-a" (optional_with_default 1 int)
        ~doc:"int Maximum accepts per batch"
    ) main
  |> Command.run