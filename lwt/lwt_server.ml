open Yojson.Safe.Util
(* open Core *)

let onBody req_body =
  let open Core in
  Yojson.Safe.from_string req_body |> fun json ->
    let id = json |> member "id" |> to_string in 
    let first_name = json |> member "first_name" |> to_string in
    let last_name = json |> member "last_name" |> to_string in
    let  now_s = Time.format ~zone:Time.Zone.utc (Time.now ()) "%F %T %z" in 
    `Assoc [
      ("id", `String id);
      ("say", `String "OCaml is the best");
      ("first_name", `String (first_name^ (Md5.digest_string first_name |> Md5.to_hex)));
      ("last_name", `String (last_name ^ (Md5.digest_string last_name |> Md5.to_hex)));
      ("current_time", `String now_s);
    ] |> Yojson.Safe.to_string
;;

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let open Httpaf in


  let body_length r = 
    match Request.body_length r with
    | `Fixed rl -> Int64.to_int rl
    | _ -> 0
  in

  let request_handler : Unix.sockaddr -> _ Reqd.t -> unit =
      fun _client_address request_descriptor ->

    let request = Reqd.request request_descriptor in
    match request.meth with
    | `POST ->
      body_length request |> fun l ->
      let request_body = Reqd.request_body request_descriptor in
      let b = Bigstring.create l in
      let rec respond () =
        Body.schedule_read
          request_body
          ~on_eof:(fun () ->
          Body.close_reader request_body;
          Bigstring.to_string b |> onBody |> fun s -> 
            let response = Response.create ~headers:(Headers.of_list [
            "content-type", "application/json";
            "content-length", (string_of_int (String.length s));
            "connection", "keep-alive";
            ]) `OK in
            Reqd.respond_with_string request_descriptor response s
          )
          ~on_read:(fun request_data ~off ~len ->
            Bigstring.blit request_data off b off len;
            respond ())
      in
      respond ()
    | _ ->
      Reqd.respond_with_string
        request_descriptor (Response.create ~headers:(Headers.of_list [
            "Content-Length", "0";
            "Connection", "keep-alive";
          ]) `Method_not_allowed) ""
  in

  let error_handler :
      Unix.sockaddr ->
      ?request:Httpaf.Request.t ->
      _ ->
      (Headers.t -> [`write] Body.t) ->
        unit =
      fun _client_address ?request:_ error start_response ->

    let response_body = start_response Headers.empty in

    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Core.(Exn.to_string exn));
      Body.write_string response_body "\n";

    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;

    Body.close_writer response_body
  in

  Httpaf_lwt.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler



let () =
  let open Lwt.Infix in

  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes POST requests. Runs forever.";

  let listen_address = (Unix.(ADDR_INET (Unix.inet_addr_any, !port))) in

  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address connection_handler
    >>= fun _server ->
    print_endline @@ "Started on " ^ string_of_int !port;
    Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever