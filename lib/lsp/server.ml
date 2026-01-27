module Lsp = Linol_lsp

type t

(* This is the hard part, what does our state look like? 
   How do we gather the info we need from the 'trace' effect?
   How do we share info between files?
*)
let process_file : string -> t = assert false
let diagnostics : t -> Lsp.Types.Diagnostic.t list = assert false

class lsp_server =
  object (self)
    inherit Linol_eio.Jsonrpc2.server
    val buffers : (Lsp.Types.DocumentUri.t, t) Hashtbl.t = Hashtbl.create 64
    method spawn_query_handler f = Linol_eio.spawn f

    method
      private _on_doc
      ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
      (uri : Lsp.Types.DocumentUri.t)
      (contents : string) =
      let new_state = process_file contents in
      Hashtbl.replace buffers uri new_state;
      let diagnostics' = diagnostics new_state in
      notify_back#send_diagnostic diagnostics'

    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_eio.t =
      Hashtbl.remove buffers d.uri
  end

let run () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun _ ->
      let s = new lsp_server in
      let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
      let task () =
        let shutdown () = s#get_status = `ReceivedExit in
        Linol_eio.Jsonrpc2.run ~shutdown server
      in
      match task () with
      | () -> ()
      | exception e ->
        let e = Printexc.to_string e in
        Printf.eprintf "error: %s\n%!" e;
        exit 1))
;;

let () = run ()
