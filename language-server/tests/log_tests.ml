(* Log level tests *)

open Base
open Dm

(* Runs [f] with stderr redirected to a temporary file and returns what was
   written. [Log.handle_event] prints through the buffered [stderr] channel,
   so it is flushed before the file descriptor is put back. *)
let with_captured_stderr f =
  Stdlib.flush Stdlib.stderr;
  let saved = Unix.dup Unix.stderr in
  let path = Stdlib.Filename.temp_file "vsrocq_log_test" ".txt" in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let restore () =
    Stdlib.flush Stdlib.stderr;
    Unix.dup2 saved Unix.stderr;
    Unix.close saved
  in
  (try f () with e -> restore (); raise e);
  restore ();
  Stdlib.In_channel.with_open_text path Stdlib.In_channel.input_all

(* The test binary is started without -vsrocq-d, so this source is not
   selected and, since nobody answers an initialize request here, the logger
   is in its pre-initialization state until the test below flips it. *)
let Types.Log log = Log.mk_log "log_tests"

let%test_unit "levels: before initialization only Error reaches stderr" =
  let out = with_captured_stderr (fun () ->
    log (fun () -> "debug line, source off");
    log ~level:Log.Info (fun () -> "info line, held back");
    log ~level:Log.Error (fun () -> "error line, printed at once")) in
  [%test_eq: bool] (String.is_substring out ~substring:"debug line") false;
  [%test_eq: bool] (String.is_substring out ~substring:"info line") false;
  [%test_eq: bool] (String.is_substring out ~substring:"[ERROR, log_tests") true;
  [%test_eq: bool] (String.is_substring out ~substring:"error line, printed at once") true

(* Calling [lsp_initialization_done] flips a global for the rest of this test
   binary: from here on, lines that a test source selects go to stderr
   instead of the init log. No test reads either, so the order of the two
   tests in this file is the only thing that matters. *)
let%test_unit "levels: initialization releases Info, Debug stays gated" =
  let flushed = with_captured_stderr (fun () -> ignore (Log.lsp_initialization_done ())) in
  [%test_eq: bool] (String.is_substring flushed ~substring:"[ INFO, log_tests") true;
  [%test_eq: bool] (String.is_substring flushed ~substring:"info line, held back") true;
  let out = with_captured_stderr (fun () ->
    log (fun () -> "debug line, still off");
    log ~level:Log.Info (fun () -> "info line, printed at once")) in
  [%test_eq: bool] (String.is_substring out ~substring:"debug line") false;
  [%test_eq: bool] (String.is_substring out ~substring:"info line, printed at once") true
