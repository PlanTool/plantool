;;;; File: get-url-support.lsp
;;; Contains: Support code for getting URLs
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 9 Apr 95
;;; Updated: Tue Apr 20 01:31:42 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan)

;;;; Getting http URLs

;;; This is (on the one hand) kinda cool, and yet (on the other)
;;; a hack.  Telnet does much of the work.

;;; Anyway, once we can get files from URLs, we have forms in which
;;; people can specify TF files to syntax-check or use for planning.

(defparameter *get-http-url-parameters*
  '((:host  (:text)   "host name")	;e.g. localhost
    (:port  (:int 0)  "port number")	;usually 80
    (:path  (:text)   "path")))		;e.g. /~jeff/

(defun get-http-url-cgi ()
  (write-cgi-response-headers "text/plain")
  (with-web-environment "get-http-url"
    (parse-query-args)
    (convert-query-args *get-http-url-parameters*)
    (with-open-stream
        (stream (get-http-url-stream (query-arg :host)
				     (query-arg :port)
				     (query-arg :path)))
      (loop
        (let ((line (read-line stream nil :eof)))
	  (when (eq line :eof)
	    (return))
	  (write-line line))))))

(defun get-http-url-stream (host port path) ; -> stream
  (unless (sequence-begins-p "/" path)
    (setq path (concat-string "/" path)))
  ;; Run telnet and get a stream for talking to it.
  (let ((io (telnet-io host port)))
    ;; Wait for output reporting the connection.
    (read-initial-telnet-lines io)
    ;; Send a GET request.
    (web-note "~&Sending: GET ~A~%~%" path)
    (output (:stream io)
      "GET " path //)
    (finish-output io)
    ;; Return the io-stream that's connected to the telnet process.
    io))

(defun telnet-io (&rest args) ; -> stream
  ;; Run telnet redirecting error output to a tmp file.
  ;; Expected error output is "Connection closed by foreign host."
  (web-note "~&telnet~{ \"~A\"~}~%" args)
  (unix-process-io "/bin/sh"
    "-c" (format nil "telnet~{ \"~A\"~} 2> ~A"
		 args
		 (web-tmp-filename "telnet" "err"))))

(defun read-initial-telnet-lines (io)
  (unless (pseudo-select-p (list io) 15)
    (error "telnet took too long to start."))
  (web-note "Initial telnet lines:~%")
  (handler-case
      (dolist (expect '("Trying " "Connected to " "Escape character is "))
	(web-note "   ~S~%"
          (expect-line-beginning expect io)))
    (error (c)
      (web-note "~&~%Telnet problem:~%~A~&~%" c)
      (error "Can't get that URL.  Are you sure it exists?"))))


;;; Utilities

(defun expect-line-beginning (prefix stream) ; -> line
  (let ((line (read-line-no-hang stream)))
    (if (sequence-begins-p prefix line)
	line
      (error "Expected a line beginning with ~S, but found ~S."
	     prefix line))))

(defun sequence-begins-p (prefix seq)
  (eql 0 (search prefix seq
		 :start2 0
		 :end2 (min (length prefix) (length seq)))))

(defun read-line-no-hang (stream) ; -> line, success-p
  (let ((line (make-string-output-stream)))
    (loop
      (let ((char (read-char-no-hang stream)))
	(cond ((null char)
	       (return (values (get-output-stream-string line) nil)))
	      ((char= char #\Newline)
	       (return (values (get-output-stream-string line) t)))
	      (t
	       (write-char char line)))))))


;;; Notes

;;; Since we get the host, path, etc from a FORM, they're already
;;; encoded in the way required for URLs (space as +, etc).  That's
;;; why the parameters have type :string rather than :text.  If they
;;; were :text, they'd be decoded by convert-query-args.

;;; /\/: Actually, that seems to be wrong.  They have to be :text.
;;; At least now (April 99) on Spottisvax.

;;; "GET path" is a SimpleRequest.  We're sent the contents without
;;; any response headers such as "Content-type" or "Content-length".
;;; Then the connection's closed by the server.  We could send a
;;; FullRequest instead:
;;;
;;;   GET path ProtocolVersion
;;;   header-lines
;;;   blank-line
;;;
;;; Then we would get in sequence: response headers, blank line,
;;; contents, connection closed.  We're not guaranteed to get a
;;; "Content-length".
;;;
;;; If we did send a FullRequest, no header lines would be required,
;;; but we should send
;;;
;;;   Accept: text/plain
;;;   Accept: text/html
;;;   User-Agent:  O-Plan/3.x
;;;
;;; text/plain and text/html are assumed if no "Accept:"s are sent.

;;; 4.4 BSD telnet accepts -E for no escape sequence, -8 for 8-bit data.
;;; SunOS 4.1.x telnet doesn't.

;;; Sample telnet output:
;;;
;;;   ebay 34% telnet www.aiai.ed.ac.uk 80
;;;   Trying 192.41.105.2 ...
;;;   Connected to www.
;;;   Escape character is '^]'.
;;;
;;; Of course, we might also get something like this:
;;;
;;;   ebay 36% telnet localhost 80        
;;;   Trying 127.0.0.1 ...
;;;   telnet: connect: Connection refused
;;;   telnet>
;;;
;;; But the "Connection refused" line goes to stderr.

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
