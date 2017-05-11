;;;; File: http-server.lsp
;;; Contains: CGI code for starting a server and handling requests
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1997
;;; Updated: Thu Feb 24 05:45:13 2000 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)

#+kcl
(import '(fork::call-real-select fork::real-select-p))
#-kcl
(error "Forget it.  No select, no server.")

;;; Contents:
;;;  * Explanation
;;;  * CGI entry points
;;;     - Starting a server
;;;     - Join
;;;     - Restart
;;;  * Non-CGI entry point
;;;  * HTTP server init and top-level loop
;;;  * Startup actions
;;;  * Request handlers
;;;  * HTTP requests
;;;  * HTTP request handler
;;;  * Reading HTTP requests
;;;  * Replies to HTTP requests
;;;     - Configuration and dispatcher
;;;     - Response status line and headers
;;;     - Redirection
;;;     - Log-file requests
;;;     - Exit requests
;;;     - File requests
;;;  * Useful procedures of various sorts
;;;     - Fork / exec supervision
;;;     - Host-name lookup
;;;     - Lists, etc.
;;;     - String and I/O utilities


;;;; Explanation
;;;
;;; [In example URLs, capitalized names are syntactic variables, and
;;; the "-" char is part of the names, not punctuation.]
;;;
;;; This file implements a number of CGI-based operations invoked by
;;; visiting URLs of the form
;;;
;;;   http://Initial-Host/.../Program.cgi/Command/...
;;;
;;; The most important Command is "start-server".
;;;
;;; A server is started by visiting a URL of the form
;;;
;;;   http://Initial-Host/.../Program.cgi/start-server/Demo-Name
;;;
;;; where Program.cgi is a CGI script that runs O-Plan, telling O-Plan
;;; to load this file (http-server) and then call the server-cgi procedure
;;; defined below.  The server-cgi procedure will then call start-server.
;;; (Other Commands cause it to call other procedures.)
;;;
;;; Note that O-Plan-related CGI programs may run on a different host
;;; than Initial-Host.  This is normally invisible to the user, but
;;; then the CGI program starts an HTTP server, the server will be on,
;;; and URLs will have to refer to, that other host.
;;;
;;; The start-server procedure obtains a socket, binds the socket to a
;;; free port (supplied by the operating system), and then forks.  The
;;; child becomes a kind of HTTP server listening to the socket.  The 
;;; parent outputs a "location" header to redirect the Web client (usually
;;; someone's browser) to this new server, and then exits.  The redirection
;;; URL looks like this:
;;;
;;;   http://Server-Host:Port/Demo-Name/
;;;
;;; or (for join) like this:
;;;
;;;   http://Server-Host:Port/Demo-Name/Suffix                 [see below]
;;;
;;; and, in general, the HTTP server expects
;;;
;;;   http://Server-Host:Port/...
;;;
;;; where the Server-Host is determined by calling http-server-host
;;; (it is often the same as the Initial-Host), and the Port is the 
;;; one bound to the socket.  The "..." is interpreted as specified
;;; by *http-uri-interpretations*.  This can include references to files
;;; and CGI-like "actions" that call Lisp procedures rather than running
;;; programs.
;;;
;;; It's assumed that the HTTP requests (as seen by the server) do not
;;; contain the http://Server-Host:Port part of the URLs.
;;;
;;; The server is normally used to handle requests from a single client
;;; who is running an O-Plan "Web demo".  It is then a kind of interface
;;; to O-Plan.  If several people are running server-based demos at the
;;; same time, they will each get a separate server, because the link
;;; that takes them to the demo will be a link to a Start-Server URL.
;;; (Actually, a single demo may allow more than one client to conect,
;;; usually in distinct "user roles".  This is handled by using the
;;; command "join" rather than "start server".  See below.  Nonetheless,
;;; separate demos -- "sessions" -- will still get separate servers.)
;;;
;;; O-Plan demos use action URLs that look like this:
;;;
;;;   http://Server-Host:Port/Demo-Name/...
;;;
;;; They are interpreted by the http-demo-action procedure defined
;;; by the demo support code.  (That is, URLs of that form result
;;; in a call to http-demo-action.)
;;;
;;; The server can also handle other URLs, such as references to files.
;;; URL interpretations are specified by *http-uri-interpretations*.
;;; Demo-Name is added as a new interpretation when the server starts up.
;;;
;;; A demo that requires a password should (setq *http-password-p* t).
;;; The password lookup is provided by the CGI-environment code,
;;; and the password is checked only before the server is started.
;;;


;;;; /\/: Put all config here?


;;;; CGI entry point

(defvar *server-cgi-demo-id* nil)	;to put in the server notes file

(defvar *server-cgi-query-args* nil)	;something to remember

(defun server-cgi ()
  (with-web-environment "server-cgi"
    #-:gcl-2
    (sys:catch-bad-signals)		;so we can log that they happened
    (setq *server-cgi-demo-id* *web-demo-id*)
    (let ((command (string->keyword (first (get-path-info))))
	  (demo-name (server-demo-name)))
      (save-server-cgi-query-args)
      (load-demo-system-if-one-exists demo-name)
      (instantiate-demo-class demo-name)
      (ecase command
        (:start-server (start-server))
	(:join         (join-server))
	(:restart      (restart-server))))))

(defun started-by-cgi-p ()		;because we aren't always
  *server-cgi-demo-id*)

(defun save-server-cgi-query-args ()
  (parse-query-args)
  (setq *server-cgi-query-args* 
	(copy-hash-table *query-arg-table* :test #'eq)))

(defun load-demo-system-if-one-exists (demo-name)
  (let* ((system-name (concat-name (string-upcase demo-name) "-DEMO"))
	 (system (find-system system-name)))
    (when system
      (unless (system-load-date system)	;already loaded?
        (let ((*standard-output* *web-notes*))
	  (load-system system-name
		       :recursive :if-not-loaded))))))

(defun instantiate-demo-class (demo-name)
  (let ((class-name (concat-name (string-upcase demo-name) "-DEMO")))
    (if (find-class class-name nil)
	(setq *demo*
	      (make-instance class-name
			     :name demo-name))
      (error "Cannot find demo class ~S." class-name))))


;;;; Starting a server

;;; Calls listen-socket before forking to avoid a potential timing bug.

;;; Remember that start-server is also called (to start the server)
;;; when we see the first attempt by a user to join a session.

(setq *cgi-content-type* nil)		;headers not sent yet

(defparameter *http-server-host* nil)	;see http-server-host

(defparameter *http-max-waiting-requests* 5)

(defun start-server (&key (url-suffix ""))
  (check-client-password)
  ;; Make sure we can get the host name before forking.
  (web-note "~&Starting server, host ~A.~%~%" (http-server-host))
  ;; Standard socket stuff.
  (let* ((demo-name (server-demo-name))
	 (s (create-socket))
	 (p (bind-socket-to-a-free-port s)))
    (listen-socket s *http-max-waiting-requests*)
    ;; Fork.
    (let ((pid (fork:fork)))
      (cond ((null pid)
	     ;; Child.
	     (setq *web-demo-name* demo-name)
	     ;; Record the CGI demo id so it will appear in the
	     ;; server's notes file.
	     (set-parameter :server-cgi-demo-id *server-cgi-demo-id*)
	     ;; Add URI interpretations for the current demo.
	     (add-demo-http-request-handlers)
	     ;; Become an http server.
	     (http-server s p))
	    (t
	     ;; Parent
	     ;; /\/ Close the socket?  (But we will, after all, exit.)
	     (web-note "~&Server for ~S started on port ~S, pid = ~S~%~%"
		       demo-name p pid)
	     (redirect-to-server p demo-name :url-suffix url-suffix))))))

(defun redirect-to-server (server-port demo-name &key (url-suffix ""))
  (cgi-redirection-response
    (http-server-url server-port (concat-string demo-name "/" url-suffix))))

(defun cgi-redirection-response (redirect-url) ;/\/ move to cgi-env.lsp
  (write-cgi-response-headers "text/html"
    (concat-string "Location: " redirect-url))
  (html-paragraph 
    "Your browser does not understand the \"Location\" header."
    "So you'll have to " (html-anchor redirect-url "do it yourself.")))

(defun http-server-url (port name)
  ;; N.B. the http server can interpret the name any way it likes.
  (concat-string "http://" (http-server-host)
		 ":"       (int->string port)
		 "/"       name))

(defun http-server-host ()
  ;; We normally stay on the host that received the CGI request,
  ;; but not always.  The parameter allows it to be specified by
  ;; the CGI script.
  (ensuref *http-server-host*
	   (or (get-parameter :server-host)
	       (default-http-server-host))))

(defun default-http-server-host ()
  ;; Of course, the server's not always started by CGI these days.
  (if (is-http-p)
      "localhost"
    (let ((server-name (getenv-else-error "SERVER_NAME")))
      (cond ((string= server-name "www.aiai.ed.ac.uk")
	     ;; was "gairsay.aiai.ed.ac.uk"
	     server-name)
	    (t
	     server-name)))))

(defun server-demo-name ()		;defaults to "http-server"
  (let* ((path (get-path-info))
	 (demo-name (or (second path) "http-server")))
    (assert (>= (length demo-name) 1))
    ; (assert (null (drop 2 path)))	;no -- allow more after the demo name
    demo-name))

;;; Password

(defparameter *http-password-p* nil)

(defparameter *http-password-parameters* 
  ;; The password is optional only to avoid signalling an error.
  '((:user     (:optional (:text))   "user")
    (:password (:optional (:text))   "password")))

(defun check-client-password ()
  (when *http-password-p*
    (convert-query-args *http-password-parameters*)
    (unless (correct-password-p "oplan" (query-arg :password))
      (write-cgi-response-headers "text/html")
      (html-standard-page (:title "Error" :title-header nil)
	(html-line "Invalid password."))
      (exit-web-demo))))


;;;; Join
;;;
;;; In some cases, we want more than one user to be able to send requests
;;; to the same server.
;;;
;;; Each user must specify a session name and a user role.  This is
;;; usually done via an HTML form that has a join URL (as described
;;; below) as its action URL and parameters called ""session" and
;;; "role".
;;;
;;; If the user role begins with "ALL" or "BOTH", then we assume there 
;;; will only ever be one user, and we start a server in the usual way,
;;; as if no session were involved.
;;;
;;; Otherwise, when the first user for a given session name comes along,
;;; we create a session file and start a server.  The server writes
;;; some things about itself to the file, and later users can then be
;;; directed to that server.
;;;
;;; The join URL looks like this:
;;;
;;;   http://Initial-Host/.../Program.cgi/join/Demo-Name
;;;
;;; It's enough like a start-server URL that server-demo-name will work.
;;;
;;; Once Program.cgi has either started the HTTP server, or else found
;;; the port number of an existing server for this session-name, it
;;; sends a response that redirects the user's browser to a URL that
;;; refers to the server.
;;;
;;; This redirection URL looks like:
;;;
;;;   http://Server-Host:Port/Demo-Name/Role
;;;
;;; Where Role is produced by the Lisp function role-for-url when given
;;; the "role" parameter from the HTML form.
;;;
;;; We assume that each demo that can be joined defines:
;;;   * (check-user-role role)  --  check kwd derived from (query-arg :role)
;;;   * (role-for-url role)     --  cvt role kwd to suffix for redirect url
;;;
;;; In the server, the demo is also responsible for:
;;;   * Calling write-server-pid-and-port.
;;;   * Dealing with > 1 request for the same role.
;;;   * Deleting the session file after everyone has joined.
;;;   * Redefining valid-client-address-p to allow requests from > 1 host.
;;;

(defparameter *http-join-parameters*
  `((:session (:text)     "session name")
    (:role    (:keyword)  "user role")))

(defvar *join-server-max-wait* 10)	;seconds

(defvar *join-server-attempt* 0)

(defun join-server ()
  ;; Check the password if we're supposed to.
  (when *http-password-p*
    (check-client-password)
    (setq *http-password-p* nil))	;in case we call start-server /\/

  ;; Get the session name and user role.
  (convert-query-args *http-join-parameters*)
  (let ((filename (join-filename (query-arg :session)))
	(role (query-arg :role)))

    ;; If role is :all or :both, just start a server as if no
    ;; session were involved.
    (when (or (sequence-begins "BOTH" (symbol-name role))
	      (sequence-begins "ALL"  (symbol-name role)))
      (return-from join-server
        (start-server)))

    ;; If > 1 actual user, need a real session id.
    (when (= (length (query-arg :session)) 0) 	;nil or ""
      (error "Null session id."))

    ;; Make sure the role is one recognised by the demo.
    (check-user-role role)

    ;; See what's what.
    (join-or-start-session filename role)))

(defun join-or-start-session (filename role)
  ;; Make sure we don't keep doing this forever because of the recursive
  ;; call in try-to-join-existing-server.
  (incf *join-server-attempt*)
  (when (> *join-server-attempt* 1)
    (web-note "~&~@(~:r~) attempt~%~%" *join-server-attempt*))
  (when (> *join-server-attempt* 2)
    (error "Persistent bogus session file ~S" filename))
  ;; Try to create the session file.
  ;; The open below is assumed to be atomic.
  (let ((stream (open filename :direction :output :if-exists nil)))
    (cond (stream
	   ;; We're working for the first user who tried to join and
	   ;; so should start a server.  The server will record the
	   ;; port for connections in the session file.
	   (web-note "~&First user for session ~S.~%~%" (query-arg :session))
	   (set-parameter :session-filename filename)
	   (close stream)
	   (start-server :url-suffix (role-for-url role)))
	  (t
	   ;; The session file already exists.  We're either working for
	   ;; a later user, not the first, or else the file is left over
	   ;; from an earlier run.
	   (web-note "~&Session file ~S already exists.~%~%" filename)
	   (try-to-join-existing-server filename role)))))

(defun try-to-join-existing-server (filename role)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)

    ;; We need to read the session file to get the server's pid and
    ;; its port number for connections, but we may have to wait a bit
    ;; before the information appears.
    (let ((port nil)
	  (pid nil))
      (dotimes (slept *join-server-max-wait*)
	(when (> (file-length stream) 0)
	  (web-note "~&Slept ~S times waiting for data in ~S~%~%"
		    slept filename)
	  (multiple-value-setq (pid port)
	    (read-server-pid-and-port stream))
	  (return))
	(sleep 1))

      ;; If we failed to get the numbers, then either the server
      ;; died before writing them or else the server's very slow.
      ;; Since it's not clear which, we'll just tell the user
      ;; about the problem.  If the file is a leftover, we ought to
      ;; have some way to get rid of it, but in this case we don't.  /\/
      (unless port
	(error "Cannot find the server for session ~S." (query-arg :session)))

      ;; If we have a port, see if the server is still running.
      ;; If it is, redirect the user to the server; otherwise,
      ;; we have a leftover file and should get rid of it.
      (cond ((process-exists-p pid)
	     (web-note "~&Joining session ~S in role ~S.~%~%"
		       (query-arg :session) role)
	     (redirect-to-server
	       port (server-demo-name) :url-suffix (role-for-url role)))
	    (t
	     ;; Get rid of the leftover file by renaming and then
	     ;; try again from the beginning.  [Maybe we're really
	     ;; the first user for this session, but it looked like
	     ;; we weren't because the file already existed.]
	     (web-note "~&Leftover session file ~S.~%~%" filename)
	     (rename-file filename (concat-string filename ".leftover"))
	     (join-or-start-session filename role))))))

(defun join-filename (session)
  (web-demo-filename
    (concat-string "tmp/" session ".session")))

(defun write-server-pid-and-port (filename)
  ;; We use write-line to make it more likely that everything will
  ;; be written at once.
  (with-open-file (stream filename :direction :output
			  :if-does-not-exist :error)
    (assert (zerop (file-length stream)))
    (write-line (prin1-to-string (list (get-parameter :http-server-pid)
				       (get-parameter :http-port)))
		stream)
    (finish-output stream)))		;do we need this? /\/

(defun read-server-pid-and-port (stream) ; -> pid, port
  (let* ((line (read-line stream))
	 (list (read-safely (make-string-input-stream line))))
    (assert (every #'numberp list))
    (assert (= (length list) 2))
    (values-list list)))

(defun encode-for-url (s) ; -> string
  (check-type s simple-string)
  (let ((parts '())
	(start 0))
    (declare (fixnum start))
    (dotimes (i (length s))
      (declare (fixnum i))
      (let ((c (schar s i)))
	(unless (or (alphanumericp c) (find c "$-_."))
	  (when (> i start)
	    (push (subseq s start i) parts))
	  (push "%" parts)
	  (push (int->hex-string (char-int c)) parts)
	  (setq start (1+ i)))))
    (cond (parts
	   (when (< start (length s))
	     (push (subseq s start) parts))
	   (big-string-concat (nreverse parts)))
	  (t
	   s))))

(defun int->hex-string (i)
  (let ((*print-base* 16))
    (princ-to-string i)))


;;;; Restart

;;; This is somewhat confusing.

;;; A "restart URL" is put into an HTML page by a server demo to give
;;; the user a way to kill the demo's server, if it is still running,
;;; and then go to the initial page of the demo.  That initial page
;;; will then allow a new instance of the demo to be started.  Going to
;;; the restart URL will not in itself start a new instance.

;;; Note that the restart URL is not one that will be handled by the demo
;;; server; it instead causes a separate CGI invocation.  This allows us
;;; to get the server to exit even it's somewhat messed up, because the
;;; server is sent a signal rather than an http request.

;;; The demo support code calls server-restart-url when it wants to make
;;; a restart link.

;;; Server-restart-url assumes that *restart-cgi-program* understands the
;;; "restart" command (which it will if it works in the way described
;;; at the start of this file).  Following the URL will then lead to 
;;; restart-server (below) being called.

;;; Restart-server assumes there is a Demo-Name.html that provides (re)entry
;;; to the demo.

(defparameter *restart-cgi-program* "matrix-server.cgi")

(defun server-restart-url ()
  (web-special-url
    (concat-string *restart-cgi-program*
               "/" "restart"
               "/" *web-demo-name* 
	       "/" *web-demo-id*
               "/" (int->string (fork:getpid)))))

(defun restart-server ()
  (destructuring-bind (demo-name demo-id pid) (cdr (get-path-info))
    (setq pid (string->int pid))
    ;; Kill server if it's still running
    (when (and (process-exists-p pid)
	       (file-exists-p (server-lock-file demo-name demo-id)))
      (web-note "~&Killing pid ~D~%~%" pid)
      (fork:kill pid 2))		;kill -INT pid
    ;; Redirect to initial page
    (cgi-redirection-response
      (web-special-url (concat-string demo-name ".html")))))

(defun process-exists-p (pid)
  (fork:kill pid 0))

#+:undef
(defun process-exists-p (pid)
  (let ((status-lines
	 (stream->lines
	  (fork:process-receive "ps" "-p" (int->string pid)))))
    (web-note "~&Status of pid ~D:~%~%~{~S~%~}~%" pid status-lines)
    (> (length status-lines) 1)))

(defun server-lock-file (*web-demo-name* *web-demo-id*)
  (web-tmp-filename "running" "lock"))


;;;; Non-CGI entry point

;;; We sometimes want to run as an HTTP server directly rather than
;;; via CGI.  One reason is that we can do it even on machines where
;;; no one has set up the O-Plan "Web demos" (which can be tricky,
;;; since CGI is often configured in awkward ways).

;;; The commands "oplan -http" and "oplan -netscape" end up here
;;; via the routines in http-mode.lsp.  "-netscape" is equivalent
;;; to "-http -browser netscape".  If a browser name is specified
;;; and contains "netscape", we will fork a child process that
;;; tries to direct the browser to an appropriate URL.  It will
;;; direct an existing Netscape, if there is one on display :0.0,
;;; or else run a new Netscape.  (We know how to do this for
;;; Netscape, but not for other browsers.)  If no browser is
;;; specified, or if the name does not begin with "netscape",
;;; we will merely print the "appropriate URL" so that the user
;;; can visit it manually.

;;; By default, we let the operating system pick a free port for us,
;;; but the -port command-line-argument can be used when some particular
;;; port is desired.

;;; /\/: The HTTP server should be made independent of CGI details.
;;; All we really need from the CGI code is the notes file and some
;;; of the URL routines; so those things should be made less dependent
;;; as well.

(defun run-as-http-server ()
  (let ((domain-name (get-parameter :domain))
	(class-name (or (get-parameter :interface-class)
			"any-tf-http-server"
			(error "No -interface-class was specified."))))
    (let ((*web-notes* *standard-output*))
      (load-demo-system-if-one-exists class-name))
    (setq *web-demo-name* class-name)
    (instantiate-demo-class class-name)
    (add-demo-http-request-handlers))
  ;; Set up certain parts of our CGI env.
  (setq *web-demo-name* (string-downcase (demo-name *demo*)))
  (set-query-arg-string "")
  (letf (((get-parameter :oplan-tf-dir) (get-parameter :oplan-tf-dir)))
    (establish-web-setup))
  (go-faster)
  (catch :web-demo-exit
    ;; Standard socketry.
    (let* ((s (create-socket))
	   (p (if (parameter-set-p :port)
		  (progn (bind-socket s (server-port))
			 (server-port)) 		;see server-mode.lsp
		(bind-socket-to-a-free-port s))))
      (listen-socket s *http-max-waiting-requests*)
      ;; Early initialization - things to do before directing the browser
      ;; and without waiting for a request.  The port must be available
      ;; to use in URLs.
      (letf (((get-parameter :interactive) t))
	(let ((*http-port* p))
	  (declare (special *http-port*))
	  (init-http-server-demo-methods *demo*)))
      ;; Tell the user or browser the URL to visit
      (direct-user-to-server-url p)
      ;; Set up a read-eval-print loop
      (set-up-http-server-repl)
      (allow-http-server-repl-cycle)
      ;; Start acting as a server.
      (http-server s p))))

(defun direct-user-to-server-url (port)
  (let ((url ; Was: (http-server-url port "plan/car")
	     (http-server-url
	       port
	       (concat-string (demo-name *demo*) "/")))
	(browser (get-parameter :browser)))
    (format t "~&~%~A~%~%" url)
    (when (and browser (search "netscape" browser))
      (let ((pid (fork:fork)))
	(if pid
	    ;; Parent
	    (record-child-pid pid)	;so we'll check its status
	  ;; Child
	  (progn
	    (direct-netscape-to-url url browser)
	    (exit-lisp)))))))

(defun direct-netscape-to-url (url &optional (netscape "netscape"))
  (multiple-value-bind (pid)
      (values-list
       (fork:process netscape
	  :args `("-remote"
		  ,(concat-string "openURL(" url ",new-window)"))))
    (let* ((status (fork:wait))
	   (status-pid (car status))
	   (status-value (cdr status)))
      (assert (= status-pid pid))
      (unless (zerop status-value)
	;; Didn't work.  We'll assume no netscape was already running,
	;; and hence that it's up to us to run one.
	(system (concat-string netscape " " url "&"))	;independence!
	#+undef
	(fork:execlp netscape url)))))

(defun set-up-http-server-repl ()
  (let ((repl (new-repl :start-fn 'print-repl-greeting)))
    (add-request-handler
      (repl-io repl)
      'http-server-repl-request))
  (advice+ 'pprocess::repl-eval 'http-server
    #'(lambda (previous)
	#'(lambda (form)
	    (web-note "~&~%Eval request: ~S~%" form)
	    (let ((vals (multiple-value-list (funcall previous form))))
	      (web-note "~&Results:~%~{~S~%~}~%" vals)
	      (values-list vals))))))

(defun http-server-repl-request (io)
  (let ((repl (exists-pprocess :lisp-listener)))
    (handler-bind ((condition
		    #'(lambda (c)
			(break "~A" c)
			(format io "~%~A" (repl-prompt repl))
			(return-from http-server-repl-request))))
      (assert (eq io (repl-io repl)))
      (allow-http-server-repl-cycle))))

(defun allow-http-server-repl-cycle ()
  (let ((repl (exists-pprocess :lisp-listener)))
    (when repl
      (let ((*all-pprocesses* (list repl)))
	(pprocess-main-loop)))))


;;;; The HTTP server

(define-condition request-timeout (simple-condition) ())

(defparameter *http-config-parameters*
  '(*http-server-host*
    *http-max-waiting-requests*
    *http-request-timeout*
    *http-uri-interpretations*
    *http-file-root*
    *http-file-content-types*
    *http-default-file-content-type*))

(defparameter *http-request-timeout* (* 60 60)) ;seconds

(defvar *http-socket* nil)		;what we listen to
(defvar *http-port* nil)		;our address

(defvar *http-startup-date* nil)	;for "Expires" headers

(defvar *http-client-addresses* nil)	;valid addresses (as ints)
					; or nil if all addrs are valid

(defvar *http-startup-actions* nil)	;list of fns to call at startup

;;; Init and top-level loop

;;; The lock file is just to indicate that the server is still running,
;;; to be checked by restart-server.

(proclaim '(notinline http-server-init)) ;advisable

(defun http-server (socket port)
  (http-server-init socket port)
  (add-request-handler socket 'handle-next-http-request)
  (run-http-startup-actions)
  ;; Handle requests.
  (with-lock-file (web-tmp-filename "running" "lock")
    (handler-case
        (loop
	  (mapc #'call-request-handler 
		(select-request-sources)))
      ;; Exiting the loop should cause the http server to exit.
      (request-timeout (c)
        (web-note-failure c))
      (condition (c)
        (web-note-failure c)))))

(defun http-server-init (socket port)
  (setq *http-socket* socket)
  (setq *http-port* port)
  (setq *http-startup-date* (http-date-string))

  ;; Note that parameters are recorded in the notes file.
  (set-parameter :http-server-pid (fork:getpid))
  (set-parameter :http-port port)

  ;; Get a new notes file and unique id.
  (when (started-by-cgi-p)
    (close *web-notes*)
    (generate-web-demo-id))

  ;; Disassociate from old stdin, stdout, and stderr.  (If we don't,
  ;; the client may wait for more CGI output, since we'd still be
  ;; keeping the fd open.)
  ;; /\/: We don't _have_ to do stderr on FreeBSD, but I'm not sure
  ;; what would happen to any output.
  (when (started-by-cgi-p)
    (let ((null (fork:stream-fd (open "/dev/null" :direction :io)))
	  (log (fork:stream-fd *web-notes*)))
      (dotimes (i #+freebsd 2 #-freebsd 3)
        (fdclose i)
	(if (= i 0)
	    (fork:dup2 null i)		;stdin
	  (fork:dup2 log i)))))		;stdout and stderr

  ;; Ignore SIGPIPE
  ; (ignore-sigpipe)

  ;; Enable SIGPIPE noting
  (note-sigpipes)
  (assert (not (sigpipe-p)))

  ;; Make our query-args and path-info available via the usual means.
  (advice-replace 'get-query-arg-string 'http-server
    #'http-get-query-arg-string)
  (advice-replace 'get-path-info 'http-server
    #'http-get-path-info)

  ;; Note the configuration as it now stands.
  (record-http-configuration *web-notes*))

(defun record-http-configuration (stream)
  (let ((*print-pretty* t))
    (format stream "~&;;; HTTP configuration~%")
    (dolist (p *http-config-parameters*)
      (print `(setq ,p ,(symbol-value p)) stream))
    (format stream "~%~%")
    (finish-output stream)))


;;;; Startup actions

;;; Once we've (so far as we know) successfully started as a server,
;;; but before we process any requests, we perform any startup actions
;;; specified as functions in the list *http-startup-actions*.
;;; Functions should be added to that list only by calling
;;; add-http-startup-action.

(defun add-http-startup-action (fn)
  (nconcf1 *http-startup-actions* fn))

;;; Since the startup actions run before any requests are handled, 
;;; there won't be a user to whom we can send any error message if
;;; something goes wrong.  We try to deal with this by signalling an
;;; error (about the earlier problem) when we get the first request.
;;; Of course, the user who issued that request may not know what
;;; to do with this information.

(defun run-http-startup-actions ()
  (handler-case (mapc #'funcall *http-startup-actions*)
    (condition (c)
      (web-note "~&Condition before 1st request: ~S~%~%" c)
      (advice+ 'call-http-request-handler 'early-error
        #'(lambda (previous)
	    (declare (ignore previous))
            #'(lambda (fn path request reply-io)
		(declare (ignore fn path request reply-io))
                (advice- 'call-http-request-handler 'early-error)
		(error "Problem at startup: ~A" c)))))))


;;;; Request handlers

;;; The HTTP server normally listens to a single socket and handles
;;; any requests from there.  However, in some cases there may be
;;; other input sources; and so we need to be able to wait for
;;; any of several sources to have input ready.

;;; /\/: We may not be quite set up, at present, to handle multiple
;;; sources of HTTP requests, because of globally-set variables such
;;; as *http-socket*, and perhaps for other reasons.

(defvar *request-handlers* nil)		;a-list of (source . fn) pairs

(defun add-request-handler (source fn)
  (nconcf *request-handlers* (list (cons source fn))))

(defun select-request-sources ()
  (let ((sources (mapcar #'car *request-handlers*)))
    (or (call-real-select sources *http-request-timeout*)
	(signal-request-timeout))))
  
(defun find-request-handler (source)
  (or (cdr (assoc source *request-handlers*))
      (error "No request handler for ~S." source)))

(defun call-request-handler (source)
  (funcall (find-request-handler source) source))

(defun signal-request-timeout ()
  (signal-error 'request-timeout
    :format-string "No request in ~S seconds."
    :format-arguments (list *http-request-timeout*)))


;;;; HTTP requests

;;; /\/: Do we really want both the client address and the address string?

(defstruct http-request
  (method nil)			;keyword: one of :get, :post
  (uri nil)			;string, from the request line
  (http-version nil)		;string, from the request line
  (simple-p nil)		;t/f, t when the req line does not incl version
  (client-address nil)		;int, from get-socket-peer-address
  (client-address-string nil)   ;n.n.n.n string, converted from the address int
  (content-length nil)		;int, from the request headers
  (header-alist '())		;((keyword . string) ...)
  (body-content nil))		;string, from the body of post requests

;;; The condition objects include the status.  (The value given in
;;; the definition is never overridden.)  The reason-phrase can then
;;; be derived from the status.

(defcond http-request-error (simple-error)
  (request nil)			;http-request struct
  (status nil))			;int

(defcond bad-http-request
    (http-request-error (status 400)))

(defcond http-request-forbidden
    (http-request-error (status 403)))

(defcond http-request-uri-not-found
    (http-request-error (status 404)))

(defcond internal-http-server-error
    (http-request-error (status 500))
  (condition nil))			;the internal error

(defcond http-request-not-implemented
    (http-request-error (status 501)))

(defun http-request-error (r error-type &rest initargs)
  (apply #'signal-error error-type :request r initargs))

;;; Status values

(defparameter *http-status-alist*	;HTTP 1.0
  '((200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (204 . "No Content")
    (301 . "Moved Permanently")
    (302 . "Moved Temporarily")
    (304 . "Not Modified")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")))

(defun http-status-reason-phrase (status)
  (or (lookup status *http-status-alist*)
      (error "Unknown HTTP status value ~S." status)))


;;;; HTTP request handler

;;; /\/: We bind *html-out* to a non-nil value (1) to make it more likely
;;; that we'll get a clear error message if something tries to output to
;;; the non-stream, and (2) because nil would allow the HTML code to install
;;; a default stream.

;;; /\/: http-connection-broken is deliberately ambiguous between
;;; "closed by the client" and plain "isn't working", since we don't
;;; really know what's wrong.

(define-condition http-connection-broken (simple-error) ())

(defvar *http-reply-content-type* nil)

(defvar *http-io* 'not-an-http-io) ;/\/ *http-request-io* ?

(defun handle-next-http-request (socket)
  (web-note "~%---~70,1,30,'-A~%~%" (http-date-string))
  (let* ((io (socket-connection-stream socket))
	 (*http-reply-content-type* nil)
	 (*html-out* 'not-an-html-out)
	 (*in-html* nil)
	 (*http-io* io))
    (handler-case
	(let ((r (read-http-request io)))
	  (reply-to-http-request r io)
	  (check-for-sigpipe io))
      (http-connection-broken (c)
	(setq *http-io* 'broken-http-io)
	(web-note-condition c))
      (http-request-error (c)
	(report-http-problem c io))
      (error (c)
	(report-http-problem
	  (make-internal-http-server-error-from-condition c)
	  io)))
    (web-note "~&---Finished ~A~%~%" (http-date-string))
    (close-connection io)
    (clear-sigpipe-note)
    (check-children-status)))

(defun check-for-sigpipe (io)
  (when (sigpipe-p)
    (web-note "~&Had a sigpipe~%~%")
    (unless (fork:stream-ferror (fork:deref-stream io :output))
      (web-note "~&But no error on the io stream.~%~%"))
    t))

(defun close-connection (io)
  (mapc #'close (fork:unpack-stream io)) ;gcl may already close recursively /\/
  (close io))

(defun make-internal-http-server-error-from-condition (c)
 (make-condition 'internal-http-server-error 
   :condition c
   :format-string "The error was: ~A"
   :format-arguments (list c)))

;;; Report-http-problem

;;; Report-http-problem figures out what to do in cooperation with
;;; handle-next-http-request and send-http-response-headers.

;;; If *http-io* is a stream, assume we can write to it.
;;; If *http-reply-content-type* is not nil, assume it's too late to
;;;   send headers, and hence too late to send a status other than whatever
;;;   we've already sent.
;;; If *http-reply-content-type* is "text/plain" or "text/html",
;;;   it may still make sense to send an error message.

(defun report-http-problem (condition connection-stream)
  ;; Make a note in the notes file.
  (web-note-condition condition)
  ;; See what we can send to the client.
  (handler-case
      (cond ((not (streamp connection-stream))
	     ;; Can't send anything
	     )
	    ((null *http-reply-content-type*)
	     ;; Looks like we can send status and headers
	     ;; as well as an error message.
	     (send-http-rejection condition connection-stream))
	    ((string-equal "text/html"  *http-reply-content-type*)
	     ;; Can't send headers, but maybe we can still send
	     ;; an error message.  /\/: We assume HTML output
	     ;; will go to the connection-stream.
	     (html-report-condition condition))
	    ((string-equal "text/plain" *http-reply-content-type*)
	     ;; Can't send headers, but maybe we can still send
	     ;; an error message.
	     (format connection-stream "~&~%Error: ~S~%~%" condition)))
    (error (c)
      (web-note "~&Error when reporting http problem:~%~S~%~%" c))))

(defun send-http-rejection (condition stream)
  (let* ((status-code (http-request-error-status condition))
	 (reason-phrase (http-status-reason-phrase status-code))
	 (fail-string
	  (format nil "Request rejected: ~S ~A~%(~A)"
		  status-code reason-phrase condition)))
    (web-note-failure fail-string)
    ;; Status line and headers
    (send-http-response-headers nil stream
      :status-code status-code
      :reason-phrase reason-phrase
      :content-type "text/plain")
    ;; Body
    (output (:stream stream)
      fail-string //)
    ;; Ensure output sent                  /\/ ? Do we need this ?
    (finish-output stream)))

(defun web-note-condition (c)
  (web-note "~&Condition: ~S~%~%" c))


;;;; Reading http requests

;;; Clients can complain about "connection broken by peer" if we don't
;;; read the (whole) request, so we try to read put off signalling errors
;;; until after we've finished reading it.

;;; Requests are accepted only from *http-client-addresses*.  However,
;;; it is possible to change this by redefining valid-client-address-p.

(proclaim '(notinline valid-client-address-p))

(defun read-http-request (stream)
  (let ((r (read-http-request-line stream)))
    (record-http-client-address r stream)
    (unless (http-request-simple-p r)
      (read-http-request-headers r stream)
      (read-http-request-body r stream))
    (note-http-request r)
    (cond ((not (member (http-request-method r) '(:get :post)))
	   (http-request-error r
	     'http-request-not-implemented
	     :format-string "Method \"~A\" not implemented"
	     :format-arguments (list (http-request-method r))))
	  ((not (implies (http-request-simple-p r)
			 (eq (http-request-method r) :get)))
	   (http-request-error r
	     'bad-http-request
	     :format-string "Simple request that's not method = GET."))
	  ((not (valid-client-address-p r))
	   (http-request-error r
	     'http-request-forbidden
	     :format-string "Not a valid client host"))
	  (t
	   r))))

(defun valid-client-address-p (request)
  (or (null *http-client-addresses*)
      (member (http-request-client-address request) *http-client-addresses*)))

;;; Reading the request line

(defun read-http-request-line (stream)
  (let* ((request-line (string-trim-final-cr (read-line stream)))
	 (parts (break-string-at #\space request-line)))
    (web-note "~&Request line: ~S~%~%" request-line)
    (make-http-request
      :method (string->keyword (first parts))
      :uri (second parts)
      :http-version (third parts)
      :simple-p (null (third parts)))))

;;; Getting the client's internet address (as an int)

(defun record-http-client-address (r stream)
  (let* ((fd (fork:stream-fd (fork:deref-stream stream :input)))
	 (addr (get-socket-peer-address fd)))
    (setf (http-request-client-address r)
	  addr)
    (setf (http-request-client-address-string r)
	  (address-to-string addr))
    r))

;;; Reading request headers

;;; /\/: The headers are read in a simpleminded fashion without any
;;; recognition of comma-separated list elements, comments, or double-quotes.
;;; Everything after colon-whitespace is taken as a single string without
;;; any further parsing.  Field-name-specific parsing could be applied later,
;;; if desired.  If a field name appears more than once, the value in the
;;; header-alist will be a list of the values obtained from the separate lines.

;;; /\/: Continuations lines are handled, but I've never seen one in
;;; an actual request from a browser, so it's hard to be sure they're
;;; handled correctly.

(defun read-http-request-headers (r stream)
  (let ((table (make-hash-table :test #'eq)))
    (loop
      (let ((line (read-http-header-line stream)))
        (when (http-blank-input-line-p line)
	  (return))
	;; HTTP-header = field-name ":" [ field-value ] CRLF
	(let* ((parts (multiple-value-list (break-string-at-first #\: line)))
	       (name (string->keyword (first parts)))
	       (value
		 (string-left-trim '(#\space #\tab)
		   (string-trim-final-cr (second parts)))))
	  (multiple-value-bind (val val-p) (gethash name table)
	    ;; If there's already one value, we make a list of values.
	    ;; If there's alreay a list, we add to the end.
	    (setf (gethash name table)
		  (if val-p
		      (if (consp val) 
			  (nconc val (list value))
			(list val value))
		    value))))))
    ;; We now have a hash table that holds the header values,
    ;; but we want an a-list, e.g. for printing.  So ...
    (setf (http-request-header-alist r)
	  (hash-table-alist table))
    ;; There may now be a content-length.
    (let ((cl (gethash :content-length table)))
      (when cl
	(setf (http-request-content-length r) (string->int cl))))
    ;; Return the modified request struct.
    r))

(defun read-http-header-line (stream)	;handles continuation lines
  (let* ((line1 (read-line stream))
	 (line line1)
	 (continuations '()))
    ;; Read any continuation lines.
    (while (and (not (http-blank-input-line-p line))
		(member (peek-char nil stream) '(#\tab #\space)))
      (setq line (read-line stream))
      (push line continuations))
    (if (null continuations)
	line1
      ;; Create one string out of line1 and the continuations.
      ;; Spaces and tabs are trimmed from the beginning of each
      ;; continuation line, and the final cr is trimmed from the
      ;; end of line1 and the end of each continuation line except
      ;; the last.  The resulting lines are then joined together,
      ;; with a single space as a separator.
      (concat-strings-with-separator " "
	(cons (string-trim-final-cr line1)
	      (reverse
	       (mapcar #'(lambda (c) (string-left-trim '(#\space #\tab) c))
		       ;; The continuations are backwards, so that the last
		       ;; is first, because they were collected using push.
		       (cons (car continuations)
			     (mapcar #'string-trim-final-cr
				     (cdr continuations))))))))))

(defun http-blank-input-line-p (string)
  (and (= (length string) 1)
       (char= (char string 0) #\return)))

;;; Reading the request body

;;; It seems that the content-length can legitimately be zero, e.g.
;;; when an HTML form is submitted and all the parameters are unchecked
;;; checkboxes.  In cgi-env.lsp, we arrange for the query-arg-string
;;; to be "" in such cases, and we should probably do the same here,
;;; by making the http-request-body-content be "" (rather than nil).

(defun read-http-request-body (r stream)
  (let ((len (http-request-content-length r)))
    (when (and (numberp len) (>= len 0))
      (let ((body (read-chars-to-string stream len)))
	(if body
	    (setf (http-request-body-content r) body)
	  (signal-bad-http-request r
	    :format-string "Incomplete or missing body."))))
    r))

;;; Request logging in the notes file

;;; The request-line should already have been logged.

(defparameter *http-request-accessors-to-compare-when-logging*
  '(http-request-simple-p
    http-request-client-address
    http-request-client-address-string
    http-request-header-alist))

(defparameter *always-note-http-request-headers* nil)

(defvar *last-noted-http-request* nil)

(defun note-http-request (r)
  (let ((previous-request *last-noted-http-request*)
	(*print-pretty* t))
    (setq *last-noted-http-request* r)
    (if (null previous-request)
	(web-note "~&1st Request:~%~S~%~%" r)
      (let ((len (http-request-content-length r))
	    (note-p nil))
	(when len
	  (web-note "~&Content-length: ~S (and is ~S).~%"
		    len (length (http-request-body-content r)))
	  (setq note-p t))
	(dolist (accessor *http-request-accessors-to-compare-when-logging*)
	  (let ((then (funcall accessor previous-request))
		(now (funcall accessor r)))
	    (when (or (not (equal then now))
		      (and *always-note-http-request-headers*
			   (eq accessor 'http-request-header-alist)))
	      (web-note "~&~S =~%  ~S~%" accessor now)
	      (setq note-p t))))
	(when note-p
	  (web-note "~%"))))))


;;;; Replies to HTTP requests

;;; /\/: The lhs of an interpretation ends in "/" when we want to make
;;; sure we match a whole word, not just an initial segment.  This has
;;; the side-effect that the requests must contain that "/".  If the
;;; lhs were instead a list of parts (e.g. as returned by break-string-at),
;;; the final "/" would not be required, and we could require that the
;;; whole path end in "/" by using a list such as ("log" "").

;;; Interpretations may be added at run-time.  O-Plan's HTTP server
;;; Web demos typically add ("/Demo-Name"/" . http-demo-action), with
;;; "Demo-Name" replaced by the actual demo name.

;;; The interpretation functions may refer to (at least) the following
;;; variables:
;;;   *http-request*         The request being answered.
;;;   *http-io*              The stream connected to the client.

(defparameter *http-uri-interpretations*
  '(; ("/tf/"   . http-show-tf)
    ; ("/act/"  . http-action)
    ("/log/"  . http-show-notes-file)
    ; ("/goto/" . http-redirection-test)
    ; ("/file/" . http-show-file)
    ("/exit/" . http-exit)))

(defun add-http-uri-interpretation (root fn)
  (nconcf *http-uri-interpretations*
	  (list (cons root fn))))

(defun remove-http-uri-interpretation (root)
  (assert (member root *http-uri-interpretations* :test #'string=))
  (removef root *http-uri-interpretations* :test #'string=))

(defvar *http-request* nil)

(defvar *http-request-path* nil)
(defvar *http-request-query-args* nil)

;;; The path is decoded here, but not the query-args.

(proclaim '(notinline call-http-request-handler))

(defun reply-to-http-request (r stream)
  (setq *http-request* r)
  (multiple-value-bind (path query-args)
      (break-string-at-first #\? (http-request-uri r))
    (setq path (decode-query-string path))
    (setq *http-request-path* path)
    (setq *http-request-query-args* query-args)
    (setq *query-arg-string* nil)		;clear cache
    (dolist (i *http-uri-interpretations* 
	       (signal-http-request-uri-not-found r))
      (let ((root (car i))
	    (fn (cdr i)))
	(when (sequence-begins root path)
	  (return
	    ;; /\/: What args _should_ we pass?  Should they include
	    ;; the query-args?  Should we bind special vars instead?
	    (call-http-request-handler
	      fn
	      (sequence-after root path)
	      r
	      stream)))))))

(defun call-http-request-handler (fn path request reply-io)
  ;; N.B. This function is redefinable.
  (funcall fn path request reply-io))

;;; It's possible to "observe errors" that occur while an http request
;;; is being handled.  Since errors are (usually) recorded anyway in
;;; the log file, this is not especially useful unless it's arranged
;;; for a backtrace to be available, by using with-calltrace, which
;;; in turn requires that the suspect code be run interpreted.

(defun observe-errors-during-http-request-handling ()
  (sys:use-fast-links nil)
  (advice-replace 'call-http-request-handler 'observe-errors
    #'error-observing-http-request-handler))

(defun error-observing-http-request-handler (fn path request reply-io)
  (handler-bind ((error #'observe-any-error))
    (funcall fn path request reply-io)))

(defun observe-any-error (c)
  ;; So ... an error has occurred.
  (web-note "~&Observed condition ~S~%~%" c)
  (when (calltrace-p)
    (calltrace))
  (web-note "~%")
  ;; Decline to handle the condition.
  nil)

;;; Not found

(defun signal-http-request-uri-not-found (r)
  (http-request-error r 'http-request-uri-not-found
    :format-string "Can't find ~S."
    :format-arguments (list (break-string-at-first #\? (http-request-uri r)))))

#+:undef
(defun reply-to-http-request (r stream)
  (send-http-response-headers r stream)
  (output (:stream stream)
       "<HTML>"
    // "<HEAD>"
    // "<TITLE>Hi, there!</TITLE>"
    // "</HEAD>"
    // "<BODY>"
    // "At least it's something"
    // "</BODY>"
    // "</HTML>"
    //))


;;; Query-args

;;; #'http-get-query-arg-string becomes the definition of get-query-arg-string
;;; when the server is initialized.  This makes our query args available
;;; via the mechanisms used for CGI requests.

;;; /\/: What if there's both a "?" in the URI and some body-content?

;;; /\/: If there are no args, should the value be nil or ""?

(defun http-get-query-arg-string ()
  (let ((args (ecase (http-request-method *http-request*)
	        (:get  *http-request-query-args*)
		(:post (http-request-body-content *http-request*)))))
    (when (> (length args) 0)
      (web-note "~&Query-argument values:~%")
      (dolist (assign (break-string-at #\& args))
        (web-note "  ~A~%" assign))
      (web-note "~%"))
    args))


;;; PATH_INFO

;;; #'http-get-path-info becomes the definition of get-path-info when
;;; the server is initialized.  This makes our path-info available via
;;; the mechanisms used for CGI requests.

;;; However, unlike query-args, the path-info is not made available
;;; automatically.  Instead, uri-interpretation fns have to explicitly
;;; call http-set-path-info.  This gives us more flexibility about how
;;; path-info is derived.

(defvar *http-path-info* nil)

(defun http-get-path-info ()
  *http-path-info*)

(defun http-set-path-info (string-or-list)
  (web-note "~&Setting path-info from ~S~%~%" string-or-list)
  (setq *http-path-info*
    (etypecase string-or-list
      (list
       string-or-list)
      (string
       (if (string= string-or-list "")
	   nil
	 (let ((parts (break-string-at #\/ string-or-list)))
	   (if (sequence-begins "/" string-or-list)
	       (cdr parts)
	     parts)))))))


;;; CRLF output

(defvar crlf (coerce '(#\return #\newline) 'string))

(define-output-expander :crlf ()
  `(write-string crlf *output*))


;;; Response status-line and headers

;;; Response routines must send headers before sending anything else,
;;; and the headers must be sent by send-http-response-headers or by
;;; some other routine that cooperates in the same way with handle-next-
;;; http-request and report-http-problem.

(defun send-http-response-headers
    (r stream &rest args
              &key (output-function 'standard-http-header-output)
                   (status-code 200)
                   (reason-phrase (http-status-reason-phrase status-code))
		   (content-type "text/html")
		   (no-cache nil)
		   (headers nil)
		   &allow-other-keys)
  (web-note "~&Sending: ~S ~S, content-type: ~S, no-cache: ~S~%~%"
	    status-code reason-phrase content-type no-cache)
  (when headers
    (web-note "~&Extra headers: ~{~%~S~}~%~%" headers))
  (unless (and r (http-request-simple-p r))
    (handler-case
        (apply output-function stream (remove-prop args :output-function))
      (error (c)
	(web-note "~&Error outputting headers: ~S~%~%" c)
	(signal-error 'http-connection-broken))))

  ;; See if the above output seemed to work.
  ;; [An error is not always signalled: e.g. GCL doesn't seem
  ;; to notice if the client has closed the connection.]
  (when (check-for-sigpipe stream)
    (signal-error 'http-connection-broken))

  ;; We have now sent the headers, which means it is too late to send
  ;; any different headers, and hence too late to send a different status.
  ;; The following assignments will (among other things) make it clear to
  ;; report-http-problem that we are now in such a state.
  (setq *http-reply-content-type* content-type)
  (when (string-equal content-type "text/html")
    ;; Content will be HTML, so we should let the HTML output routines work.
    (setq *html-out* stream
	  *in-html* t)))

(defun standard-http-header-output
    (stream &key (status-code 200)
                 (reason-phrase (http-status-reason-phrase status-code))
		 (content-type "text/html")
		 (no-cache nil)
		 (headers nil))
  (output (:stream stream)
    ;; Status line
    "HTTP/1.0 " status-code " " reason-phrase (:crlf)
    ;; Headers
    "Date: " (http-date-string)    (:crlf)
    "Server: O-Plan/3.1"           (:crlf)
    "Connection: close"            (:crlf) 		;HTTP 1.1
    "Content-type: " content-type  (:crlf)
    (:include
     (when no-cache
       (output
         "Cache-Control: no-cache"       (:crlf)	;HTTP 1.1
	 "Expires: " *http-startup-date* (:crlf)))) 	;already expired
    (:include
     (when headers
       (dolist (h headers)
	 (output (car h) ": " (cdr h) (:crlf)))))
    (:crlf)
    ;; Body will follow.
    ))

(defun http-date-string ()
  ;; An RFC 1123 date representing the current GMT time.
  ;; For example: Sun, 06 Nov 1994 08:49:37 GMT.
  (multiple-value-bind (sec min hr day month year day-of-week dst-p time-zone)
      (decode-universal-time (get-universal-time) 0)
    (assert (not dst-p))
    (assert (= time-zone 0))
    (format nil "~:(~A, ~2D ~A~) ~4D ~2,'0D:~2,'0D:~2,'0D GMT"
       (subseq (int->day day-of-week) 0 3)
       day
       (subseq (int->month month) 0 3)
       year
       hr
       min
       sec)))


;;; Redirection

;;; Let's us test redirection.

(defun http-redirection-test (name r stream)
  (send-http-redirection-response r stream :to name))

(defun send-http-redirection-response (r stream &key to (status-code 301))
  (send-http-response-headers r stream
    :status-code status-code
    :headers `(("Location" . ,to)))
  ;; A body to say the same thing
  (html-block "html"
    (html-block "head"
      (html-tag-line "title" "Moved"))
    (html-block "body"
      "Your browser seems not to understand redirection,"
      "So you will have to " (html-anchor to "do it yourself") ".")))


;;; Log-file requests

;;; Very useful when debugging, but we have to make sure the log file
;;; doesn't contain any secrets (or else disable such requests) when
;;; dealing with users from the great wide world outside.

(defun http-show-notes-file (name r stream)
  (unless (string= name "")
    (http-request-error r 'http-request-forbidden))
  (assert (web-note-p))
  (with-open-file (f *web-notes* :direction :input)
    (send-http-response-headers r stream
      :content-type "text/plain"
      :no-cache t)
    (send-file-contents f stream)))


;;; Exit requests

(defun http-exit (name r stream)
  (declare (ignore name r stream))
  (exit-web-demo))


;;; File requests

;;; Figure out the content type, for the headers, then transmit the
;;; contents.

(defparameter *http-file-root* nil)	;defaults if nil -- see below

(defparameter *http-file-content-types*
  '(("html" . "text/html")
    ("gif"  . "image/gif")
    ("jpg"  . "image/jpg")
    ("ps"   . "application/postscript")))

(defparameter *http-default-file-content-type*
  "text/plain")

(defun http-show-file (name r stream)
  (web-note "~&Looking for file ~S~%~%" name)
  (ensuref *http-file-root*
	   (let ((root (web-demo-filename "")))
	     (web-note "~&Setting *http-file-root* to ~S~%~%" root)
	     root))
  (assert (eql (last-elt *http-file-root*) #\/))
  (let* ((safename (or (normalize-filename name)
		       (signal-http-request-uri-not-found r)))
	 (fullname (concat-string *http-file-root* safename)))
    (cond ((not (file-exists-p fullname))
	   (signal-http-request-uri-not-found r))
	  ((directory-name-p fullname)
	   (send-http-response-headers r stream :content-type "text/plain")
	   (send-directory-contents fullname stream))
	  (t
	   (with-open-file (f fullname :direction :input)
	     (send-http-response-headers r stream
	       :content-type (file-name->content-type name))
	     (send-file-contents f stream))))))

(defun file-exists-p (name)
  ;; Let's hope this, unlike probe-file, doesn't get the truename.  /\/
  (open name :direction :probe))

(defun directory-name-p (name)
  ;; /\/: This trick works in Unix.
  (file-exists-p (concat-string name "/.")))

(defun file-name->content-type (name)
  (multiple-value-bind (basename type) (break-string-at-last #\. name)
    (declare (ignore basename))
    (or (cdr (assoc type *http-file-content-types* :test #'string-equal))
	*http-default-file-content-type*)))

(defun normalize-filename (filename) ; -> string or nil
  ;; Removes . and .. from filenames.
  ;; Returns nil if ".."s would take us before the beginning of the name.
  (let ((in (break-string-at #\/ filename))
	(out '()))
    (dolist (name in (concat-strings-with-separator "/" (nreverse out)))
      (cond ((string= name "..")
	     (if out
		 (pop out)
	       (return nil)))		;a .. too far
	    ((string= name ".")
	     )
	    (t
	     (push name out))))))
    

;;; File and directory contents

;;; The contents are sent by a fork/exec of "cat" or "ls".  This is not
;;; as crazy as it may seem.  It allows us to get to the next request
;;; sooner, something that ordinary servers accomplish by having the
;;; server fork for each request; and in any case "cat" may well be able
;;; to transmit the contents faster than we could.  "cat" can be very
;;; spohisticated, for instance by mapping the file into memory.

(defun send-file-contents (source destination)
  (finish-output destination)
  (let ((s-fd (fork:stream-fd (fork:deref-stream source :input)))
	(d-fd (fork:stream-fd (fork:deref-stream destination :output))))
    (fork-to-exec '("/bin/cat")
      #'(lambda ()
	  ;; Exec prep
	  (fork:dup2 s-fd 0)		;stdin
	  (fork:dup2 d-fd 1)))))	;stdout

(defun send-directory-contents (directory-name destination)
  (check-type directory-name string)
  (finish-output destination)
  (let ((d-fd (fork:stream-fd (fork:deref-stream destination :output))))
    (fork-to-exec `("/bin/ls" "-l" ,directory-name)
      #'(lambda ()
	  ;; Exec prep
	  (fork:dup2 d-fd 1)))))	;stdout


;;;; Useful procedures of various sorts

;;; Fork / exec supervision

;;; These procedures record what's executed in the notes file and
;;; manage the collection of status values from child processes,
;;; without which, in Unix, the children do not fully terminate.
;;; Check-children-status should be called "often enough" so that
;;; child processes do not hang around "too long" after exiting.

(defvar *child-pids* '())

(defun fork-to-exec (exec-args exec-prep)
  (web-note "~&Running~{ ~S~}~%~%" exec-args)
  (let ((pid (fork:fork)))
    (cond (pid
	   ;; Parent
	   (push pid *child-pids*)
	   (web-note "~&pid = ~S~%~%" pid)
	   pid)
	  (t
	   ;; Child
	   (funcall exec-prep)
	   (apply #'fork:execlp exec-args)))))

(defun record-child-pid (pid)
  (push pid *child-pids*))

(defun check-children-status ()
  (flet ((check-child (pid)
	   (let* ((status (fork:waitpid-no-hang pid))
		  (status-pid (car status))
		  (status-value (cdr status)))
	     (case status-pid
	       (-1			; Error
		(web-note "~&Child ~S status error = ~S~%~%"
			  pid status-value)
		(list pid))		;  keep for later checking anyway
	       (0			; Status not yet available
		(list pid))		;  so keep for later checking
	       (t
		;; Have a status
		(assert (= status-pid pid))
		(web-note "~&Child ~S status = ~S~%~%"
			  pid status-value)
		;; Eliminate pid from the list
		nil)))))
     (setq *child-pids*
	   (mapcan #'check-child
		   *child-pids*))))


;;; Host-name lookup

;;; /\/: This should probably be a general CGI utility.

;;; The address can be either an integer or a string containing a
;;; number expressed in Internet "."-notation.  If it is an integer,
;;; and is not in the table, it is converted to "."-notation and
;;; looked up again.  This is because the CGI gives us addresses
;;; in "."-notation, and we want to put them into the table without
;;; converting them to integers.  (This reduces the number of C
;;; routines we need to be able to call, since that's such a pain
;;; on Solaris.)

;;; /\/: For now, we won't call get-host-by-address, because it
;;; doesn't work on Solaris, and because we normally don't need
;;; to -- because CGI gives us a REMOTE_HOST when it gives us
;;; a REMOTE_ADDR.  (This requires extra work for "join"s.)

(defvar *addr-to-host-table* (make-hash-table :test #'equal))

(defun get-host-name (addr)
  (ensuref (gethash addr *addr-to-host-table*)
    (or (have-host-name-p addr)
	; (get-host-by-address addr)
	(etypecase addr
	  (integer (address-to-string addr))
	  (string  addr)))))

(defun have-host-name-p (addr) ; -> hostname or nil
  (etypecase addr
    (string  (gethash addr *addr-to-host-table*))
    (integer (or (gethash addr *addr-to-host-table*)
		 (gethash (address-to-string addr) *addr-to-host-table*)))))

(defun record-address-to-host (addr host-name)
  (web-note "~&Recording address ~S to host ~S~%~%" addr host-name)
  (let ((already (have-host-name-p addr)))
    (assert (implies already (string-equal already host-name))))
  (etypecase addr
    (string
     (setf (gethash addr *addr-to-host-table*) host-name))
    (integer
     (setf (gethash addr *addr-to-host-table*) host-name
	   (gethash (address-to-string addr) *addr-to-host-table*)
	     host-name))))


;;; Lists, etc.

(defun remove-prop (plist propname)
  (do ((p plist (cddr p)))
      ((null p) plist)
    (when (and (consp p) (eq (car p) propname))
      (return
        (append (ldiff plist p) (cddr p))))))


;;; String and I/O utilities

(defparameter *char-time-limit* (* 2 60)) ;for network delays ...

(defun read-chars-to-string (stream len)
  (let ((s (make-string len)))
    (dotimes (i len s)
      (let ((char ;; Trust no one.
	     (or (read-char-no-hang stream)
		 (and (real-select-p (list stream) *char-time-limit*)
		      (read-char-no-hang stream)))))
	(if char
	    (setf (schar s i) char)
	  (return nil))))))

(defun string-trim-final-cr (string)
  (let ((len (length string)))
    (unless (and (>= len 1) (char= (char string (1- len)) #\return))
      (error "String ~S does not end in a CR." string))
    (subseq string 0 (1- len))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
