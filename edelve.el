;; -*- lexical-binding: t; -*-
;; ## Debugging notes
;; Call (edelve--reset-state) to reset everything.
;;
;; ## Implementation notes
;;
;; ### JSON RPC
;;
;; There is builtin (require 'jsonrpc). But it sends Content-Length
;; header, which dlv doesn't support. Hence we just open a socket and
;; send json directly.
;;
;; ### How to debug dlv itself
;; dlv debug --headless --api-version=2 --log --log-output=debugger,dap,rpc --listen=127.0.0.1:8181

;; TODO: json-serialize
;; TODO: https://github.com/go-delve/delve/blob/master/Documentation/api/ClientHowto.md
;; TODO: Introduce edelve-minor-mode
;; TODO: Use LastModified call to check that breakpoint line numbers are still good
;; TODO: Use https://github.com/aarzilli/delve_client_testing

(require 'cl-lib)
(require 'seq)
(require 'map)

(defface edelve-breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'edelve)

(defcustom edelve-dlv-program "dlv"
  "Path to `dlv' executable")

(defcustom edelve-dlv-args '("debug" "--headless")
  "Arguments to path to the `dlv' executable.")

(defcustom edelve-dlv-remote-addr nil
  "Remote `dlv' instance address.\
This will be used instead of `edelve-dlv-program'. \
Run `dlv' as `dlv debug --headless --listen=127.0.0.1:8181'.
Then set this variable to '127.0.0.1:8181'")

(defcustom edelve-process-output-bufer-name "*edelve-process-output*"
  "Name of a buffer into which your process output will be delivered.")

;;; Internal variables

(defvar edelve--connection nil)
(defvar edelve--jsonrpc-id 0)

;; TODO: make sure we don't leak anything here!
(defvar edelve--requests nil)
(defvar edelve--breakpoints nil)
(defvar edelve--breakpoint-fringes nil)

(defvar edelve--process nil "dlv process.")

(defvar edelve--process-state nil "Debugging target process.")
(defvar edelve--process-buffer nil)
(defvar edelve--process-stacktrace nil)
(defvar edelve--process-breakpoints nil)
(defvar edelve--process-stack-depth 0)

(defvar edelve--eval-result nil)
(defvar edelve--buffer nil)
(defvar edelve--log-buffer nil)
(defvar edelve--line-fringe nil)

;; TODO: this is used for debugging. Remove me
(defvar edelve--last-response nil)

(defvar edelve--load-config '((FollowPointers . t)
                              (MaxVariableRecurse . 1)
                              (MaxStringLen . 64)
                              (MaxArrayValues . 64)
                              (MaxStructFields . -1)))

;; See https://pkg.go.dev/reflect#Kind
(defvar edelve--go-kinds [Invalid
                          Bool
                          Int Int8 Int16 Int32 Int64
                          Uint Uint8 Uint16 Uint32 Uint64
                          Uintptr
                          Float32 Float64 Complex64 Complex128
                          Array Chan Func Interface Map Pointer Slice String Struct
                          UnsafePointer])


;;; Public API

(defun edelve ()
  (interactive)
  (unless (json-available-p)
    (error "Sorry, you need to have Emacs build with native JSON support."))

  (edelve--reset-state)

  (edelve--create-buffer edelve--buffer "*edelve*" go-mode) ; TODO: don't depend on go-mode here!!!
  (edelve--create-buffer edelve--log-buffer "*edelve-log*")
  (edelve--create-buffer edelve--process-buffer edelve-process-output-bufer-name edelve-minor-mode)

  (edelve--start-process)

  (edelve--enable-minor-mode-in-project))

(defvar-keymap edelve-minor-mode-map
  :doc "Keymap for edelve-minor-mode."
  "<f5>" #'edelve-restart
  "<f8>" #'edelve-continue
  "S-<f8>" #'edelve-halt
  "<f9>" #'edelve-toggle-breakpoint
  "<f10>" #'edelve-next
  "<f11>" #'edelve-step

  "C-c C-p" #'edelve-print-dwim
  "C-c C-e" #'edelve-eval
  "C-c C-u" #'edelve-up
  "C-c C-d" #'edelve-down

  "C-c C-z" #'edelve-display-dwim

  ;; TODO this doesn't work with (list) in the modestring. Check out how it's done in flymake
  :menu '("Edelve"
          :help "Edelve stuff"
          ["Halt" edelve-halt :help "Halt the execution"]))

(defvar-keymap edelve-minor-mode-repeat-map
  :doc "Keymap to repeat navigation commands.
Used in `repeat-mode'"
  :repeat t
  "C-u" #'edelve-up
  "C-d" #'edelve-down)

;; TODO: enable this mode in supported buffers instead of via the mode hook
(define-minor-mode edelve-minor-mode
  "Minor mode for go buffers that can interract with `dlv'."
  :init-value nil
  :lighter nil ;; (:eval (edelve-modeline-string))
  :keymap edelve-minor-mode-map)

(add-to-list 'mode-line-misc-info `(edelve-minor-mode (" [" (:eval (edelve-modeline-string)) "]")))

(defun edelve-modeline-string ()
  (list "edlv:"
        (if-let (state (edelve--get-process-state))
            (propertize (symbol-name state) 'face (pcase state ;; TODO: introduce out own inherited fonts
                                                    ('stop 'warning)
                                                    ('run 'success)))
          "-")))

(defun edelve-quit ()
  (interactive)
  (when edelve--connection
    (edelve--ensure-halted)
    (edelve--send "RPCServer.Detach"))
  (edelve--reset-state))

;; Commands

(defun edelve-restart ()
  (interactive)
  (edelve--ensure-halted)
  ;; TODO: Customize rebuild
  (setq edelve--process-stack-depth 0)
  (edelve--send "RPCServer.Restart" '((Rebuild . t)) #'edelve-continue)
  (display-buffer edelve--process-buffer))

(defun edelve-continue ()
  (interactive)
  (edelve--ensure)
  (unless (edelve--process-running-p)
    (edelve--send "RPCServer.Command"
                  `((name . "continue") (ReturnInfoLoadConfig . ,edelve--load-config)))
    ;; Response to "continue" will arrive only after we halt the
    ;; target.  Hence we request the state with a nonblocking flag.
    (edelve--request-state)))

(defun edelve-halt ()
  (interactive)
  (edelve--ensure)
  (edelve--send "RPCServer.Command" '((name . "halt") (ReturnInfoLoadConfig . :null))))

(defun edelve-next ()
  (interactive)
  (edelve--when-not-running
   (edelve--send "RPCServer.Command" '((name . "next")))))

(defun edelve-step ()
  (interactive)
  (edelve--when-not-running
    (edelve--send "RPCServer.Command" '((name . "step")))))

(defun edelve-up ()
  (interactive)
  (edelve--when-not-running
    (edelve--send "RPCServer.Stacktrace" `((Id . -1) (Depth . ,(cl-incf edelve--process-stack-depth))))))

(defun edelve-down ()
  (interactive)
  (edelve--when-not-running
    (edelve--send "RPCServer.Stacktrace" `((Id . -1) (Depth . ,(cl-decf edelve--process-stack-depth))))))

(defun edelve-toggle-breakpoint (&optional location)
  "LOCATION should be (file . line)"
  (interactive)
  (edelve--ensure)
  (let* ((file (if location (car location) (buffer-file-name)))
         (line (if location (cdr location) (line-number-at-pos)))
         (loc (format "%s:%d" file line))
         (action (if-let* ((breakpoint (edelve--get-breakpoint file line)))
                     (lambda ()
                       (edelve--log "Clearing breakpoint at %s" loc)
                       (edelve--clear-breakpoint (map-elt breakpoint 'id)))
                   (lambda ()
                     (edelve--log "Setting breakpoint at %s" loc)
                     (edelve--create-breakpoint loc)))))
    (if (eq (edelve--get-process-state) 'stop)
        (funcall action)
      (edelve-halt)
      (funcall action)
      (edelve-continue))))

(defun edelve-eval (&optional expr)
  (interactive "sExpr: ")
  (edelve--when-not-running
   (if expr
       (edelve--eval expr)
     (user-error "Cannot evaluate empty expression"))))

(defun edelve-print-dwim ()
  (interactive)
  (edelve--when-not-running
   (if (region-active-p)
       (let ((region-string (buffer-substring-no-properties (region-beginning) (region-end))))
         (edelve-eval region-string))
     (let ((thing (thing-at-point 'sexp)))
       (edelve-eval thing)))))

(defun edelve-display-dwim ()
  (interactive)
  (if (eq (current-buffer) edelve--process-buffer)
      (next-buffer) ;; TODO make something better here
    (if (buffer-live-p edelve--process-buffer)
        (display-buffer edelve--process-buffer)
      (edelve--log "No live process"))))


;;; UI stuff

(defvar edelve-ui-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?d] 'process-menu-delete-process)
    map))

(defun edelve-ui-breakpoints-clear-breakpoint ()
  (interactive)
  (let ((pos (point)))
    (message "TODO: %s" (tabulated-list-get-id))
    (revert-buffer)
    (goto-char (min pos (point-max)))
    (if (eobp)
        (forward-line -1)
      (beginning-of-line))))

(define-derived-mode edelve-ui-breakpoints-mode tabulated-list-mode "Breakpoints"
  "Major mode for listing edelve breakpoints."
  (setq tabulated-list-format [("ID" 4 t)
			       ("Location" 0 t)])
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook 'edelve-ui-breakpoints--refresh nil t))

(defun edelve-ui-breakpoints ()
  (interactive)
  (let ((buffer (get-buffer-create "*edelve-breakpoints*")))
    (with-current-buffer  buffer
      (edelve-ui-breakpoints-mode)
      (edelve-ui-breakpoints--refresh)
      (tabulated-list-print))

    (display-buffer buffer)))

(defun edelve-ui-breakpoints--refresh ()
  "Update breakpoints."
  (setq tabulated-list-entries nil)
  (seq-do (lambda (breakpoint)
            (let ((id (map-elt breakpoint 'id))
                  (file (map-elt breakpoint 'file))
                  (line (map-elt breakpoint 'line)))
              (when (> id 0)
                (push (list id (vector (format "%d" id) (format "%s:%s" file line)))
                      tabulated-list-entries))))
           edelve--process-breakpoints)
  (tabulated-list-init-header))


;;; Private stuff

(defun edelve--reset-state ()
  ;; TODO: remove our overlays!
  ;; (remove-overlays)

  (when (process-live-p edelve--connection)
    (delete-process edelve--connection))

  (setq edelve--connection nil)

  (when (process-live-p edelve--process)
    (delete-process edelve--process))

  (setq edelve--process nil)

  (setq edelve--requests (make-hash-table))
  (setq edelve--breakpoints (make-hash-table))
  (setq edelve--breakpoint-fringes (make-hash-table))

  (edelve--kill-buffer edelve--process-buffer)
  (edelve--kill-buffer edelve--log-buffer)
  (edelve--kill-buffer edelve--buffer)

  (setq edelve--process-state nil)

  (setq edelve--process-stacktrace nil)
  (setq edelve--process-breakpoints nil)
  (setq edelve--last-response nil)
  (setq edelve--eval-result nil)

  (setq edelve--jsonrpc-id 0))

(defun edelve--enable-minor-mode-in-project (&optional project)
  "Enabled `edelve-minor-mode' for all go buffers in the PROJECT."
  ;; TODO: Make configurable (including which modes to enable minor mode in)
  (unless project
    (setq project (project-current)))
  (when project
    (dolist (buffer (project-buffers project))
      (with-current-buffer buffer
        (when (seq-some #'derived-mode-p '(go-mode go-ts-mode))
          (edelve--trace "Enabling edelve-minor-mode in %s" (buffer-name))
          (edelve-minor-mode))))))

(defun edelve--start-process ()
  (cl-assert (null edelve--process))
  (if edelve-dlv-remote-addr
      (edelve--process-connect edelve-dlv-remote-addr)
      (setq edelve--process (make-process :name "dlv"
                                          :buffer edelve--process-buffer
                                          :command (append (list edelve-dlv-program) edelve-dlv-args)
                                          :filter #'edelve--process-startup-filter
                                          :sentinel #'edelve--process-sentinel))))

(defun edelve--process-startup-filter (process input-string)
  (edelve--trace "`dlv' filter: %s" input-string)
  (set-process-filter process nil)
  (pcase input-string
    ((rx "listening at: " (group (* any)))
     (let ((addr (match-string 1 input-string)))
       (edelve--log "listening at %s" addr)
       (edelve--process-connect addr)))
    ((rx "cannot find main module")
     (edelve--error "Cannot find go module to debug. \
Please ensure that you are running (edelve) within a go project directory."))
    (_ (edelve--error "Cannot connect to `dlv' because it didn't specify which port it is listening on."))))

(defun edelve--process-sentinel (process event)
  (edelve--trace "`dlv' sentinel: %s" event)
  (pcase event
    ("killed\n" (edelve--log "`dlv' process was killed")
     (edelve--reset-state))
    ((rx "exited abnormally with code " (group (? digit)))
     (let ((code (match-string 1 event)))
       (when edelve--connection
           (edelve--error "`dlv' process exited with code %s" code))
       (edelve-quit)))))

(defun edelve--process-connect (addr)
  (pcase (split-string addr ":")
    (`(,host ,port)

     (setq edelve--connection (open-network-stream "edelve" nil host port))
     (set-process-filter edelve--connection #'edelve--connection-filter)

     (edelve--send "RPCServer.SetApiVersion" '((APIVersion . 2)))
     (edelve--request-state))
    (_ (edelve--error "cannot parse dlv addr '%s'" addr)
       (edelve-quit))))

(defmacro edelve--create-buffer (variable name &optional mode)
  `(progn
     (setq ,variable (get-buffer-create ,name))
     (with-current-buffer ,variable
       (erase-buffer)
       ,(when mode (list mode)))))

(defmacro edelve--kill-buffer (buffer)
  `(when (buffer-live-p ,buffer)
     (kill-buffer ,buffer)
     (setq buffer nil)))

;; Connection related things

(defun edelve--connection-filter (process input-string)
  ;; NOTE: Write nicely formatted data into a buffer for debugging
  (when nil
    (with-current-buffer (get-buffer-create "*edelve-debug*")
      (erase-buffer)
      (js-json-mode)
      (insert input-string)
      (json-pretty-print-buffer)))

  ;; NOTE: we can get multiple strings here (one as a response for our
  ;; request, and a notification for the status change)
  ;; TODO: Is there a way to get a substring without making a new string?
  (dolist (response-string (string-split input-string "\n" t))
    (let* ((response (json-parse-string response-string :object-type 'alist))
           (id (map-elt response 'id)))
      (if (eq id :null)
          (edelve--log "Got notification from the server (id: %d)" id)
        (pcase-let ((`(,method ,callback) (map-elt edelve--requests id)))
          (edelve--trace "Got response with id %d for method %s" id method)
          (map-delete edelve--requests id)
          (edelve--handle-response method response)
          (when callback
            (funcall callback))))

      (setq edelve--last-response response))))

(defmacro edelve--when-not-running (&rest body)
  `(progn
     (edelve--ensure)
     (if (edelve--process-running-p)
         (message "Process is running. Use (edelv-halt) to stop it.")
       ,@body)))

(defun edelve--send (method &optional params callback)
  ;; NOTE dlv uses jsonrpc 1.0
  (let ((data (json-serialize `((method . ,method)
                                (params . [,params])
                                (id  . ,(cl-incf edelve--jsonrpc-id))))))
    (edelve--trace "Sending %s" data)
    (map-put! edelve--requests edelve--jsonrpc-id (list method callback))
    (process-send-string edelve--connection data)))

(defun edelve--handle-response (method response)
  (let ((result (map-elt response 'result))
        (err (map-elt response 'error)))
    (if (eq err :null)
        (pcase method
          ("RPCServer.SetApiVersion")
          ((or "RPCServer.State" "RPCServer.Command") (edelve--handle-state (map-elt result 'State)))
          ("RPCServer.Stacktrace" (edelve--handle-stacktrace (map-elt result 'Locations)))
          ("RPCServer.ListBreakpoints" (setq edelve--process-breakpoints (map-elt result 'Breakpoints)))
          ("RPCServer.Eval" (edelve--handle-eval-result result)) ;; TODO: unify with the others
          ("RPCServer.CreateBreakpoint" (edelve--handle-create-breakpoint (map-elt result 'Breakpoint)))
          ("RPCServer.ClearBreakpoint" (edelve--handle-clear-breakpoint (map-elt result 'Breakpoint)))
          ("RPCServer.Restart")
          (_ (edelve--log "no handler for method %s" method)))
      (user-error "edelve: method %s failed: %s" method err))))

(defun edelve--handle-state (state)
  (setq edelve--process-state state)
  (pcase (edelve--get-process-state)
    ('run (when edelve--line-fringe (delete-overlay edelve--line-fringe)))
    ('stop (edelve--jump-to-current-line))))

(defun edelve--handle-eval-result (result)
  (setq edelve--eval-result (map-elt result 'Variable))
  (with-current-buffer edelve--buffer
    (erase-buffer)
    (insert (with-output-to-string (edelve--pp-variable edelve--eval-result)))
    (display-buffer edelve--buffer 'display-buffer-reuse-window)))

(defun edelve--handle-stacktrace (stacktrace)
  (setq edelve--process-stacktrace stacktrace)
  (let ((last-location (aref edelve--process-stacktrace (1- (length edelve--process-stacktrace)))))
    (edelve--jump-to-current-line last-location)))

(defun edelve--handle-create-breakpoint (breakpoint)
  (edelve--request-breakpoints)
  (let ((id (map-elt breakpoint 'id))
        (fringe (make-overlay (line-beginning-position) (line-beginning-position 2))))
    (map-put! edelve--breakpoint-fringes id fringe)
    (map-put! edelve--breakpoints id breakpoint)
    (overlay-put fringe 'before-string
                 (propertize ">" 'display (list 'left-fringe 'large-circle 'edelve-breakpoint-enabled)))))

(defun edelve--handle-clear-breakpoint (breakpoint)
  (edelve--request-breakpoints)
  (let ((id (map-elt breakpoint 'id)))
    (delete-overlay (map-elt edelve--breakpoint-fringes id))
    (setq edelve--breakpoint-fringes (map-delete edelve--breakpoint-fringes id))
    (setq edelve--breakpoints (map-delete edelve--breakpoints id ))))

;; Commands

(defun edelve--ensure ()
  (unless edelve--connection
    (user-error "Edelve is not running. Try `M-x edelve'")))

(defun edelve--ensure-halted ()
  (edelve--ensure)
  (when (edelve--process-running-p)
    (edelve-halt)))

(defun edelve--request-state ()
  "Request program state even the program is running"
  (edelve--send "RPCServer.State" '((NonBlocking . t))))

(defun edelve--request-stacktrace ()
  (edelve--send "RPCServer.Stacktrace" `((Id . ,(edelve--get-current-goroutine-id))
                                         (Depth . 50)
                                         (MaxVariableRecurse . 1)
                                         (MaxStringLen . 64)
                                         (MaxArrayValues . 64))))

(defun edelve--eval (expr)
  (edelve--send "RPCServer.Eval" `((Scope . ((GoroutineID . -1) (Frame . ,edelve--process-stack-depth)))
                                   (Expr . ,expr)
                                   (Cfg . ,edelve--load-config))))

(defun edelve--jump-to-current-line (&optional location)
  (unless location
    (setq location (map-nested-elt edelve--process-state '(currentGoroutine userCurrentLoc))))
  (when location
    (let ((buffer (find-file-noselect (map-elt location 'file))))
      (display-buffer buffer '(display-buffer-reuse-window))
      (with-current-buffer buffer
        (goto-line (map-elt location 'line) buffer)
        (back-to-indentation)

        (unless edelve--line-fringe
          (setq edelve--line-fringe (make-overlay (line-beginning-position) (line-beginning-position 2)))
          (overlay-put edelve--line-fringe 'before-string
                       (propertize ">" 'display '(left-fringe right-triangle))))

        (move-overlay edelve--line-fringe (line-beginning-position) (line-beginning-position 2))))))

(defun edelve--create-breakpoint (location)
  "Create a breakpoint at LOCATION.
NOTE: process must *not* be running for this to work.
Halt the process first to set a breakpoint.
"
  (edelve--send "RPCServer.CreateBreakpoint" `((LocExpr . ,location))))

(defun edelve--clear-breakpoint (id)
  (edelve--send "RPCServer.ClearBreakpoint" `((Id . ,id))))

(defun edelve--request-breakpoints ()
  (edelve--send "RPCServer.ListBreakpoints"))

;; Dissecting state

(defun edelve--get-current-goroutine-id ()
  ;; TODO: use SelectedGoroutine here!
  (map-nested-elt edelve--process-state '(currentThread goroutineID)))

(defun edelve--process-running-p ()
  (eq (edelve--get-process-state) 'run))

(defun edelve--get-process-state ()
  (if edelve--process-state
      (let ((running (map-elt edelve--process-state 'Running)))
        (if (eq running :false) 'stop 'run))
    nil))

(defun edelve--get-breakpoint (file line)
  (seq-find (lambda (frame)
              (and (equal file (map-elt frame 'file))
                   (equal line (map-elt frame 'line))))
            edelve--process-breakpoints))

;; Pretty printing

(defun edelve--pp-state ()
  edelve--process-state)

(defun edelve--pp-stacktrace ()
  ;; TODO: Figure out if we can get working directory and strip it
  ;; from a filename.
  (princ "Stacktrace:\n")
  (seq-do
   (lambda (frame)
     (princ (format "%s(...)\n    %s:%d\n"
                    (map-nested-elt frame '(function name))
                    (map-elt frame 'file)
                    (map-elt frame 'line))))
   edelve--process-stacktrace)
  nil)

(defun edelve--pp-breakpoints ()
  (princ "Breakpoints:\n")
  (seq-do
   (lambda (frame)
     (let ((id (map-elt frame 'id)))
       (when (> id 0)
         (princ (format "[%d] %s:%d (in %s())\n"
                        id
                        (map-elt frame 'file)
                        (map-elt frame 'line)
                        (map-elt frame 'functionName))))))
   edelve--process-breakpoints)
  nil)

(defun edelve--pp-where ()
  (let ((loc (map-nested-elt edelve--process-state '(currentGoroutine userCurrentLoc))))
    (princ (format "%s:%d (in %s())"
                   (map-elt loc 'file)
                   (map-elt loc 'line)
                   (map-nested-elt loc '(function name)))))
  nil)

(defun edelve--pp-eval-result ()
  (edelve--pp-variable edelve--eval-result))

(defun edelve--pp-variable (variable &optional stream depth is-pointer)
  (unless depth (setq depth 0))
  (let ((kind (aref edelve--go-kinds (map-elt variable 'kind)))
        (name (map-elt variable 'name))
        (type (map-elt variable 'type))
        (value (map-elt variable 'value))
        (children (map-elt variable 'children))
        (indent (lambda ()
                  (princ (format (concat "%" (number-to-string (* 4 depth)) "s") "") stream))))
    (unless is-pointer
      (funcall indent))
    (pcase kind
      ('Pointer (if (equal value "0")
                    (princ (format "%s %s: nil\n" name type) stream)
                  (princ (format "%s: *" name) stream)
                  (edelve--pp-variable (seq-elt (map-elt variable 'children) 0) stream depth t)))
      ('Struct (unless (or is-pointer (string-empty-p name))
                 (princ (format "%s: " name) stream))
               (if (zerop (length children))
                   (princ (format "%s {}\n" type) stream)
                 (princ (format "%s {\n" type) stream)
                 (seq-do (lambda (child)
                           (edelve--pp-variable child stream (1+ depth)))
                         children)
                 (funcall indent)
                 (princ "}\n" stream)))
      ('Slice (unless (string-empty-p name)
                (princ (format "%s: " name) stream))
              (princ (format "%s: " type) stream)
              (if (zerop (map-elt variable 'len))
                  (princ "[]" stream)
                (princ "[\n")
                (seq-do (lambda (child)
                          (edelve--pp-variable child stream (1+ depth)))
                        children)
                (funcall indent)
                (princ "]\n")))
      ('String
       (princ (format "%s: \"%s\"\n" name value) stream))
      ((or 'Bool 'Uint 'Int 'Float32 'Float64)
       (princ (format "%s: %s\n" name value) stream))
      (_ ; (edelve--log "kind is not supported %s" kind)
         (princ (format "%s: %s (xxx)\n" name type) stream))))
  nil)

;; Utils

(defun edelve--error (fmt &rest args)
  (message "edelve error: %s" (apply #'format fmt args)))

(defun edelve--log (fmt &rest args)
  (message "edelve: %s" (apply #'format fmt args)))

(defun edelve--trace (fmt &rest args)
  (with-current-buffer edelve--log-buffer
    (insert (apply #'format fmt args) "\n")))


;;; Debug stuff

;; (edelve--send "RPCServer.GetVersion" '())
;; (edelve--send "RPCServer.IsMulticlient" '())
;; (edelve--send "RPCServer.Recorded" '())

;; (edelve--send "RPCServer.FindLocation" '((Scope . ((GoroutineID . -1)
;;                                                    (Frame . 0)
;;                                                    (DeferredCall . 0)))
;;                                          (Loc . "main.go:549")
;;                                          (IncludeNonExecutableLines . :false)
;;                                          (SubstitutePathRules . :null)))

;; RPCServer.ListLocalVars(rpc2.ListLocalVarsIn{"Scope":{"GoroutineID":22,"Frame":0,"DeferredCall":0},"Cfg":{"FollowPointers":true,"MaxVariableRecurse":1,"MaxStringLen":64,"MaxArrayValues":64,"MaxStructFields":-1}})
;; RPCServer.ListFunctionArgs(rpc2.ListFunctionArgsIn{"Scope":{"GoroutineID":22,"Frame":0,"DeferredCall":0},"Cfg":{"FollowPointers":true,"MaxVariableRecurse":1,"MaxStringLen":64,"MaxArrayValues":64,"MaxStructFields":-1}})

;; RPCServer.FindLocation(rpc2.FindLocationIn{"Scope":{"GoroutineID":-1,"Frame":0,"DeferredCall":0},"Loc":"main.go:549","IncludeNonExecutableLines":false,"SubstitutePathRules":null})

;; RPCServer.LastModified(rpc2.LastModifiedIn{})
;; RPCServer.AttachedToExistingProcess(rpc2.AttachedToExistingProcessIn{})

;; TODO: we get multiple process filter calls with parts of the huge json here. Do something about it.
;; (edelve--send "RPCServer.ListFunctions" '(:Filter "" :FollowCalls 0))

(provide 'edelve)
