;; -*- lexical-binding: t; -*-
;;
;; Implementation notes
;;
;; # JSON RPC
;;
;; There is builtin (require 'jsonrpc). But it sends Content-Length
;; header, which dlv doesn't support. Hence we just open a socket and
;; send json directly.

;; dlv debug --headless --api-version=2 --log --log-output=debugger,dap,rpc --listen=127.0.0.1:8181

;; TODO: json-serialize
;; TODO: https://github.com/go-delve/delve/blob/master/Documentation/api/ClientHowto.md
;; TODO: Introduce edelve-minor-mode
;; TODO: Use LastModified call to check that breakpoint line numbers are still good
;; TODO: Use https://github.com/aarzilli/delve_client_testing

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'bind-key)

(defface edelve-breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'edelve)

(defvar edelve--connection nil)
(defvar edelve--jsonrpc-id 0)

;; TODO: make sure we don't leak anything here!
(defvar edelve--requests nil)
(defvar edelve--breakpoints nil)
(defvar edelve--breakpoint-fringes nil)

(defvar edelve--process-state nil)
(defvar edelve--process-stacktrace nil)
(defvar edelve--process-breakpoints nil)

(defvar edelve--eval-result nil)
(defvar edelve--buffer nil)
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

  (setq edelve--buffer (get-buffer-create "*edelve*"))

  (with-current-buffer edelve--buffer
    (erase-buffer)
    ;; TODO: don't depend on go-mode here!!!
    (go-mode))

  ;; TODO: use global minor mode
  (setq global-mode-string '(:eval (if-let (state (edelve--get-process-state))
                                       (format "[edlv:%s]" state)
                                     "[edlv]")))

  (setq edelve--connection (open-network-stream "edelve" nil "127.0.0.1" "8181"))

  (set-process-filter edelve--connection #'edelve--connection-filter)

  (edelve--send "RPCServer.SetApiVersion" '((APIVersion . 2)))
  (edelve--request-state))

(defun edelve-setup-default-keymap (&optional map)
  (unless map
    (setq map 'go-mode-map))
  (bind-key "<f5>" #'edelve-restart map)
  (bind-key "<f8>" #'edelve-continue map)
  (bind-key "S-<f8>" #'edelve-halt map)
  (bind-key "<f9>" #'edelve-toggle-breakpoint map)
  (bind-key "<f10>" #'edelve-next map)
  (bind-key "<f11>" #'edelve-step map)
  (bind-key "C-c C-e" #'edelve-eval map)
  (bind-key "C-c C-p" #'edelve-print-dwim map))

(defun edelve-quit ()
  (interactive)
  (edelve--ensure-halted)
  (edelve--send "RPCServer.Detach")
  (edelve--reset-state))

;; Commands

(defun edelve-restart ()
  (interactive)
  (edelve--ensure-halted)
  ;; TODO: Customize rebuild
  (edelve--send "RPCServer.Restart" '((Rebuild . t)))
  (edelve-continue))

(defun edelve-continue ()
  (interactive)
  (edelve--send "RPCServer.Command" `((name . "continue")
                                      (ReturnInfoLoadConfig . ,edelve--load-config)))
  (edelve--request-state))

(defun edelve-halt ()
  (interactive)
  (edelve--send "RPCServer.Command" '((name . "halt") (ReturnInfoLoadConfig . :null))))

(defun edelve-next ()
  (interactive)
  (edelve--send "RPCServer.Command" '((name . "next"))))

(defun edelve-step ()
  (interactive)
  (edelve--send "RPCServer.Command" '((name . "step"))))

(defun edelve-toggle-breakpoint ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (loc (format "%s:%d" file line))
         (action (if-let ((breakpoint (edelve--get-breakpoint file line)))
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
  (if expr
      (edelve--eval expr)
    (user-error "Cannot evaluate empty expression")))

(defun edelve-print-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((region-string (buffer-substring-no-properties (region-beginning) (region-end))))
        (edelve-eval region-string))
    (let ((thing (thing-at-point 'sexp)))
      (edelve-eval thing))))


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

  (setq edelve--requests (make-hash-table))
  (setq edelve--breakpoints (make-hash-table))
  (setq edelve--breakpoint-fringes (make-hash-table))

  (setq edelve--process-state nil)
  (setq edelve--process-stacktrace nil)
  (setq edelve--process-breakpoints nil)
  (setq edelve--last-response nil)
  (setq edelve--eval-result nil)

  (setq edelve--jsonrpc-id 0))

;; Connection related things

(defun edelve--connection-filter (process input-string)
  (with-current-buffer (get-buffer-create "debug.json")
    (erase-buffer)
    (insert input-string)
    (json-pretty-print-buffer))
  ;; NOTE: we can get multiple strings here (one as a response for our
  ;; request, and a notification for the status change)
  ;; TODO: Is there a way to get a substring without making a new string?
  (dolist (response-string (string-split input-string "\n" t))
    (let* ((response (json-parse-string response-string :object-type 'alist))
           (id (map-elt response 'id)))
      (if (eq id :null)
          (edelve--log "Got notification from the server (id: %d)" id)
        (let ((method (map-elt edelve--requests id)))
          (cl-assert method)
          (edelve--trace "Got response with id %d for method %s" id method)
          (map-delete edelve--requests id)
          (edelve--handle-response method response)))

      (setq edelve--last-response response))))

(defun edelve--send (method &optional params)
  ;; NOTE dlv uses jsonrpc 1.0
  (let ((data (json-serialize `((method . ,method)
                                (params . [,params])
                                (id  . ,(cl-incf edelve--jsonrpc-id))))))
    (edelve--trace "Sending %s" data)
    (map-put! edelve--requests edelve--jsonrpc-id method)
    (process-send-string edelve--connection data)))

(defun edelve--handle-response (method response)
  (let ((result (map-elt response 'result))
        (err (map-elt response 'error)))
    (if (eq err :null)
        (pcase method
          ("RPCServer.SetApiVersion")
          ((or "RPCServer.State" "RPCServer.Command") (edelve--handle-state (map-elt result 'State)))
          ("RPCServer.Stacktrace" (setq edelve--process-stacktrace (map-elt result 'Locations)))
          ("RPCServer.ListBreakpoints" (setq edelve--process-breakpoints (map-elt result 'Breakpoints)))
          ("RPCServer.Eval" (edelve--handle-eval-result result)) ;; TODO: unify with the others
          ("RPCServer.CreateBreakpoint" (edelve--handle-create-breakpoint (map-elt result 'Breakpoint)))
          ("RPCServer.ClearBreakpoint" (edelve--handle-clear-breakpoint (map-elt result 'Breakpoint)))
          (_ (edelve--log "no handler for method %s" method)))
      (user-error "edelve: method %s failed: %s" method err))))

(defun edelve--handle-state (state)
  (setq edelve--process-state state)
  (pcase (edelve--get-process-state)
    ('run (delete-overlay edelve--line-fringe))
    ('stop (edelve--jump-to-current-line))))

(defun edelve--handle-eval-result (result)
  (setq edelve--eval-result (map-elt result 'Variable))
  (with-current-buffer edelve--buffer
    (erase-buffer)
    (insert (with-output-to-string (edelve--pp-variable edelve--eval-result)))
    (display-buffer edelve--buffer 'display-buffer-reuse-window)))

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

(defun edelve--ensure-halted ()
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
  (edelve--send "RPCServer.Eval" `((Scope . ((GoroutineID . -1) (Frame . 0)))
                                   (Expr . ,expr)
                                   (Cfg . ,edelve--load-config))))

(defun edelve--jump-to-current-line ()
  (when-let ((loc (map-nested-elt edelve--process-state '(currentGoroutine userCurrentLoc))))
    (let ((buffer (find-file-noselect (map-elt loc 'file))))
      (display-buffer buffer '(display-buffer-reuse-window))
      (with-current-buffer buffer
        (goto-line (map-elt loc 'line) buffer)
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
  (unless is-pointer
    (princ (format (concat "%" (number-to-string (* 4 depth)) "s") "") stream))
  (let ((kind (aref edelve--go-kinds (map-elt variable 'kind))))
    (pcase kind
      ('Pointer (if (equal (map-elt variable 'value) "0")
                    (princ (format "%s %s: nil\n" (map-elt variable 'name) (map-elt variable 'type)) stream)
                  (princ (format "%s: *" (map-elt variable 'name)) stream)
                  (edelve--pp-variable (seq-elt (map-elt variable 'children) 0) stream depth t)))
      ('Struct (let ((children (map-elt variable 'children)))
                 (unless is-pointer
                   (princ (format "%s: " (map-elt variable 'name)) stream))
                 (if (zerop (length children))
                     (princ (format "%s {}\n" (map-elt variable 'type)) stream)
                   (princ (format "%s {\n" (map-elt variable 'type)) stream)
                   (seq-do (lambda (child)
                             (edelve--pp-variable child stream (1+ depth)))
                           children)
                   (princ (format (concat "%" (number-to-string (* 4 depth)) "s") "") stream)
                   (princ "}\n" stream))))
      ('String
       (princ (format "%s: \"%s\"\n" (map-elt variable 'name) (map-elt variable 'value)) stream))
      ((or 'Bool 'Uint)
       (princ (format "%s: %s\n" (map-elt variable 'name) (map-elt variable 'value)) stream))
      (_ ; (edelve--log "kind is not supported %s" kind)
         (princ (format "%s: %s\n" (map-elt variable 'name) (map-elt variable 'type)) stream))))
  nil)

;; Utils

(defun edelve--log (fmt &rest args)
  (message "edelve: %s" (apply #'format fmt args)))

(defun edelve--trace (fmt &rest args)
  (if nil
   (message "edelve: %s" (apply #'format fmt args))))


;;; Debug session

(edelve--send "RPCServer.GetVersion" '())
(edelve--send "RPCServer.IsMulticlient" '())
(edelve--send "RPCServer.Recorded" '())

(edelve--send "RPCServer.FindLocation" '((Scope . ((GoroutineID . -1)
                                                   (Frame . 0)
                                                   (DeferredCall . 0)))
                                         (Loc . "main.go:549")
                                         (IncludeNonExecutableLines . :false)
                                         (SubstitutePathRules . :null)))

;; So, to create a breakpoint while the program is running, we halt
;; it, create a breakpoint and continue


;; RPCServer.Stacktrace(rpc2.StacktraceIn{"Id":1,"Depth":50,"Full":false,"Defers":false,"Opts":0,"Cfg":{"FollowPointers":true,"MaxVariableRecurse":1,"MaxStringLen":64,"MaxArrayValues":64,"MaxStructFields":-1}})

;; RPCServer.ListLocalVars(rpc2.ListLocalVarsIn{"Scope":{"GoroutineID":22,"Frame":0,"DeferredCall":0},"Cfg":{"FollowPointers":true,"MaxVariableRecurse":1,"MaxStringLen":64,"MaxArrayValues":64,"MaxStructFields":-1}})
;; RPCServer.ListFunctionArgs(rpc2.ListFunctionArgsIn{"Scope":{"GoroutineID":22,"Frame":0,"DeferredCall":0},"Cfg":{"FollowPointers":true,"MaxVariableRecurse":1,"MaxStringLen":64,"MaxArrayValues":64,"MaxStructFields":-1}})


;; RPCServer.FindLocation(rpc2.FindLocationIn{"Scope":{"GoroutineID":-1,"Frame":0,"DeferredCall":0},"Loc":"main.go:549","IncludeNonExecutableLines":false,"SubstitutePathRules":null})

;; RPCServer.CreateBreakpoint(rpc2.CreateBreakpointIn{"Breakpoint":{"id":0,"name":"","addr":8760041,"addrs":[8760041],"addrpid":[711336],"file":"","line":0,"ExprString":"","Cond":"","HitCond":"","HitCondPerG":false,"continue":false,"traceReturn":false,"goroutine":false,"stacktrace":0,"LoadArgs":null,"LoadLocals":null,"WatchExpr":"","WatchType":0,"hitCount":null,"totalHitCount":0,"disabled":false,"RootFuncName":"","TraceFollowCalls":0},"LocExpr":"main.go:549","SubstitutePathRules":null,"Suspended":false})


;; RPCServer.LastModified(rpc2.LastModifiedIn{})
;; RPCServer.AttachedToExistingProcess(rpc2.AttachedToExistingProcessIn{})
;; RPCServer.Detach(rpc2.DetachIn{"Kill":true})


;; TODO: we get multiple process filter calls with parts of the huge json here. Do something about it.
(edelve--send "RPCServer.ListFunctions" '(:Filter "" :FollowCalls 0))

;;

(edelve)
(edelve-setup-default-keymap)
(edelve-continue)

(edelve-halt)
(edelve--create-breakpoint "main.go:511")
(edelve-continue)

(edelve--request-breakpoints)
(edelve--pp-breakpoints)

(edelve-next)

(edelve--pp-where)
(edelve--eval "pl")
(edelve--pp-eval-result)

(edelve-quit)

(set-window-fringes (selected-window) 30 0)
