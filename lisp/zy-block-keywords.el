;;; zy-block-keywords.el --- keywords definition for zy-blocks.

(require 'zy-block)


;; Benchmark.

(defun zb-setup-benchmark ()
  "Setup the ':benchmark' keyword."
  (defvar zb-benchmark-result nil
    "Result of benchmarking, with each element being a (NAME . TIME)
pair, where NAME is the zy-block name, and TIME is the time used
to execute it."))

(defun zb-wrapper-benchmark (name arg body)
  "Wrap BODY with benchmarking code if ARG is non-nil.

The code append (NAME . TIME) to `zb-benchmark-result', where
TIME is the time used to execute the body."
  (if arg
      `((let ((--time-start-- (current-time))
	      --time-elapsed--
	      --name-time-pair--)
	  ,@body
	  (setq --time-elapsed--
		(float-time (time-since --time-start--)))
	  (add-to-list '--name-time-pair-- --time-elapsed--)
	  (add-to-list '--name-time-pair-- ',name)
	  (add-to-list 'zb-benchmark-result
		       --name-time-pair--
		       'append)))
    body))


;; Protect.

(defun zb-wrapper-protect (name arg body)
  "Wrap BODY with protecting code if ARG is non-nil.

The code prevent any warning or error from stopping the whole
configuration from executing. Warnings or errors issued inside
BODY will be reported by the zy-block."
  (if arg
      `((condition-case --ex--
	    ,(if (cdr body) `(progn ,@body) (car body))
	  ('warning
	   (zb-warn ',name :warning "%s" --ex--))
	  ('error
	   (zb-warn ',name :error "%s" --ex--))))
    body))


;; Provide.

(defun zb-wrapper-provide (name arg body)
  "Provide feature ARG after BODY.

If ARG is t, provide feature NAME."
  (if arg
      (if (equal arg t)
	  (append body `((provide ',name)))
	(append body `((provide ,arg))))
    body))


;; When and unless.

(defun zb-wrapper-when (name arg body)
  "Execute BODY when ARG is non-nil."
  `((when ,arg ,@body)))

(defun zb-wrapper-unless (name arg body)
  "Execute BODY when ARG is nil."
  `((unless ,arg ,@body)))


;; After-load.

(defun zb-wrapper-after-load (name arg body)
  "Execute BODY after ARG is loaded.

ARG is a feature name, or a list of feature names."
  ;; Remove quote in arg.
  (when (equal (car arg) 'quote)
    (setq arg (cadr arg)))
  ;; Wrap body in a single form.
  (if (cdr body)
      (setq body `(progn ,@body))
    (setq body (car body)))
  ;; Wrap body inside `eval-after-load'.
  (if (listp arg)
      (dolist (f (nreverse arg) `(,body))
	(setq body `(eval-after-load ',f ,body)))
    `((eval-after-load ',arg ,body))))


;; Idle.

(defun zb-wrapper-idle (name arg body)
  "Execute BODY after being idle for ARG seconds."
  `((run-with-idle-timer ,arg nil (lambda () ,@body))))


;; Pkg.

(defun zb-wrapper-pkg--recipe-p (sexp)
  "Return t if SEXP is a package recipe."
  (or (symbolp sexp)
      (and (symbolp (cadr sexp))
	   (equal (substring (symbol-name (cadr sexp)) 0 1) ":"))))

(defun zb-wrapper-pkg (name arg body)
  "Execute BODY after package ARG is successfully installed.

ARG is a package recipe, or a list of recipes."
  ;; Remove quote in arg.
  (when (equal (car arg) 'quote)
    (setq arg (cadr arg)))
  ;; Wrap BODY around package conditional.
  (let ((pkgc nil))
    (if (zb-wrapper-pkg--recipe-p arg)
	(setq pkgc `(straight-use-package ',arg))
      (push 'and pkgc)
      (dolist (recipe arg)
	(push `(straight-use-package ',recipe) pkgc))
      (setq pkgc (nreverse pkgc)))
    `((when ,pkgc ,@body))))


;; Keyword setup.

;;;###autoload
(defun zb-setup ()
  "Setup the zy-block system."
  ;; Setup flag keywords in order: the early a flag keyword is
  ;; defined, the outer it will be wrapped.
  (zb-define-keyword ':protect 'flag #'zb-wrapper-protect)
  (zb-define-keyword ':benchmark 'flag #'zb-wrapper-benchmark
		     #'zb-setup-benchmark)
  (zb-define-keyword ':provide 'flag #'zb-wrapper-provide)
  ;; Setup non-flag keywords. The order is not important here.
  (zb-define-keyword ':when 'single #'zb-wrapper-when)
  (zb-define-keyword ':unless 'single #'zb-wrapper-unless)
  (zb-define-keyword ':after-load 'single #'zb-wrapper-after-load)
  (zb-define-keyword ':idle 'single #'zb-wrapper-idle)
  (zb-define-keyword ':pkg 'single #'zb-wrapper-pkg))
