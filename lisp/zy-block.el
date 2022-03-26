;;; zy-block.el --- configurable code blocks.

(require 'cl-lib)


;; Keyword management.

(defvar zb-keyword-list nil
  "List of available keywords for zy-block.")

(defvar zb-keyword-type-plist nil
  "List of keywords and their types for zy-block.")

(defvar zb-flag-list nil
  "List of flag keywords for zy-block.")

(defvar zb-flag-default-plist nil
  "List of default values for flag keywords of zy-block.")

(defvar zb-keyword-func-plist nil
  "List of keywords and their wrapper cuntions for zy-block.")

;;;###autoload
(defun zb-define-keyword (keyword type func)
  "Define a new keyword for zy-block.

KEYWORD is the keyword, and TYPE decides what type of arguments
the keyword should receive:

- 'single'	a single expression.
- 'multiple'	multiple expressions.

FUNC is the wrapper function for the keyword."
  (cl-pushnew keyword zb-keyword-list)
  (setq zb-keyword-type-plist
	(plist-put zb-keyword-type-plist keyword type))
  (setq zb-keyword-func-plist
	(plist-put zb-keyword-func-plist keyword func))
  keyword)

(defun zb-define-flag (flag default func)
  "Define a new flag keyword for zy-block.

FLAG is the flag keyword, and DEFAULT is its default value.

FUNC is the wrapper function for the flag keyword."
  (cl-pushnew flag zb-flag-list)
  (setq zb-flag-default-plist
	(plist-put zb-flag-default-plist flag default))
  (setq zb-keyword-func-plist
	(plist-put zb-keyword-func-plist flag func))
  flag)


;; Keyword parsing.

(defun zb-parse--keyword-p (sexp)
  "Determine if SEXP is a keyword.

If it is a valid flag keyword, return 'flag'.

If it is a valid keyword, return its type.

If it is '--zb-eol--', return itself.

Otherwise, return 'nil'."
  (if (equal sexp '--zb-eol--)
      sexp
    (when (and (symbolp sexp)
	       (equal (substring (symbol-name sexp) 0 1) ":"))
      (if (member sexp zb-flag-list)
	  'flag
	(when (member sexp zb-keyword-list)
	  (plist-get zb-keyword-type-plist sexp))))))

(defun zb-parse (body)
  "Parse BODY, return a list of keywords and arguments.

For a 'single' typed keyword, only one argument will be parsed;
for a 'multiple' typed keyword, all argument will be parsed until
the next keyword, or the end of BODY, and the arguments will be
placed inside a list. For a flag keyword, zero or one argument
will be parsed.

If there are objects that do not belong to any keyword, they will
be stored with the key 'nil'."
  (let* (newobj				; new object to parse
	 newkwp				; (zb-parse--keyword-p newobj)
	 curkey				; current keyword
	 curkwp				; (zb-parse--keyword-p curkwp)
	 curarg				; current argument
	 result				; the result list
	 (body (append body '(--zb-eol--))))
    ;; '--zb-eol--' means the end of the list.
    (while body
      (setq newobj (car body)
	    newkwp (zb-parse--keyword-p newobj))
      (cond
       ;; No keyword is active.
       ((not curkwp)
	(if newkwp
	    ;; Save non-argument sexps and start parsing the next
	    ;; keyword.
	    (progn
	      (when curarg
		(push (nreverse curarg) result)
		(push nil result))
	      (setq curkey newobj
		    curkwp newkwp
		    curarg nil))
	  ;; Collect non-argument sexp.
	  (push newobj curarg)))
       ;; A flag keyword is active.
       ((equal curkey 'flag)
	(if newkwp
	    ;; Use the default value for the flag and start parsing
	    ;; the next keyword.
	    (progn
	      (push (plist-get zb-flag-default-plist
			       curkey)
		    result)
	      (push curkey result)
	      (setq curkey newobj
		    curkwp newkwp
		    curarg nil))
	  ;; Use the value
	  ))))))


;; The `zb' macro.

;;;###autoload
(defmacro zb (name &rest body)
  "Define a block of code as a zy-block."
  (declare (indent 1) (debug (form def-body)))
  ;; Parse keywords.
  ;; Apply keywords.
  ;; Apply flag functions.
  (dolist (flag zb-flag-list)
    (let ((func (plist-get zb-keyword-func-plist flag)))
      (when func
	(setq body (funcall func name t body)))))
  `(prog1 ',name ,@body))


;; Benchmark.

(defvar zb-benchmark-result nil
  "Result of benchmarking, with each element being a (NAME . TIME)
pair, where NAME is the zy-block name, and TIME is the time used
to execute it.")

;;;###autoload
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

;;;###autoload
(defun zb-wrapper-protect (name arg body)
  "Wrap BODY with protecting code if ARG is non-nil.

The code prevent any warning or error from stopping the whole
configuration from executing. Warnings or errors issued inside
BODY will be reported by the zy-block."
  (if arg
      `((condition-case --ex--
	    ,(if (cdr body) (append '(progn) body) (car body))
	  ('warning
	   (lwarn 'zb :warning "In %s: %s" ',name --ex--))
	  ('error
	   (lwarn 'zb :error "In %s: %s" ',name --ex--))))
    body))


;; Keyword setup.

;;;###autoload
(defun zb-setup ()
  "Setup the zy-block system."
  (zb-define-flag ':protect t #'zb-wrapper-protect)
  (zb-define-flag ':benchmark t #'zb-wrapper-benchmark))
