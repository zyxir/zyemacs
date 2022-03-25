;;; zy-block.el --- configurable code blocks.


;; Keyword management.

(defvar zb-keyword-list nil
  "List of available keywords for zy-block.")

(defvar zb-default-keyword-list nil
  "List of keywords that is enabled by default for zy-block.")

(defvar zb-kwfunc-plist nil
  "List of keywords and their wrapper cuntions for zy-block.")

(defvar zb-kwtype-plist nil
  "List of keywords and their types for zy-block.")

;;;###autoload
(defun zb-define-keyword (keyword type func &optional default)
  "Define a new keyword for zy-block.

KEYWORD is the keyword, and TYPE decides what type of arguments
the keyword should receive:

- 'single'	a single expression.
- 'multiple'	multiple expressions.
- 't' 'nil'	zero or one expression, with 't' or 'nil' as
		the default value.

FUNC is the wrapper function for the keyword.

If DEFAULT is non-nil, the keyword is enabled by default."
  (add-to-list 'zb-keyword-list keyword)
  (setq zb-kwtype-plist (plist-put zb-kwtype-plist keyword type))
  (setq zb-kwfunc-plist (plist-put zb-kwfunc-plist keyword func))
  (when default
    (add-to-list 'zb-default-keyword-list keyword))
  keyword)

;;;###autoload
(defmacro zb (name &rest body)
  "Define a block of code as a zy-block."
  (declare (indent 1) (debug (form def-body)))
  (dolist (kw zb-default-keyword-list)
    (let ((kwfunc (plist-get zb-kwfunc-plist kw)))
      (setq body (apply kwfunc name body))))
  `(prog1 ',name ,@body))


;; Benchmark.

(defvar zb-benchmark-result nil
  "Result of benchmarking, with each element being a (NAME . TIME)
pair, where NAME is the zy-block name, and TIME is the time used
to execute it.")

;;;###autoload
(defun zb-wrapper-benchmark (name &rest body)
  "Wrap BODY with benchmarking code.

The code append (NAME . TIME) to `zb-benchmark-result', where
TIME is the time used to execute the body."
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
		   'append))))


;; Protect.

;;;###autoload
(defun zb-wrapper-protect (name &rest body)
  "Wrap BODY with protecting code.

The code prevent any warning or error from stopping the whole
configuration from executing. Warnings or errors issued inside
BODY will be reported by the zy-block."
  `((condition-case --ex--
	,(if (cdr body) (append '(progn) body) (car body))
      ('warning
       (lwarn 'zb :warning "In %s: %s" ',name --ex--))
      ('error
       (lwarn 'zb :error "In %s: %s" ',name --ex--)))))


;; Keyword setup.

;;;###autoload
(defun zb-setup ()
  "Setup the zy-block system."
  (zb-define-keyword ':protect 't #'zb-wrapper-protect 'default)
  (zb-define-keyword ':benchmark 't #'zb-wrapper-benchmark 'default))
