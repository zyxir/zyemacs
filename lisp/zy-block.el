;;; zy-block.el --- configurable code blocks.

(require 'cl-lib)


;; Keyword management.

(defvar zb-keyword-list nil
  "List of available keywords for zy-block.")

(defvar zb-keyword-type-plist nil
  "List of keywords and their types for zy-block.")

(defvar zb-keyword-func-plist nil
  "List of keywords and their wrapper cuntions for zy-block.")

(defvar zb-flag-list nil
  "List of all flag keywords for zy-block.")

(defvar zb-global-flag-alist nil
  "Alist of globally enabled flags and their default values.

These flag keywords will be enabled for every zy-blocks, even if
they are not explicitly given. However, if a global flag is
explicitly given, the given value will be used instead of the
default value.")

(defun zb-define-keyword (keyword type func &optional fsetup)
  "Define a new keyword KEYWORD for zy-block.

KEYWORD is the keyword, which is a symbol starts with the
comma (:) sign, like ':when' or ':pkg'.

TYPE is the type of the keyword, determing the way `zb' parses
its argument. Possible types are:

'single' -- the keyword reads a single s-expression as its
argument.

'multiple' -- the keyword reads multiple s-expressions, which are
then stored in a list to form its argument.

'flag' -- the keyword is a flag that reads zero or one
s-expression as its argument. If no s-expression is given, its
argument would be 't'. Additionally, flag keywords can be
globally enabled by adding them and their default values to
`zb-global-flag-alist'.

FUNC is the wrapper function for the keyword.

Optionally, if function FSETUP is non-nil, run the function to
setup the keyword."
  (cl-pushnew keyword zb-keyword-list)
  (when (equal type 'flag)
    (cl-pushnew keyword zb-flag-list))
  (setq zb-keyword-type-plist
	(plist-put zb-keyword-type-plist keyword type)
	zb-keyword-func-plist
	(plist-put zb-keyword-func-plist keyword func))
  (when fsetup (funcall fsetup))
  keyword)

(defun zb-keyword-p (sexp)
  "Determine if SEXP is a valid keyword.

If it is, return its type. Otherwise, return nil.

Additionally, if SEXP is '--eol--', which is used by zb-parse to
indicate the end of the parsed list, the function will return
itself."
  (if (equal sexp '--eol--)
      sexp
    (when (and (symbolp sexp)
	       (equal (substring (symbol-name sexp) 0 1) ":")
	       (member sexp zb-keyword-list))
      (plist-get zb-keyword-type-plist sexp))))


;; Debugging utilities.

(defun zb-warn (name level message &rest args)
  "Display a zy-block warning message.

NAME is the name of the current zy-block name.

All other arguments correspond to those of `lwarn'."
  (apply #'lwarn
	 (format "zb %s" name)
	 level
	 message
	 args))


;; Keyword parsing.

(defun zb-parse (name body)
  "Parse BODY, return two plists (KPLIST FPLIST).

The first plist KPLIST is of non-flag keywords and their
arguments. If there are objects that do not belong to any
keyword, they will be stored with the keyword 'nil'.

The second plist FPLIST is of flag keywords and their arguments.

NAME is just used for proper warning display."
  (let* (kplist				; plist of non-flags
	 fplist				; plist of flags
	 (body
	  (append body '(--eol--)))	; append '--eol--' to body
	 curkey				; current keyword
	 curkwp				; (zb-keyword-p curkey)
	 curarg				; current argument
	 newobj				; new object to parse
	 newkwp				; (zb-keyword-p newobj)
	 (kwcnt 0))			; keyword counter
    (while body
      ;; Get a new sexp to parse.
      (setq newobj (car body)
	    newkwp (zb-keyword-p newobj)
	    body (cdr body))
      ;; Parsing argument based on the current keyword.
      (cond
       ;; If there is no active keyword, but the new object is still
       ;; not a keyword, collect the object as a free sexp.
       ((and (not curkwp) (not newkwp))
	(push newobj curarg))
       ;; If there is no active keyword, and the new object is a valid
       ;; keyword, store all free sexp.
       ((and (not curkwp) newkwp)
	(when curarg
	  (push (nreverse curarg) kplist)
	  (push nil kplist)))
       ;; If there is an active 'multiple' typed keyword, and the new
       ;; object is not a keyword, just collect the new object as ar
       ;; part of its argument.
       ((and (equal curkwp 'multiple) (not newkwp))
	(push newobj curarg))
       ;; If there is an active 'multiple' typed keyword, and the new
       ;; object is an valid keyword, stop the parsing of the current
       ;; keyword.
       ((and (equal curkwp 'multiple) newkwp)
	(push (nreverse curarg) kplist)
	(push curkey kplist))
       ;; If there is an active 'single' typed keyword, and the new
       ;; object is not a keyword, store the object as its argument.
       ((and (equal curkwp 'single) (not newkwp))
	(push newobj kplist)
	(push curkey kplist))
       ;; If there is an active 'single' typed keyword, and the new
       ;; object is an valid keyword, ignore the current keyword, and
       ;; issue an warning, as no argument has been passed to the
       ;; current keyword.
       ((and (equal curkwp 'single) newkwp)
	(zb-warn name :warning
		 "%dth keyword %s ignored."
		 kwcnt curkey))
       ;; If there is an active 'flag' typed keyword, and the new
       ;; object is not a keyword, store the object as its argument.
       ((and (equal curkwp 'flag) (not newkwp))
	(push newobj fplist)
	(push curkey fplist))
       ;; If there is an active 'flag' typed keyword, and the new
       ;; object is a valid keyword, store the flag as 't'.
       ((and (equal curkwp 'flag) newkwp)
	(push t fplist)
	(push curkey fplist))
       ;; For other circumstances, only issue an warning.
       (t
	(zb-warn name :warning
		 "unknown circumstance met at %dth keyword %s."
		 kwcnt curkey)))
      ;; Change the parsing state.
      (if newkwp
	  (setq curkey newobj
		curkwp newkwp
		curarg nil)
	(when (or (equal curkwp 'single)
		  (equal curkwp 'flag))
	  (setq curkey nil
		curkwp nil
		curarg nil)))
      ;; Increment keyword counter if necessary.
      (when newkwp
	(setq kwcnt (+ kwcnt 1))))
    ;; Return the two parsed plists.
    `(,kplist ,fplist)))


;; The `zb' macro.

(defmacro zb (name &rest body)
  "Define a block of code as a zy-block."
  (declare (indent 1))
  (let* ((parse-result (zb-parse name body))
	 (kplist (car parse-result))
	 (fplist (cadr parse-result))
	 keyword
	 func
	 arg
	 body)
    ;; Apply non-flag keyword functions.
    (while kplist
      (setq keyword (car kplist)
	    arg (cadr kplist)
	    kplist (cddr kplist)
	    body
	    (if keyword
		(funcall (plist-get zb-keyword-func-plist
				    keyword)
			 name arg body)
	      (append arg body))))
    ;; Add global flag keywords.
    (dolist (fd zb-global-flag-alist)
      (let ((flag (car fd))
	    (default (cdr fd)))
	(when (not (member flag fplist))
	  (push default fplist)
	  (push flag fplist))))
    ;; Apply flag keywords in order.
    (dolist (flag zb-flag-list)
      (when (member flag fplist)
	(setq arg (plist-get fplist flag)
	      body
	      (funcall (plist-get zb-keyword-func-plist
				  flag)
		       name arg body))))
    ;; Construct the final body.
    (cond
     ((symbolp body)
      `(prog1 ',name ,body))
     ((not (cdr body))
      `(prog1 ',name ,(car body)))
     (t
      (append `(prog1 ',name) body)))))


(provide 'zy-block)

;;; end of zy-block.el
