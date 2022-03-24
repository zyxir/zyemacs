;;; zy-macros.el --- ZyEmacs macros.


;; Lambda-free macros.

;;;###autoload
(defmacro add-hook! (hook &rest body)
  "Add a lambda function of BODY to the value of HOOK."
  (declare (indent 1) (debug (form def-body)))
  `(add-hook ,hook (lambda () ,@body)))

;;;###autoload
(defmacro run-with-idle-timer! (secs &rest body)
  "Execute BODY after being idle for SECS seconds."
  (declare (indent 1) (debug (form def-body)))
  `(run-with-idle-timer ,secs nil (lambda () ,@body)))

;; Eval after several features are all ready.

;;;###autoload
(defmacro with-eval-after-features! (features &rest body)
  "Execute BODY after everything in FEATURES is loaded.

FEATURES is a list. Each element of FEATURES is a feature name or
a file name."
  (declare (indent 1) (debug (form def-body)))
  (setq features (cadr features))
  (dolist (feature features)
    (setq body `(with-eval-after-load ,feature ,body)))
  body)
