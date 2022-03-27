;;; zy-benchmark.el --- Benchmarking utilities of ZyEmacs.

(require 'zy-block)


;; Definitions.

(defvar zbch-result nil
  "Result of benchmarking, with each element being a (TIME-SINCE
NAME TIME-TAKEN) pair, where TIME-SINCE is the time before
loading the zy-block, NAME is the zy-block name, and TIME-TAKEN
is the time used to execute the zy-block.")


;; Benchmark as a zy-block flag keyword.

(defmacro zbch--time-since (time)
  "Return the time elapsed since TIME.

The result is in milliseconds, and is a string."
  `(format
    "%.2f"
    (* 1000
       (float-time (time-since ,time)))))

(defun zb-wrapper-benchmark (name arg body)
  "Wrap BODY with benchmarking code if ARG is non-nil.

The code append (NAME . TIME) to `zb-benchmark-result', where
TIME is the time used to execute the body."
  (if arg
      `((let ((--time-start-- (current-time))
	      --result--)
	  (add-to-list '--result--
		       (zbch--time-since
			before-init-time))
	  ,@body
	  (add-to-list '--result-- ',name)
	  (add-to-list '--result--
		       (zbch--time-since
			--time-start--))
	  (add-to-list 'zbch-result
		       (nreverse --result--)
		       'append)))
    body))

(zb-define-keyword ':benchmark 'flag #'zb-wrapper-benchmark
		   :after ':provide)


;; Display benchmark result in a dedicated mode.

(define-derived-mode zbch-time-list-mode
  tabulated-list-mode "Benchmark"
  "Show times taken to execute each zy-block.")


(provide 'zy-benchmark)

;;; end of zy-benchmark.el
