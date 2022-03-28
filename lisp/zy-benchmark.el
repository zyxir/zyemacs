;;; zy-benchmark.el --- Benchmarking utilities of ZyEmacs.

;; This file should be loaded by zy-block.el. Do not load this solely.


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

(defun zbch-zb-wrapper (name arg body)
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

(zb-define-keyword ':benchmark 'flag #'zbch-zb-wrapper
		   :after ':provide)


;; Display benchmark result in a dedicated mode.

(define-derived-mode zbch-time-list-mode
  tabulated-list-mode "Benchmark"
  "Show times taken to execute each zy-block.")


(provide 'zy-benchmark)

;;; end of zy-benchmark.el
