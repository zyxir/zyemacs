;;; zy-os.el --- ZyEmacs OS-specific utilities.


;; Path utilities.

(defun zo--path-type (path &optional default)
  "Return the type of PATH.

The algorithm to decide its type is:

1. If PATH starts with '/' or '~', return `unix'.

2. If PATH contains \"\\\", return `win'.

3. If PATH contains '/', return `unix'.

4. Otherwise, return DEFAULT. If DEFAULT is omitted or nil,
return `unix'.

The returned type is not always correct. For example, \".\" is
both a Windows path and a Unix path, but this function would
return `unix'."
  (let (first-char)
    (cond
     ((and (setq first-char (substring path 0 1))
	   (or (equal first-char "/")
	       (equal first-char "~")))
      'unix)
     ((string-match-p "\\\\" path)
      'win)
     ((string-match-p "/" path)
      'unix)
     (t (if default default 'unix)))))

(defun zo--wsl-root ()
  "Get the WSL root path in Windows."
  (unless (boundp 'zo--wsl-root)
    (setq zo--wsl-root
	  (with-temp-buffer
	    (call-process "wslpath" nil (current-buffer) nil
			  "-w" "/")
	    (string-trim-right (buffer-string)))))
  zo--wsl-root)

(defun zo-path-to-win (path)
  "Convert WSL path PATH to a Windows path.

Relative path would be converted to absolute ones."
  (if (equal (zo--path-type path) 'win)
      path
    (let ()
      ;; Convert path to absolute.
      (setq path (file-truename path))
      ;; For files in Windows filesystem.
      (when (equal (string-match-p "/mnt/[c-z]" path) 0)
	(setq path (replace-regexp-in-string
		    "/mnt/\\([c-z]\\)" "\\1:" path))
	(setq path (concat
		    (upcase (substring path 0 1))
		    (substring path 1 nil))))
      ;; Replace Unix home.
      (when (equal (string-match-p "/" path) 0)
	(setq path (concat
		    (zo--wsl-root)
		    (substring path 1 nil))))
      ;; Convert / to \\.
      (setq path (string-replace "/" "\\" path))
      path)))
