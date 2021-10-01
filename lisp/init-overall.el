;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Unclassified general settings.

;;; Code:

;; Always use the ISO C date format.

(setq system-time-locale "C")

;; Persoanl information.

(setq user-full-name "Eric Zhuo Chen"
      user-mail-address "zyxirchen@outlook.com")

;; The location of Zybox, my all-in-one file center.
;; This value should be manually set in init-local.el, as it is different on
;; different machines.

(defvar zy/zybox-path nil
  "The path of Zybox, the collection of all my files.")

;; If Zybox is not set in init-local.el, try to guess one based on `system-type'.

(unless zy/zybox-path
  (require 'cl-extra)
  (let* ((zybox-possible-locs-win64
	  '("C:\\Zybox"
	    "C:\\Users\\zyxir\\Documents\\Zybox"))
	 (zybox-possible-locs-linux
	  '("~/Zybox"
	    "~/Documents/Zybox"))
	 (guessed-zybox-path
	  (cl-some
	   (lambda (path)
	     (when (file-directory-p path)
	       path))
	   (cond
	    (*win64* zybox-possible-locs-win64)
	    (*linux* zybox-possible-locs-linux)))))
    (if guessed-zybox-path
	(progn
	  (setq zy/zybox-path (file-truename guessed-zybox-path))
	  (warn
	   "Zybox should be manually set instead of auto detected!"))
      (warn "No Zybox path is detected, many features will be unavailable!"))))

;; End of config.

(provide 'init-overall)
