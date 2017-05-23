+++
date = "2012-11-22T17:36:00.000000+01:00"
title = "M-x ack"
tags = ["Emacs", "Ack", "Grep"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/11/22-Emacs-Ack-Mode",
           "/blog/2012/11/22-Emacs-Ack-Mode.html"]
+++

I've been asked about how to integrate the 
[ack](http://betterthangrep.com/) tool (you know, the one that
is 
*better than grep*) into Emacs today. Again. And I just realized that I
didn't blog about my solution. That might explain why I keep getting asked
about it after all...

So here it is, 
`M-x ack`:

~~~
;;; dim-ack.el --- Dimitri Fontaine
;;
;; http://stackoverflow.com/questions/2322389/ack-does-not-work-when-run-from-grep-find-in-emacs-on-windows

(defcustom ack-command (or (executable-find "ack")
			   (executable-find "ack-grep"))
  "Command to use to call ack, e.g. ack-grep under debian"
  :type 'file)

(defvar ack-command-line (concat ack-command " --nogroup --nocolor "))
(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)

(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive)
  ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ; Close STDIN to keep ack from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command ack-command-line)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history             grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))

(provide 'dim-ack)
~~~


Enjoy!
