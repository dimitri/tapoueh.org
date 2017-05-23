+++
date = "2009-09-22T23:04:00.000000+02:00"
title = "Escreen integration"
tags = ["Emacs", "prefix"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/09/22-escreen-integration",
           "/blog/2009/09/22-escreen-integration.html"]
+++

After having used 
[elscreen](http://www.morishima.net/~naoto/software/elscreen/) for a long time, I'm now a very happy user of
[escreen](http://www.splode.com/~friedman/software/emacs-lisp/#ui), which feels much better integrated and allows to have one ring of
recently visited buffers per screen. Which is what you need when using a
*screen* like feature, really.

At first, it seemed so good as not to require any tweaking, but soon enough
I had to adapt it to my workflow. After all that's exactly for being able to
do this that I'm using emacs :)

It began quite simple with things like 
`M-[` and 
`M-]` to navigate in screens,
and mouse wheel support to, but then I found that the 
`C-\ b` list of screens
could also support the 
`C-\ a runs the command
escreen-get-active-screen-numbers` command by just adding some 
*emphasis* to
the current escreen in use.

As soon as I had this, and seeing people eyes blinking when working with me
in front of my computer, I wanted to have 
*escreen* switching display where I
am in the minibuffer. You have to try the mouse wheel navigation to fully
appreciate it I guess. Anyway, here it is:

~~~
(load "escreen")
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (propertize (number-to-string s)
					     ;;'face 'custom-variable-tag) " ")
					     'face 'info-title-3)
					     ;;'face 'font-lock-warning-face)
					     ;;'face 'secondary-selection)
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

(defun dim:escreen-goto-last-screen ()
  (interactive)
  (escreen-goto-last-screen)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-prev-screen (&optional n)
  (interactive "p")
  (escreen-goto-prev-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-next-screen (&optional n)
  (interactive "p")
  (escreen-goto-next-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

(global-set-key (kbd "M-[") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'dim:escreen-goto-next-screen)
(global-set-key (kbd "C-\\ DEL") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "C-\\ SPC") 'dim:escreen-goto-next-screen)

(global-set-key '[s-mouse-4] 'dim:escreen-goto-prev-screen)
(global-set-key '[s-mouse-5] 'dim:escreen-goto-next-screen)
~~~


Oh, and as I'm in the 
*terms in emacs* part of universe (rather than using
`emacs -nw` in some terminal emulator, but loosing sync between X clipbloard
and emacs selection), I had to add this too:

~~~
;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'dim:escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'dim:escreen-goto-next-screen)
~~~

