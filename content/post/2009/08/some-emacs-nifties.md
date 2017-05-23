+++
date = "2009-08-03T15:15:00.000000+02:00"
title = "Some emacs nifties"
tags = ["Emacs", "cssh", "rcirc"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/08/03-some-emacs-nifties",
           "/blog/2009/08/03-some-emacs-nifties.html"]
+++

First, here's a way to insert at current position the last message printed
into the minibuffer... well not exactly, in 
`*Messages*` buffer in fact. I was
tired of doing it myself after invoking, e.g., 
`M-x emacs-version`.

~~~
;; print last message
;; current-message is already lost by the time this gets called
(defun dim:previous-message (&optional nth)
  "get last line of *Message* buffer"
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (setq nth (if nth nth 1))
      (while (> nth 0)
	(previous-line)
	(setq nth (- nth 1)))
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun dim:insert-previous-message (&optional nth)
  "insert last message of *Message* to current position"
  (interactive "p")
  (insert (format "%s" (dim:previous-message nth))))

(global-set-key (kbd "C-c m") 'dim:insert-previous-message)
~~~


Now I stumbled accross 
[Planet Emacsen](http://planet.emacsen.org/) and saw this 
[Emacs Utility Functions](http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/)
post, containing a version of 
`duplicate-current-line` that I didn't
like... here's mine:

~~~
;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
	(insert current-line)
	(decf n)))))

(global-set-key (kbd "C-S-d") 'duplicate-current-line)  
~~~


And a last one inspired by some strange 
`vim` behavior for which I fail to see
a need:

~~~
;; on request by cyrilb, who missed it from vim
;; no global-set-key yet, still have to think I'll use it someday...
(defun copy-char-from-prev-line ()
  "Copy char at same position on previous line, when such a line and position exists"
  (interactive)
  (let ((c)
	(p (- (point) (line-beginning-position))))
    (save-excursion
      (when (eq 0 (forward-line -1))
	(when (< (+ (point) p) (line-end-position))
	  (forward-char p)
	  (setq c (thing-at-point 'char)))))
    (when c
      (insert c))))
~~~


Next time I'll try to talk about 
`rcirc-groups` or 
`cssh` which have managed to
take some of my free time recently.
