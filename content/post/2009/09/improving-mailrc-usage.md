+++
date = "2009-09-07T01:29:00.000000+02:00"
title = "Improving ~/.mailrc usage"
tags = ["Emacs", "prefix"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/09/07-improving-mailrc-usage",
           "/blog/2009/09/07-improving-mailrc-usage.html"]
+++

So I've been adviced to use 
`~/.mailrc` for keeping a basic address book in
Emacs, for use within 
`gnus` for example. I had to resort to the manual to
find out how to use the file aliases when I need them, that is when
composing a mail. For the record, here's what I had to do:

~~~
;; mails and aliases
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
(global-set-key (kbd "C-c @") 'mail-abbrev-insert-alias)
~~~


That means I prefer hitting 
`C-c @`, then typing the alias in the minibuffer
(with completion) and there after see the full mail address in my
`message-mode` buffer. This looks like it'll change over time, but rather than
searching how to have a nice inline alias completion (
`M-tab` maybe, but
already used by the 
*window manager*), I've tackled the problem of maintaining
the ~/.mailrc file.

Lazy as I am (or I wouldn't be using Emacs this much), having to manually
select the email region in the buffer, open or switch to the 
`mailrc` buffer
then paste my new entry, not forgetting to format it with 
`alias foo` prefix
and checking for alias usage while doing so didn't strike me as
appealing. Oh and don't forget to add quote where they belong, too.

Too much work that I wanted to automate. Here we go:

~~~
;; automate adding mail at point to ~/.mailrc
(defun dim:mailrc-add-entry (alias)
  "read email at point"
  (interactive "Malias: ")
  (let ((address (thing-at-point 'email-address))
	(buffer (find-file-noselect mail-personal-alias-file t)))
    (when address
      (with-current-buffer buffer
	;; we don't support updating existing alias in the file
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward (concat "alias " alias) nil t)
	      (error "Alias %s is already present in .mailrc" alias)))

	(save-current-buffer
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "\nalias %s \"%s <%s>\"" alias (cdr address) (car address)))))))))

(global-set-key (kbd "C-c C-@") 'dim:mailrc-add-entry)
~~~


Quite there, you'll notice that I'm using 
`thing-at-point 'email-address`, and
maybe you already know that 
`emacs23` does not provide this. It provides
`thing-at-point 'email` which will ignore real name and all. For example,
given a point somewhere inside the right part of 
`John Doe
<johndoe@email.tld>` the 
`'email` variant of 
`thing-at-point` will return
`johndoe@email.tld`. In words of one syllabe: not what I want.

So after searching around for a solution, I saw 
`mail-header-parse-address`
from the API oriented 
`mail-parse` librairy, and finaly came up with this dead simple
solution which works fine enough for me:

~~~
(require 'mail-parse)

(defun thing-at-point-bounds-of-email-address ()
  "return a cons of begin and end position of email address at point, including full name"
  (save-excursion
    (let* ((search-point (point))
	   (start (re-search-backward "[:,]" (line-beginning-position) 'move))
	   (dummy (goto-char search-point))
	   (end   (re-search-forward  "[:,]" (line-end-position) t)))
      (setq start (if start (+ 1 start)
		    (line-beginning-position)))
      (unless end (setq end (line-end-position)))
      (cons start end))))

(defun thing-at-point-email-address ()
  "return full email address at point"
  (let* ((bounds (thing-at-point-bounds-of-email-address))
	 (email-address-text
	  (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (mail-header-parse-address email-address-text)))

(put 'email-address 'bounds-of-thing-at-point 'thing-at-point-bounds-of-email-address)
(put 'email-address 'thing-at-point 'thing-at-point-email-address)
~~~


Now, when I receive a mail and want to store an alias for it, I simply place
point somewhere in the mail then hit 
`C-c C-@`, and 
*voilà* my 
`~/.mailrc` is
uptodate.

Hope it'll be useful for someone else, but at least I'm keeping annotated
history of the files :)
