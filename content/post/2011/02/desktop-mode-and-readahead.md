+++
date = "2011-02-23T16:45:00.000000+01:00"
title = "desktop-mode and readahead"
tags = ["Emacs", "restore"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/02/23-desktop-mode-and-readahead",
           "/blog/2011/02/23-desktop-mode-and-readahead.html"]
+++

I'm using 
[Desktop Save Mode](http://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html#Desktop-Save-Mode) so that 
[Emacs](http://www.gnu.org/software/emacs/) knows to open again all the
buffers I've been using.  That goes quite well with how often I start 
`Emacs`,
that is once a week or once a month.  Now, 
`M-x ibuffer` last line is as
following:

~~~
718 buffers         19838205                  668 files, 15 processes
~~~


That means that at startup, 
`Emacs` will load that many files.  In order not
to have to wait until it's done doing so, I've setup things this way:

~~~
;; and the session
(setq desktop-restore-eager 20
      desktop-lazy-verbose nil)
(desktop-save-mode 1)
(savehist-mode 1)
~~~


Problem is that it's still slow.  An idea I had was to use the 
[readahead](https://fedorahosted.org/readahead/browser/README)
tool that allows reducing some distributions boot time.  Of course this tool
is not expecting the same file format as 
`emacs-desktop` uses.  Still,
converting is quite easy is some 
`awk` magic.  Here's the result:

~~~
;;; dim-desktop.el --- Dimitri Fontaine
;;
;; Allows to prepare a readahead file list from desktop-save

(require 'desktop)

(defvar dim-desktop-file-readahead-list
  "~/.emacs.desktop.readahead"
  "*Where to save the emacs desktop `readahead` file list")

(defvar dim-desktop-filelist-command
  "gawk -F '[ \"]' '/desktop-.*-buffer/ {getline; if($4) print $4}' %s"
  "Command to run to prepare the readahead file list")

(defun dim-desktop-get-readahead-file-list (&optional filename dir)
  "get the file list for readahead from dekstop file in DIR, or ~"
  (with-temp-file (or filename dim-desktop-file-readahead-list)
    (insert
     (shell-command-to-string
      (format dim-desktop-filelist-command
	      (expand-file-name desktop-base-file-name (or dir "~")))))))

;; This will not work because the hook is run before to add the buffers into
;; the desktop file.
;;
;;(add-hook 'desktop-save-hook 'dim-desktop-get-readahead-file-list)

;; so instead, advise the function
(defadvice desktop-save (after desktop-save-readahead activate)
  "Prepare a readahead(8) file for the desktop file"
  (dim-desktop-get-readahead-file-list))

(provide 'dim-desktop)
~~~


The 
`awk` construct 
`getline` allows to process the next line of the input file,
which is very practical here (and in a host of other situations).  Now that
we have a file containing the list of files 
`Emacs` will load, we have to
tweak the system to 
`readahead` those disk blocks.  As I'm currently using 
[KDE](http://kde.org/)
again, I've done it thusly:

~~~
% cat ~/.kde/Autostart/readahead.emacs.sh
#! /bin/bash

# just readahead the emacs desktop files
# this file listing is maintained directly from Emacs itself
readahead ~/.emacs.desktop.readahead
~~~


So, well, it works.  The files that 
`Emacs` will need are pre-read, so at the
time the desktop really gets to them, I see no more disk activity (laptops
have a led to see that happening).  But the desktop loading time has not
changed...
