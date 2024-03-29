+++
date = "2011-01-11T16:20:00.000000+01:00"
title = "Starting afresh with el-get"
tags = ["Emacs", "el-get", "switch-window"]
categories = ["Projects","switch-window"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/01/11-starting-afresh-with-el-get",
           "/blog/2011/01/11-starting-afresh-with-el-get.html"]
+++

It so happens that a colleague of mine wanted to start using 
[Emacs](http://www.gnu.org/software/emacs/) but
couldn't get to it. He insists on having proper color themes in all
applications and some sensible defaults full of nifty add-ons everywhere,
and didn't want to have to learn that much about 
*Emacs* and 
*Emacs Lisp* to get
started. I'm not even sure that he will 
[Take the Emacs tour](http://www.gnu.org/software/emacs/tour/).

You would tell me that there's nothing we can do for so unfriendly
users. Well, here's what I did:

~~~
;; emacs setup

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq
 el-get-sources 
 '(el-get
   php-mode-improved
   psvn
   auto-complete
   switch-window

   (:name buffer-move
	  :after (lambda ()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name magit
	  :after (lambda ()
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change
	  :after (lambda ()
		   ;; azerty keyboard here, don't use C-x C-/
		   (global-set-key (kbd "C-x C-_") 'goto-last-change)))))

(when window-system
   (add-to-list 'el-get-sources  'color-theme-tango))

(el-get 'sync)

;; visual settings
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

(set-frame-font "Monospace-10")

(global-hl-line-mode)

;; suivre les changements exterieurs sur les fichiers
(global-auto-revert-mode 1)

;; pour les couleurs dans M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; S-fleches pour changer de fen&#xEA;tre
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; find-file-at-point quand &#xE7;a a du sens
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil) ; disable URL features in ffap
(setq ffap-ftp-regexp nil) ; disable FTP features in ffap
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)

;; use iswitchb-mode for C-x b
(iswitchb-mode)

;; I can't remember having meant to use C-z as suspend-frame
(global-set-key (kbd "C-z") 'undo)

;; winner-mode pour revenir sur le layout pr&#xE9;c&#xE9;dent C-c <left>
(winner-mode 1)

;; dired-x pour C-x C-j
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)
~~~


With just this simple 87 lines (all included) of setup, my local user is
very happy to switch to using 
[our favorite editor](http://www.gnu.org/software/emacs/). And he's not even afraid
(yet) of his 
`~/.emacs`. I say that's a very good sign of where we are with
[el-get](https://github.com/dimitri/el-get)!
