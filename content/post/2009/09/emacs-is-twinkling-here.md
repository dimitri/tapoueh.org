+++
date = "2009-09-24T18:08:00.000000+02:00"
title = "Emacs is Twinkling here"
tags = ["Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/09/24-emacs-is-twinkling-here",
           "/blog/2009/09/24-emacs-is-twinkling-here.html"]
+++

So you have a 
*rolodex* like database in your Emacs, or you have this phone
number in a mail and you want to call it. It happens you have 
`VoIP` setup and
you're using 
[Twinkle](http://www.twinklephone.com/) to make your calls. Maybe you'll then find this
function useful:

~~~
(defun twinkle-call-symbol-or-region ()
  "Call the phone number at point (symbol seems good enough), or in region"
  (interactive)
  (shell-command-to-string 
   (format "twinkle --cmd 'call %s'"
	   (replace-regexp-in-string 
	    "[^0-9+]" "" 
	    (if (use-region-p)
		(buffer-substring (region-beginning) (region-end))
	      (thing-at-point 'symbol))))))
~~~


It happens that 
`symbol` is better than 
`word` here because some phone numbers
begin with 
`+`. And some contains 
`/` or 
`.` as separators, or some other
variations (spaces) so as the number is easy to read for human eyes. 
*Twinkle*
will not like this.
