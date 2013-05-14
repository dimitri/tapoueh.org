(:PRE "
(defparameter *additive-color-graph*
  '((red   (red white)   (green yellow) (blue magenta))
    (green (red yellow)  (green white)  (blue cyan))
    (blue  (red magenta) (green cyan)   (blue white))))

(defun symbolic-color-add (a b)
  (cadr (assoc a (cdr (assoc b *additive-color-graph*)))))
")
