(defun accumulate (g x accumulation)
    (if (null x)
        accumulation
        (accumulate g (rest x) (funcall g accumulation (first x)))))

(defun редукция2 (g a x)
    (if (null x)
        NIL
        (accumulate g (rest x) (funcall g a (first x)))))