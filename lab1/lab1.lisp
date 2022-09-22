(defun sine (x)
(if (< x 0.001)
                x
                (- (* 3 (sine (/ x 3))) (* 4 (expt (sine (/ x 3)) 3)))))