(defclass polynom ()
    ((polunom-symbol :initarg :var1 :reader var1)
    ;; Разреженный список термов в порядке убывания степени
     (term-list :initarg :terms :reader terms)
    )
)

(defun make-term (&key order coeff)
    (list order coeff)
)

(defun order (term) (first term))
(defun coeff (term) (second term))

(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var1 p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var1 p)
                          (order term)))
                  (terms p))))

; вспомогательные функции

(defun zeroList (n)
    (make-list n :initial-element '0)
)

(defun multiplyList (list)
    (mapcar #'(lambda(x) (reduce '* x)) list)
)

(defun listSum (list)
    (reduce '+ list)
)

(defun listLastNElements (count list)
    (last list count)
)

(defun deleteLast (list)
    (loop for i on list
        while (rest i)
            collect (first i)
    )
)

(defun currentСoefficient (cur next tail)
    (cond ((null next) (if (= 0 (order cur)) 
                            (cons (coeff cur) tail)
                            (cons (coeff cur) (append (zeroList (order cur)) tail)))
          )
          ((= (order cur) (1+ (order next))) (cons (coeff cur) tail))
          (t (cons (coeff cur) (append (zeroList (1- (- (order cur) (order next)))) tail)))
    )
)

(defun coefficientList (p)
    (if p (currentСoefficient (first p) (second p) (coefficientList (rest p))))
)

(defun listCombinations (count list)
    (cond
        ((zerop count) '(()))
        ((endp list)   '())
        (t (nconc (mapcar (let ((item (first list)))
                                (lambda (comb) (cons item comb)))
                                (listCombinations (1- count) (rest list)))
              (listCombinations count (rest list))))
    )
)

; реализация формулы

(defun DSMultiplication (d a j)
    (cond
        ((oddp j) (* d (listSum (multiplyList (listCombinations j a)))))
        (t (* -1 (* d (listSum (multiplyList (listCombinations j a))))))
    )
)

(defun DSList (j d a i)
    (if d (cons (DSMultiplication (first d) (listLastNElements (- (list-length a) j) a) i) (DSList (1+ j) (rest d) a (1- i))))
)

(defun DSMultSum (d a)
    (listSum (DSList 0 d a (list-length d)))
)

(defun calculateD (p a)
    (let  ((b (coefficientList (terms p)))
           (d (list (first (coefficientList (terms p)))))
           )
        (loop for i in (rest b) 
            do (nconc d (list (+ i (DSMultSum d (deleteLast a)))))
        )
    (reverse d)
    )
)

; демонстрация работы

(defun test ()

; полиномы и коэффициенты

    (let ((polynom1 (make-instance 'polynom
          :var1 'x
          :terms (list (make-term :order 2 :coeff 5)
                       (make-term :order 1 :coeff 3.3)
                       (make-term :order 0 :coeff -7))))
         (polynom2 (make-instance 'polynom
          :var1 'x
          :terms (list (make-term :order 3 :coeff 1)
                       (make-term :order 1 :coeff 2)
                       (make-term :order 0 :coeff 1))))
         (polynom3 (make-instance 'polynom
          :var1 'x
          :terms (list (make-term :order 7 :coeff -10)
                       (make-term :order 4 :coeff 1)
                       (make-term :order 2 :coeff 3))))
         (a1 (list 1 2 3))
         (a2 (list 2 5 1 -3))
         (a3 (list 1 1 1 1 1 1 1 1)))

; печать

    (print "Полином:")
    (print polynom1)
    (print "Список коэффициентов a:")
    (print a1)
    (print "Список коэффициентов d:")
    (print (calculateD polynom1 a1))

    (print "Полином:")
    (print polynom2)
    (print "Список коэффициентов a:")
    (print a2)
    (print "Список коэффициентов d:")
    (print (calculateD polynom2 a2))

    (print "Полином:")
    (print polynom3)
    (print "Список коэффициентов a:")
    (print a3)
    (print "Список коэффициентов d:")
    (print (calculateD polynom3 a3))

    (values))
)