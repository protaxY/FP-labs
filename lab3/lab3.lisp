(defun arrayOfZeros (n)
    (let ((zeroList NIL))
    (loop for i from 0 to (1- n)
        do
        (setq zeroList (append zeroList (list 0))))
    (make-array (list n) :initial-contents zeroList)))

(defun calculateWithoutRowColSum (matrix sumX sumY sumMatrix)
    (let ((result))
    (setq result (make-array (list (array-dimension matrix 0) (array-dimension matrix 1))))
    (loop for i from 0 to (1- (array-dimension matrix 0))
        do
        (loop for j from 0 to (1- (array-dimension matrix 1))
            do
            (setf (aref result i j) (+  (- sumMatrix (aref sumX i)) (- (aref sumY j)) (aref matrix i j)))))
    result))

(defun withoutRowColSum (matrix)
    (let ((sumX)
         (sumY)
         (sumMatrix 0))
    (setq sumX (arrayOfZeros (array-dimension matrix 0)))
    (setq sumY (arrayOfZeros (array-dimension matrix 1)))
    (loop for i from 0 to (1- (array-dimension matrix 0))
        do
        (loop for j from 0 to (1- (array-dimension matrix 1))
            do
            (setf (aref sumX i) (+ (aref sumX i) (aref matrix i j)))
            (setf (aref sumY j) (+ (aref sumY j) (aref matrix i j)))
            (setf sumMatrix (+ sumMatrix (aref matrix i j)))))
    
    (calculateWithoutRowColSum matrix sumX sumY sumMatrix)))