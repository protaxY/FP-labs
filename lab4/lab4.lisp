(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'whitespace-char-p string
                                     :start left)
                        len)
        unless (= right left)
          collect (subseq string left right)
        while (< right len)))

(defun isNumeric (str)
    (loop for c across str
        do (if (not (digit-char-p c)) (return-from isNumeric nil)))
    t)

(defun max-digital-word-length (sentenceList)
    (let ((maxLength 0)
          (maxLengthWord nil))
    (loop for sentence in sentenceList
        do 
        (loop for word in (word-list sentence)
            do (if (and (isNumeric word) (>= (length word) maxLength))
                   (setf maxLength (length word)
                         maxLengthWord word))))
    (values maxLength maxLengthWord)))