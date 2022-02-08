(in-package #:cl-user)

(defpackage #:1a
  (:use #:cl)
  (:export #:solve))

(in-package #:1a)

(defun solve (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (let ((last-num nil)
        (increase-count 0))
    (with-open-file (in filename)
        (loop for line = (read-line in nil)
            while line do (let ((n (parse-integer line)))
                            (if last-num 
                                (incf increase-count (count-increase n last-num)))
                            (setf last-num n))))
    increase-count))

(defun count-increase (n acc)
  (if (> n acc)
      1
      0))

(print (solve "1a.txt"))
