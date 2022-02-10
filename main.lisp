(in-package #:cl-user)

(defpackage #:aoc2021
  (:use #:cl))
  
(in-package #:aoc2021)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun 1a (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (loop with lines = (read-file-as-lines filename)
        with numbers = (map 'list #'parse-integer lines)
          for (a b) on numbers while b
        counting (> b a) into increase-count
        finally (return increase-count)))

(print (1a "1.txt"))

(defun 1b (filename)
  "Count the times the 3-numbers window in the file <filename> increased"
  (loop with lines = (read-file-as-lines filename)
        with numbers = (map 'list #'parse-integer lines)
        for (a b c d) on numbers while (and b c d)
        counting (< (+ a b c) (+ b c d)) into increase-count
        finally (return increase-count)))

(print (1b "1.txt"))


(defun 2a (filename)
  "Interpret move instructions for submarine")
  

(loop with instructions = (list "forward 5" "down 2" "up 3")
      with words = (map 'list #'sb-unicode:words instructions)
      and res = #c(0 0)
      for (instr _ num-str) in words
      do (let ((num (parse-integer num-str)))
           (cond ((equal instr "forward") (incf res #c(num 0)))
                 ((equal instr "down") (incf res #c(0 num)))
                 ((equal instr "up") (incf res #c(0 -num)))
                 (t (error "invalid instruction"))))
      finally (return res))

(let ((c #c(0 0)))
  (incf c #c(1 1))
  (format t "~Ai ~A" (imagpart c) (realpart c)))


