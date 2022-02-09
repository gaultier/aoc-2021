(in-package #:cl-user)

(defpackage #:1b
  (:use #:cl)
  (:export #:solve))

(in-package #:1b)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun solve (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (loop with lines = (read-file-as-lines filename)
        with numbers = (map 'list #'parse-integer lines)
        for (a b c d) on numbers while (and b c d)
        counting (< (+ a b c) (+ b c d)) into increase-count
        finally (return increase-count)))

(print (solve "1.txt"))
