(in-package #:cl-user)

(defpackage #:aoc2021
  (:use #:cl))
  
(in-package #:aoc2021)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop 
      for line = (read-line in nil nil)
      while line
      collect line)))

(defun 1a (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b) on numbers while b
    counting (> b a) into increase-count
    finally (return increase-count)))

(print (1a "1.txt"))


(defun 1b (filename)
  "Count the times the 3-numbers window in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b c d) on numbers while (and b c d)
    counting (< (+ a b c) (+ b c d)) into increase-count
    finally (return increase-count)))

(print (1b "1.txt"))


(defun 2a (filename)
  "Interpret move instructions for submarine"
  (loop 
    with instructions = (read-file-as-lines filename)
    with words = (map 'list #'sb-unicode:words instructions)
    for (instr nil num-str) in words
    for num = (parse-integer num-str)
    if (equal instr "forward") 
      summing num into forward
    if (equal instr "down")
      summing num into down
    if (equal instr "up")
      summing num into up
     finally (return (* forward (- down up)))))
  
(print (2a "2.txt"))

(defun 2b (filename)
  "Interpret move instructions for submarine"
  (loop 
    with instructions = (read-file-as-lines filename)
    with words = (map 'list #'sb-unicode:words instructions)
    and aim = 0
    and x = 0
    and y = 0
    for (instr nil num-str) in words
    for num = (parse-integer num-str)
    if (string= instr "forward")
      do 
        (incf x num)
        (incf y (* num aim))
    if (string= instr "up")
      do (decf aim num)
    if (string= instr "down")
      do (incf aim num)
    finally (return (* x y))))
  
(print (2b "2.txt"))

(defun string-popcount (s)
  (loop 
    for c across s 
    count (char= c #\1) into ones
    count (char= c #\0) into zeroes
    finally (return (values ones zeroes))))

(defun 3a (filename)
  (loop 
    with lines = (read-file-as-lines filename)
    with line-length = (length (first lines))
    and gamma = (make-array line-length :element-type 'bit :initial-element 0)
    and epsilon = (make-array line-length :element-type 'bit :initial-element 0)
    for i from 0 below line-length
    do
      (loop 
        for line in lines
        count (char= (aref line i) #\1) into ones
        count (char= (aref line i) #\0) into zeroes
        finally 
          (if (> ones zeroes)
              (setf (aref gamma i) 1)
              (setf (aref epsilon i) 1)))))
    ; finally (return (* gamma epsilon))))

(3a "3-sample.txt")

(defun bit-vector-to-integer (bits endianess)
  ""
  (loop
    for b across (if (equal endianess 'big-endian) bits (reverse bits))
    for i from 0 below (length bits)
    sum (ash b i)))

(let ((v (make-array 16 :element-type 'bit :fill-pointer 0)))
  (print (vector-push #b1 v))
  (print (vector-push #b0 v))
  (print (vector-push #b1 v))
  (print (vector-push #b1 v))
  (write v)
  (bit-vector-to-integer v))

(bit-vector-to-integer (reverse #*1011))
(let ((v (vector #b1 #b0)))
  (vector-push #b1 v)
  v)

(let ((v (vector 1 2 3)))
  (vector-push 4 v)
  v)

(let ((v (make-sequence '(vector bit) 0)))
  (vector-push #b1 v)
  v)
