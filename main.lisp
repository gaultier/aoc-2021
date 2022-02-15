(in-package #:cl-user)

(defpackage #:aoc2021
  (:use #:cl))

(in-package #:aoc2021)

(require "asdf")

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop 
      for line = (read-line in nil nil)
      while line
      collect line)))

; 1a
(defun 1a (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b) on numbers while b
    counting (> b a) into increase-count
    finally (return increase-count)))

(print (1a "1.txt"))

; 1b
(defun 1b (filename)
  "Count the times the 3-numbers window in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b c d) on numbers while (and b c d)
    counting (< (+ a b c) (+ b c d)) into increase-count
    finally (return increase-count)))

(print (1b "1.txt"))

; 2a
(defun 2a (filename)
  "Interpret move instructions for submarine"
  (loop 
    with lines = (read-file-as-lines filename)
    with instructions = (map 'list #'uiop:split-string lines)
    for (instr num-str) in instructions
    for num = (parse-integer num-str)
    if (equal instr "forward") 
    summing num into forward
    if (equal instr "down")
    summing num into down
    if (equal instr "up")
    summing num into up
    finally (return (* forward (- down up)))))

(print (2a "2.txt"))

; 2b
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

; 3a
(defun bit-vector-to-integer-little-endian (bits)
  ""
  (loop
    for b across (reverse bits)
    for i from 0 below (length bits)
    sum (ash b i)))

(defun 3a (filename)
  (loop 
    with lines = (read-file-as-lines filename)
    with line-length = (length (first lines))
    with gamma = (make-array line-length :element-type 'bit :initial-element 0 :fill-pointer 0)
    with epsilon = (make-array line-length :element-type 'bit :initial-element 0 :fill-pointer 0)
    for i from 0 below line-length
    do
    (loop 
      for line in lines
      count (char= (aref line i) #\1) into ones
      count (char= (aref line i) #\0) into zeroes
      finally
      (vector-push (if (> ones zeroes) 1 0) gamma)
      (vector-push (if (> ones zeroes) 0 1) epsilon))
    finally (let ((gamma-num (bit-vector-to-integer-little-endian gamma))
                  (epsilon-num (bit-vector-to-integer-little-endian epsilon)))
              (return (* gamma-num epsilon-num)))))

(print (3a "3.txt"))

; 3b
(defun string-to-bitarray (s)
  (loop
    with res = (make-array (length s) :element-type 'bit :initial-element 0 :fill-pointer 0)
    for c across s
    do 
    (cond 
      ((char= c #\0) (vector-push 0 res))
      ((char= c #\1) (vector-push 1 res))
      (t (error "invalid character encountered")))
    finally (return res)))

(defun read-lines-from-file-as-bitarray (filename)
  (map 'list #'string-to-bitarray (read-file-as-lines filename)))

(defun nums-with-majority-bit-in-position (nums pos criteria)
  (loop 
    for n in nums
    when (= (aref n pos) 0) collect n into nums-with-zero 
    when (= (aref n pos) 1) collect n into nums-with-one
    finally (return
              (if (equal criteria 'most-common)
                (if (> (length nums-with-zero) (length nums-with-one))
                    nums-with-zero
                    nums-with-one)
                (if (> (length nums-with-zero) (length nums-with-one))
                    nums-with-one
                    nums-with-zero)))))
              

(defun 3b-solve-O2 (nums &key (pos 0))
  (if (= 1 (length nums))   
      (bit-vector-to-integer-little-endian (first nums))
      (3b-solve-O2 (nums-with-majority-bit-in-position nums pos 'most-common) :pos (+ 1 pos))))

(defun 3b-solve-CO2 (nums &key (pos 0))
  (if (= 1 (length nums))   
      (bit-vector-to-integer-little-endian (first nums))
      (3b-solve-CO2 (nums-with-majority-bit-in-position nums pos 'least-common) :pos (+ 1 pos))))

(3b-solve-O2 (read-lines-from-file-as-bitarray "3-sample.txt"))
(3b-solve-CO2 (read-lines-from-file-as-bitarray "3-sample.txt"))

(defun 3b (filename)
  (let* ((lines (read-lines-from-file-as-bitarray filename))
         (O2 (3b-solve-O2 lines))
         (CO2 (3b-solve-CO2 lines)))
    (* O2 CO2)))

(3b "3.txt")

; 4a

(defun output-stream-to-string-if-not-empty (ss)
  (let ((s (get-output-stream-string ss)))
       (if (string= "" s)
         nil
         s)))
  

; (defun read-bingo-board (in)
;   (loop 
;     initially (read-line in nil nil) ; skip empty line
;     for line = (read-line in nil nil)
;     for num-strs = (string-split line #\Space)
;     until (or (not line) (string= line ""))
;     do (print num-strs)
;     append (map 'list #'parse-integer num-strs)))
        

; (defun read-bingo-input (filename)
;   "Read file into a list of lines."
;   (with-open-file (in filename)
;     (loop 
;       with first-line = (read-line in nil nil)
;       with draw-numbers = (map 'list #'parse-integer (string-split first-line #\,))
;       with board = (read-bingo-board in)
;       do (print board))))
      ; for board = (read-bingo-board in)
      ; while board
      ; collect board into boards
      ; finally (return (values draw-numbers boards)))))

; (read-bingo-input "4-sample.txt")
