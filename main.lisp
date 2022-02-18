(in-package #:cl-user)

(defpackage #:aoc2021
  (:use #:cl)
  (:export :main))

(in-package #:aoc2021)

(require "asdf")

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop 
      for line = (read-line in nil nil)
      while line
      collect line)))

;;; 1a
(defun 1a (filename)
  "Count the times the numbers (one on each line) in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b) on numbers while b
    counting (> b a) into increase-count
    finally (return increase-count)))

;;; 1b
(defun 1b (filename)
  "Count the times the 3-numbers window in the file <filename> increased"
  (loop 
    with lines = (read-file-as-lines filename)
    with numbers = (map 'list #'parse-integer lines)
    for (a b c d) on numbers while (and b c d)
    counting (< (+ a b c) (+ b c d)) into increase-count
    finally (return increase-count)))

;;; 2a
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

;;; 2b
(defun 2b (filename)
  "Interpret move instructions for submarine"
  (loop 
    with lines = (read-file-as-lines filename)
    with instructions = (map 'list #'uiop:split-string lines)
    and aim = 0
    and x = 0
    and y = 0
    for (instr num-str) in instructions
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

;;; 3a
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

;;; 3b
(defun string->bitarray (s)
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
  (map 'list #'string->bitarray (read-file-as-lines filename)))

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

(defun 3b (filename)
  (let* ((lines (read-lines-from-file-as-bitarray filename))
         (O2 (3b-solve-O2 lines))
         (CO2 (3b-solve-CO2 lines)))
    (* O2 CO2)))

(3b "3.txt")

;;; 4a
(defun read-bingo-board (in)
  (loop 
    initially (read-line in nil nil) ; skip empty line
    with board = (make-array '(5 5))
    for y from 0 below 5
    for line = (read-line in nil nil)
    while line
    for nums-str = (remove-if #'uiop:emptyp (uiop:split-string line))
    for nums = (map 'list #'parse-integer nums-str)
    do
      (loop
        for x from 0 below 5
        for n in nums
        do (setf (aref board y x) (list n nil)))
    finally (when (> y 0) (return board))))

(defun read-bingo-input (filename)
  "Read file into a list of numbers to draw and a list of boards"
  (with-open-file (in filename)
    (loop 
      with first-line = (read-line in nil nil)
      with draw-numbers = (map 'list #'parse-integer (uiop:split-string first-line :separator '(#\,)))
      for board = (read-bingo-board in)
      while board
      collect board into boards
      finally (return (values draw-numbers boards)))))

(defun board-some-column-complete-p (board)
  (loop
    for x from 0 below 5
    thereis (loop
              for y from 0 below 5
              for (nil marked) = (aref board y x)
              always marked)))

(defun board-some-row-complete-p (board)
  (loop
    for y from 0 below 5
    thereis (loop
              for x from 0 below 5
              for (nil marked) = (aref board y x)
              always marked)))

(defun board-complete-p (board)
  (or (board-some-row-complete-p board) (board-some-column-complete-p board)))

(defun board-mark-number (board num)
  (dotimes (y 5)
    (dotimes (x 5)
      (when (= num (first (aref board y x)))
        (setf (aref board y x) (list num t))))))

(defun board-score (board last-drawn-num)
  (let ((unmarked-sum (loop
                        for y from 0 below 5
                        sum
                          (loop
                            for x from 0 below 5
                            for (n marked) = (aref board y x)
                            unless marked
                            sum n))))
    (* unmarked-sum last-drawn-num)))

(defun 4a (filename)
  (multiple-value-bind (draw-numbers boards) (read-bingo-input filename)
    (loop
      for n in draw-numbers
      do 
        (loop
          for board in boards
          do 
            (board-mark-number board n)
            (if (board-complete-p board)
              (return-from 4a (board-score board n)))))))
                
;;; 4b
(defun find-worst-board (filename)
  (multiple-value-bind (draw-numbers boards) (read-bingo-input filename)
    (loop
      for n in draw-numbers
      do 
        (loop
          for board in boards
          do 
            (board-mark-number board n)
            (let ((new-boards (remove-if #'board-complete-p boards)))
              (if (and 
                    (= 1 (length boards))
                    (board-complete-p board))
                (return-from find-worst-board (values board n))
                (setf boards new-boards)))))))

(defun 4b (filename)
  (multiple-value-bind (board n) (find-worst-board filename)
        (board-score board n)))


;;; 5a
(defun parse-line-points (s)
  (let* ((parts (uiop:split-string s :separator '(#\, #\- #\>)))
         (x1 (parse-integer (first parts)))
         (y1 (parse-integer (second parts)))
         (x2 (parse-integer (fourth parts)))
         (y2 (parse-integer (fifth parts))))
    (list (list x1 y1) (list x2 y2))))

(defun incr-kv (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (setf (gethash key hash-table) (+ 1 value))
        (setf (gethash key hash-table) 1))))

(defun fill-grid-straight (points)
 (loop
    with grid = (make-hash-table :test #'equal)
    for ((x1 y1) (x2 y2)) in points
    when (= y1 y2) do
        (loop
          for x from (min x1 x2) to (max x1 x2)
          do (incr-kv (list x y1) grid))
    when (= x1 x2) do
        (loop
          for y from (min y1 y2) to (max y1 y2)
          do (incr-kv (list x1 y) grid))
    finally (return grid)))

(defun 5a (filename)
  (let* ((lines (read-file-as-lines filename))
         (points (map 'list #'parse-line-points lines))
         (grid (fill-grid-straight points)))
    (loop
          for v being the hash-values in grid
          when (> v 1)
          count 1)))

(5a "5.txt")

;;; entrypoint
(defun main ()
  (print (1a "1.txt"))
  (print (1b "1.txt"))
  (print (2a "2.txt"))
  (print (2b "2.txt"))
  (print (3a "3.txt"))
  (print (3b "3.txt"))
  (print (4a "4.txt"))
  (print (4b "4.txt"))
  (print (5a "5.txt")))

(main)
