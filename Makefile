aoc2021:
	sbcl --load main.lisp  --eval "(sb-ext:save-lisp-and-die #p\"aoc2021\" :toplevel #'aoc2021:main :executable t)"

run:
	sbcl --script main.lisp
