.PHONY: build run

aoc2021: main.lisp
	sbcl --load main.lisp  --eval "(sb-ext:save-lisp-and-die #p\"aoc2021\" :toplevel #'aoc2021:main :executable t)"

build: aoc2021

run:
	sbcl --script main.lisp --dynamic-space-size 10024
