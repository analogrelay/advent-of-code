# Advent of Code 2017

Implemented in Common Lisp.

Tested in [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/), not guaranteed to work in any other LISP :)

## Usage

For a given day `N`, load the `dayN/dayN.lisp` file in your LISP environment and run either `(run-dayNa "./path/to/input.txt")` or `(run-dayNb "./path/to/input.txt")` to run the part A and part B solutions for each day on a given input file.

For example, to run day 1, using SBCL

```
# cd day01/
# sbcl
... sbcl initialization ...
* (load "./day1.lisp")
T
* (run-day1a "./input.txt")
<the answer>
```

Obviously it's no fun to look at the answers if you plan to do it yourself! So solve the problem first and then take a look. Or if you're not interested in doing it, just take a look for fun :).
