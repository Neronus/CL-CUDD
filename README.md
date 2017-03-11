Common Lisp binding to CUDD [![Build Status](https://travis-ci.org/guicho271828/CL-CUDD.svg?branch=master)](https://travis-ci.org/guicho271828/CL-CUDD)
===========================

This is a fork of original CUDD using a modern common lisp convension, CFFI-Grovel and unit testing.

Supported implementations: SBCL, CCL and ECL.

Requirements: make, curl

What is BDDs and CUDD?
-------------

BDDs (Binary Decision Diagrams) are awesome datastructures that can compactly represent exponentially large number of datasets, as well as allowing the direct computation over the compressed representation (i.e. you do not have to decompress the datastructure in order to conduct addition and multiplication!)

[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)
is a famous implementation of BDDs and its relatives: 
Multi-Terminal Binary Decision Diagrams (MTBDDs or ADDs) and Zero-suppressed Decision Diagrams.

For more information, see either
[Wikipedia](http://en.wikipedia.org/wiki/Binary_decision_diagram)
or the first [paper](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1675141)
about BDDs and the first
[paper](http://repository.cmu.edu/cgi/viewcontent.cgi?article=1456&context=compsci)
about ADDs.

Building/Loading the system
---------------------------
The system is asdf-loadable.
This version of CL-CUDD automatically fetches the CUDD archive from http://vlsi.colorado.edu/~fabio/CUDD/ via curl.
The archive is expanded in the ASDF system directory and builds its dynamic library, which is then loaded by CL-CUDD.

To test the system, evaluate `(asdf:test-system :cl-cudd.test)`.
It also writes the visualizations of the decision diagrams to the system directory in DOT format.
If you have Graphviz installed, the test script also tries to convert the results into pdfs.

The binding(s)
--------------
The binding consists of two layers:
The lower layer has `cl-cudd.baseapi` package.
The initial version was automatically generated using [SWIG](http://www.swig.org) by Utz-Uwe Haus.
The second version was adapted to the needs by Christian von Essen <christian.vonEssen@imag.fr>.
If you want to use this layer, then it would be best to have a look
at the CUDD manual. This layer is a very thin wrapper around the C library,
passes raw pointers around and requires that you take care of reference counting.

Above this layer there is a package named `cl-cudd` (with a nickname `cudd`).
It wraps the pointers from the lower layer, takes care of reference counting for you, and also
adds documentation from the CUDD manual.

DD Construction Examples
------------------------

### Representing a binary function using BDD

Suppose you have a binary function denoted as `b = f(x0,x1,...xn)` which can be expressed in a DNF formula such as
`(or (and x0 (not x1) x2) ...)`.
To begin with, we construct the first disjunction, which is in this case `(and x0 (not x1) x2)`.
It can be implemented as follows:

```lisp
;; functional
(reduce #'node-and
        (list (make-var 'bdd-node :index 0)
              (node-complement (make-var 'bdd-node :index 1))
              (make-var 'bdd-node :index 2))
        :from-end t ;; more efficient when BDDs are build bottom-up
        :initial-value (one-node 'bdd-node))

;; imperative
(let ((f (one-node 'bdd-node)))
  (setf f (node-and f (make-var 'bdd-node :index 2)))
  (setf f (node-and f (node-complement (make-var 'bdd-node :index 1))))
  (setf f (node-and f (make-var 'bdd-node :index 0)))
  f)

;; with a threading macro

(ql:quickload :arrow-macros) (use-package :arrow-macros)

(-> (one-node 'bdd-node)
    (node-and (make-var 'bdd-node :index 2))
    (node-and (node-complement (make-var 'bdd-node :index 1)))
    (node-and (make-var 'bdd-node :index 0)))
```

To take the disjunction of conjunctions, there are similarly named `node-or` function and `zero-node` function.

```lisp
(-> (zero-node 'bdd-node)
    (node-or bdd1)
    (node-or bdd2)
    ...)
```

I don't think you need further explanation but there is one thing to note about
for those unfamiliar with CUDD package. The function `node-complement` turns a
variable into a node having a "complement" arc whose meaning of true-branch and
false-branch are opposite from the standard node. Complement arcs exist for the
performance. Detailed explanations are included in the CUDD manual. Briefly, the
flag designating the complement arc is stored in the least significant bit in
the else-branch pointer. Complement arc reduces the memory usage and cache usage
because taking a complement of a node is a common operation, and complementing a
node without such arc may create a huge number of nodes.

### Representing real functions using ADD and taking their symbolic sum

In ADD, the terminal node can have a value instead of boolean (in BDD), and you
are able to perform various arithmetic operations in the closed form, i.e. for
two ADDs (functions) `f` and `g`, you can obtain `f+g` efficiently.

Imagine `f(x1,x2)` and `g(x1,x2)` takes the following values:

| x1 | x2 | f(x1,x2) | g(x1,x2) |
|:--:|:--:|:--------:|:--------:|
| 0  | 0  |  2       |  4       |
| 0  | 1  |  2       |  4       |
| 1  | 0  |  3       |  3       |
| 1  | 1  |  5       |  7       |

```lisp
;; f
(-> (-> (ADD-constant 2)
        (node-and (node-complement (make-var 'ADD-node :index 0)))
        (node-and (node-complement (make-var 'ADD-node :index 1))))
    (node-or
        (-> (ADD-constant 2)
            (node-and (node-complement (make-var 'ADD-node :index 0)))
            (node-and (make-var 'ADD-node :index 1))))
    (node-or
        (-> (ADD-constant 3)
            (node-and (make-var 'ADD-node :index 0))
            (node-and (node-complement (make-var 'ADD-node :index 1)))))
    (node-or
        (-> (ADD-constant 5)
            (node-and (make-var 'ADD-node :index 0))
            (node-and (make-var 'ADD-node :index 1)))))

;; g 
(-> ...) ;; omitted

(ADD-apply +plus+ f g) ;; -> an ADD representing (f+g)(x1,x2)
```

Other possible arguments to `ADD-apply` are

* `+XNOR+` (originally Cudd_addXnor) -- XNOR of two 0-1 ADDs.
* `+XOR+` (originally Cudd_addXor) -- XOR of two 0-1 ADDs.
* `+NOR+` (originally Cudd_addNor) -- NOR of two 0-1 ADDs.
* `+NAND+` (originally Cudd_addNand) -- NAND of two 0-1 ADDs.
* `+OR+` (originally Cudd_addOr) -- Disjunction of two 0-1 ADDs.
* `+AGREEMENT+` (originally Cudd_addAgreement) -- f op g,  where f op g is f if f==g; background if f!=g.
* `+DIFF+` (originally Cudd_addDiff) -- f op g , where f op g is plusinfinity if f=g; min(f,g) if f!=g.
* `+ONE-ZERO-MAXIMUM+` (originally Cudd_addOneZeroMaximum) -- 1 if f > g and 0 otherwise.
* `+MAXIMUM+` (originally Cudd_addMaximum) -- Integer and floating point maximum.
* `+MINIMUM+` (originally Cudd_addMinimum) -- Integer and floating point minimum.
* `+MINUS+` (originally Cudd_addMinus) -- Integer and floating point substraction.
* `+DIVIDE+` (originally Cudd_addDivide) -- Integer and floating point division.
* `+SET-NZ+` (originally Cudd_addSetNZ) -- This operator sets f to the value of g wherever g != 0.
* `+THRESHOLD+` (originally Cudd_addThreshold) -- Threshold operator for Apply (f if f >=g; 0 if f<g)
* `+TIMES+` (originally Cudd_addTimes) -- Integer and floating point multiplication.
* `+PLUS+` (originally Cudd_addPlus) -- Integer and floating point addition

### Representing a family of set using ZDD

ZDD is notoriously famous among Japanese CS researchers due to the hype in
https://www.youtube.com/watch?v=Q4gTV4r0zRs . It is actually quite powerful and
is a better option than BDD when *most of the paths leads to the zero-node*, and is
particularly useful for representing a family of sets.




System structure
----------------

### Low-level

You can use the low-level system just as you would use the C API of
CUDD. This also means that you have to do all the reference counting
yourself, with one exception: The reference count of the return value
each CUDD function that returns a node is increased if it is not
`null`. If it is `null`, a signal of type `cudd-null-pointer-error` is
raised.

### High-level

The high level API automatically wraps the CUDD nodes in an instance
of class `node`. ADD nodes are wrapped in an instance of `add-node`
and BDD nodes are wrapped in an instance of type `bdd-node`.

This enables runtime type checking (so that you don't stick ADD nodes
into BDD functions or vice-versa) and also automatic reference counting.

Almost all CUDD functions need to refer to a CUDD manager. In the
high-level API this manager is contained in special variable
`*manager*`. You can bind a manager using the macro `with-manager`.
You can also create a manager by
`(make-instance 'manager :pointer (cudd-init 0 0 256 262144 0))`.

All functions of package `CL-CUDD` are documented using the original or
slightly modified documentation of CUDD.

Known problems
--------------

Using the GC to do reference counting automatically has its own share of problems:

1. References may be freed very late.

   Nodes will be dereferenced only if your CL implementation thinks
   that it's time for it. This is usually when itself is running out
   of memory. Because you are usually only holding on to the top of
   a diagram, you are not using as much memory in CL as you are using
   in CUDD. Hence the GC might come pretty late while CUDD is happily
   accumulating memory.

   The solution to that is to try to call the garbage collector
   manually every so often using for example
   TRIVIAL-GARBAGE:GC

2. References may be freed too early

   The following two examples demonstrate the problem.

        (defun foo (dd)
          (let ((ptr (node-pointer dd)))
            ;; please don't GC me here
            (cudd-do-something ptr)))

   In this example the GC might decide to run where there is the
   comment.
   In that case, provided that nothing outside of the function call
   holds on to `dd`, the reference count of `ptr` might be decreased,
   go down to zero and the node vanishes before `cudd-do-something` is
   called.
   The solution: Instead of `let`, use macro `with-pointers` which
   increases the reference count of each node before the body and
   decreases the reference count of each node after the body.

       (defun bar (dd)
         (cudd-do-something-with-callback (node-pointer dd)))

   In this case, we might have a problem if
   `cudd-do-something-with-callback` calls back into
   lisp, upon which point lisp garbage collects and decreases
   the reference count of `dd`.
   
   Solution: Again, use `with-pointers` instead of `node-pointer`.
