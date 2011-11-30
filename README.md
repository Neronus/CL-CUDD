Common Lisp binding to CUDD 
===========================

What is CUDD?
-------------
[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)
is an implementation of Binary Decision Diagrams (BDDs) and
Multi-Terminal Binary Decision Diagrams (MTBDDs or ADDs).

For more information, see either
[Wikipedia](http://en.wikipedia.org/wiki/Binary_decision_diagram)
or the first [paper](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1675141)
about BDDs and the first
[paper](http://repository.cmu.edu/cgi/viewcontent.cgi?article=1456&context=compsci)
about ADDs.

The binding(s)
--------------
The binding consists of two layers:
The lower layer is called `cuddapi` and is automatically generated
using [SWIG](http://www.swig.org).
The definition is found in file `cudd-cffi.i`. The original
version was written by Utz-Uwe Haus, and then adapted to my needs here.
If you want to use this layer, then it would be best to have a look
at the CUDD manual. This layer is a very thin wrapper around the C library,
passes raw pointers around and requires that you take care of reference counting.

Above this layer there is a layer called `cudd`. It wraps the pointers
from the lower layer, takes care of reference counting for you, and also
adds documentation from the CUDD manual.

Building/Loading the system
---------------------------
You first need to build a shared library from CUDD (which doesn't do
that automatically for some reason). See `Makefile` for that.

CL-CUDD comes with an ASDF definition. So you add the directory
containing `cl-cudd.asd` to your `asdf:*central-registry*` and
then load CL-CUDD via `(asdf:operate 'asdf:load-op 'cl-cudd)`.

This distribution comes with atomically generated bindings from
CUDD in file `cuddapi.lisp`. If you want to regenerate the bindings
for some reason, also see the `Makefile`. After that, you have
probably have to edit the generated code. Function
`Cudd_PrioritySelect` has a parameter `Pi`. You have to rename
that variable to something else.

Using the system
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
`(make-instance 'manager :pointer (cuddapi:manager ...))`.

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

Examples
--------
*TODO*
