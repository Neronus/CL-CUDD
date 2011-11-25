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

Examples
--------
*TODO*
