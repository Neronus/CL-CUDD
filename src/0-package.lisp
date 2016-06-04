;;;; package.lisp

(in-package :cl-user)

(defpackage cl-cudd.swig-macros
  (:use :cl :cffi)
  (:documentation
   "Package containing utility functions for SWIG cffi interface generation")
  (:export #:swig-lispify #:defanonenum))

(defpackage cl-cudd.baseapi
  (:documentation "Low-level interface")
  (:use :cl :cffi :cl-cudd.swig-macros :alexandria)
  (:shadow #:pi)
  (:export #:cudd-manager #:cudd-node #:cudd-bdd-not #:cudd-add-cube #:cudd-bdd-cube
           #:dump-dot #:add-var #:bdd-var #:print-info #:+cudd-max-index+ #:cudd-node-is-constant
           #:cudd-node-get-value #:cudd-node-get-then #:cudd-node-get-else
           #:cudd-node-get-ref-count)
  ;; other exports done by swig
  )

(defpackage cl-cudd
  (:documentation "High-level interface")
  (:use :cl :cffi :alexandria :cl-cudd.swig-macros :cl-cudd.baseapi)
  (:nicknames :cudd)
  (:export 
   #:+AGREEMENT+
   #:+AND+
   #:+DIFF+
   #:+DIVIDE+
   #:+EQUALS+
   #:+GREATER-THAN+
   #:+GREATER-THAN-EQUALS+
   #:+LESS-THAN+
   #:+LESS-THAN-EQUALS+
   #:+LOG-X-Y+
   #:+MAXIMUM+
   #:+MINIMUM+
   #:+MINUS+
   #:+MOD+
   #:+NAND+
   #:+NOR+
   #:+NOT-EQUALS+
   #:+ONE-ZERO-MAXIMUM+
   #:+OR+
   #:+PLUS+
   #:+POW+
   #:+SET-NZ+
   #:+THRESHOLD+
   #:+TIMES+
   #:+XNOR+
   #:+XOR+
   #:*MANAGER*
   #:ADD->BDD
   #:ADD-APPLY
   #:ADD-CONSTANT
   #:ADD-NEGATE
   #:ADD-NODE
   #:BDD->ADD
   #:BDD-NODE
   #:COFACTOR
   #:COUNT-LEAVES
   #:COUNT-MINTERM
   #:CUBE
   #:DAG-SIZE
   #:DISABLE-GC
   #:ENABLE-GC
   #:EXIST-ABSTRACT
   #:UNIV-ABSTRACT
   #:IF-THEN-ELSE
   #:MAKE-VAR
   #:MANAGER
   #:MANAGER-POINTER
   #:NODE
   #:NODE-POINTER
   #:NODE-COMPLEMENT
   #:NODE-EQUAL
   #:NODE-CONSTANT-P
   #:NODE-VALUE
   #:NODE-INDEX
   #:OR-ABSTRACT
   #:SWAP-VARIABLES
   #:WITH-MANAGER

   #:add->bdd-interval
   #:add->bdd-threshold
   #:add->bdd-strict-threshold

   #:node-or
   #:node-and

   #:plus-infinity
   #:minus-infinity

   #:min-abstract
   #:max-abstract

   #:one-node
   #:zero-node

   #:with-nodes))
