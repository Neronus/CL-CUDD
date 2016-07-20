;;;  Algebraic Decision Diagrams specific codes

(in-package :cudd)

(def-cudd-call add-apply ((:add cudd-add-apply) op (f :node) (g :node))
  :generic "Applies op to the corresponding discriminants of f and g.

The following operations are supported:

+plus+ - Integer and floating point addition
+times+ - Integer and floating point multiplication.
+threshold+ - Threshold operator for Apply (f if f >=g; 0 if f<g)
+set-NZ+ - This operator sets f to the value of g wherever g != 0.
+divide+ - Integer and floating point division.
+minus+ - Integer and floating point substraction.
+minimum+ - Integer and floating point minimum.
+maximum+ - Integer and floating point maximum.
+one-zero-maximum+ - 1 if f > g and 0 otherwise.
+diff+ - Returns NULL if not a terminal case; f op g otherwise, where f op g is plusinfinity if f=g; min(f,g) if f!=g.
+agreement+ - op g,  where f op g is f if f==g; background if f!=g.
+or+ - Disjunction of two 0-1 ADDs.
+nand+ - NAND of two 0-1 ADDs.
+nor+ - NOR of two 0-1 ADDs.
+xor+ - XOR of two 0-1 ADDs.
+xnor+ - XNOR of two 0-1 ADDs.
+equals+ - f op g, where f op g is 1 if f==g, 0 otherwise
+not-equals+ - f op g, where f op g is 1 if f!=g; 0 otherwise
+greater-than+ - f > g, where f op g is 1 if f!=g; 0 otherwise
+greater-than-equals+ -  if f >= g, 0 otherwise
+less-than+ -  < g, where f op g is 1 if f!=g; 0 otherwise
+less-than-equals+ -  <= g, where f op g is 1 if f!=g; 0 otherwise
+pow+ - f to the power of g
+mod+ - f modulo g
+log-x-y+ - log f base g"
  op)

(def-cudd-call add-negate ((:add cudd-add-negate) (node :node))
  :generic "Computes the additive inverse of an ADD.")

(def-cudd-call add-constant ((:add cudd-add-const) value)
  :generic "Retrieves the ADD for constant c if it already exists, or creates a new ADD.")

(def-cudd-call plus-infinity ((:add cudd-read-plus-infinity))
  :generic
  "Return node with value infinity.")

(def-cudd-call minus-infinity ((:add cudd-read-minus-infinity))
  :generic
  "Return node with value -infinity.")


;;; Functions for add-apply
(defparameter +plus+ (cffi:foreign-symbol-pointer "Cudd_addPlus")
  "Integer and floating point addition")
(defparameter +times+ (cffi:foreign-symbol-pointer "Cudd_addTimes")
  "Integer and floating point multiplication.")
(defparameter +threshold+ (cffi:foreign-symbol-pointer "Cudd_addThreshold")
  "Threshold operator for Apply (f if f >=g; 0 if f<g)")
(defparameter +set-NZ+ (cffi:foreign-symbol-pointer "Cudd_addSetNZ")
  "This operator sets f to the value of g wherever g != 0.")
(defparameter +divide+ (cffi:foreign-symbol-pointer "Cudd_addDivide")
  "Integer and floating point division.")
(defparameter +minus+ (cffi:foreign-symbol-pointer "Cudd_addMinus")
  "Integer and floating point substraction.")
(defparameter +minimum+ (cffi:foreign-symbol-pointer "Cudd_addMinimum")
  "Integer and floating point minimum.")
(defparameter +maximum+ (cffi:foreign-symbol-pointer "Cudd_addMaximum")
  "Integer and floating point maximum.")
(defparameter +one-zero-maximum+ (cffi:foreign-symbol-pointer "Cudd_addOneZeroMaximum")
  "1 if f > g and 0 otherwise.")
(defparameter +diff+ (cffi:foreign-symbol-pointer "Cudd_addDiff")
  "f op g , where f op g is plusinfinity if f=g; min(f,g) if f!=g.")
(defparameter +agreement+ (cffi:foreign-symbol-pointer "Cudd_addAgreement")
  "f op g,  where f op g is f if f==g; background if f!=g.")
(defparameter +or+ (cffi:foreign-symbol-pointer "Cudd_addOr")
  "Disjunction of two 0-1 ADDs.")
(defparameter +and+ (cffi:foreign-symbol-pointer "Cudd_addAnd")
  "Conjunction of two 0-1 ADDs.")
(defparameter +nand+ (cffi:foreign-symbol-pointer "Cudd_addNand")
  "NAND of two 0-1 ADDs.")
(defparameter +nor+ (cffi:foreign-symbol-pointer "Cudd_addNor")
  "NOR of two 0-1 ADDs.")
(defparameter +xor+ (cffi:foreign-symbol-pointer "Cudd_addXor")
  "XOR of two 0-1 ADDs.")
(defparameter +xnor+ (cffi:foreign-symbol-pointer "Cudd_addXnor")
  "XNOR of two 0-1 ADDs.")
(defparameter +equals+ (cffi:foreign-symbol-pointer "Cudd_addEquals")
  "f op g, where f op g is 1 if f==g, 0 otherwise")
(defparameter +not-equals+ (cffi:foreign-symbol-pointer "Cudd_addNotEquals")
  "f op g, where f op g is 1 if f!=g; 0 otherwise")
(defparameter +greater-than+ (cffi:foreign-symbol-pointer "Cudd_addGreaterThan")
  "f > g, where f op g is 1 if f!=g; 0 otherwise")
(defparameter +greater-than-equals+ (cffi:foreign-symbol-pointer "Cudd_addGreaterThanEquals")
  "1 if f >= g, 0 otherwise")
(defparameter +less-than+ (cffi:foreign-symbol-pointer "Cudd_addLessThan")
  "f < g, where f op g is 1 if f!=g; 0 otherwise")
(defparameter +less-than-equals+ (cffi:foreign-symbol-pointer "Cudd_addLessThanEquals")
  "f <= g, where f op g is 1 if f!=g; 0 otherwise")
(defparameter +pow+ (cffi:foreign-symbol-pointer "Cudd_addPow")
  "f to the power of g")
(defparameter +mod+ (cffi:foreign-symbol-pointer "Cudd_addMod")
  "f modulo g")
(defparameter +log-x-y+ (cffi:foreign-symbol-pointer "Cudd_addLogXY")
  "log f base g")
