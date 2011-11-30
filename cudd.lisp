;; -*- mode: lisp; mode: Allout; -*-
(in-package cudd)

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.")

;;;_ Manager
(defclass manager ()
  ((pointer :type cffi:foreign-pointer
            :initarg :pointer
            :initform (error "MANAGER needs to wrap a pointer")
            :accessor manager-pointer))
  (:documentation
   "CUDD manager"))

(defmacro with-manager ((&key 
                         (initial-num-vars 0)
                         (initial-num-slots 256)
                         (cache-size 262144)
                         (max-memory 0))
                        &body body)
  "Bind a freshly generated managerto *manager*"
  `(let* ((*manager*
          (make-instance
           'manager
           :pointer
           (cudd-init ,initial-num-vars 0 ,initial-num-slots ,cache-size ,max-memory))))
     (unwind-protect
          (progn ,@body)
       (cudd-quit (manager-pointer *manager*))
       (setf (manager-pointer *manager*) (cffi:null-pointer)))))

;;;_ Wrapped CUDD node
(defclass node ()
  ((pointer :type cffi:foreign-pointer
            :initarg :pointer
            :initform (error "NODE needs to wrap a pointer")
            :reader node-pointer))
  (:documentation
   "Top class of all CUDD nodes."))

(defclass add-node (node)
  ()
  (:documentation
   "Node of an algebraic decision diagram (ADD)"))

(defmacro with-pointers (pointers &body body)
  "Create a binding to pointers using a let-like specification.
Takes care that the nodes stay alive during the body.

Example:
(with-pointers ((f-ptr f)
                (g-ptr g))
  (cudd-add-apply +or+ f-ptr g-ptr))

This is implemented by increasing the reference count of
every node in the body and decreasing it after the body is run"
  (let ((manager (gensym "manager")))
    `(let* ((,manager (manager-pointer *manager*)))
       (progn
         ;; Reference all pointers
         ;; Effectively, we call node-pointer twice on every wrapper.
         ;; The reason is that we have to keep holding on to
         ;; the wrapper until after all referencing is done, i.e.,
         ;; until after the following code block ran.
         ;; Therefore, we first call `(cudd-ref (node-pointer wrapper))`
         ;; for each wrapper. Then later we create the binding for
         ;; the pointers.
         ,@(loop
              :for binding :in pointers
              :unless (and (listp binding) (= (length binding) 2))
              :do (error "Binding ~A is mal-formed" binding)              
              :collect `(cudd-ref (node-pointer ,(cadr binding))))
         (let
             ;; Create bindings
             ,(loop
                 :for binding :in pointers
                 :collect `(,(car binding) (node-pointer ,(cadr binding))))
           (unwind-protect
                (progn
                  ,@body)
             ,@(loop
                  :for binding :in pointers
                  :collect `(cudd-recursive-deref ,manager ,(car binding)))))))))

;; TODO Print more information, like the value if it is a leaf node
(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type (type-of object) :identity nil)
    (format stream "~A " (cudd-node-read-index (node-pointer object)))
    (if (node-constant-p object)
        (format stream "LEAF (VALUE ~A)" (node-value object))
        (format stream "INNER 0x~x" (pointer-address (node-pointer object))))
    (format stream " REF-COUNT ~d" (cudd-node-get-ref-count (manager-pointer *manager*) (node-pointer object)))))

(defclass bdd-node (node)
  ()
  (:documentation
   "Node of a binary decision diagram (BDD)"))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun generic-cudd-function (generic-name arguments generic-docu)
    (flet ((clean-arguments (arguments)
             (mapcar (lambda (arg)
                       (if (and (listp arg) (eq (second arg) :node))
                           (first arg)
                           arg))
                     arguments)))
      `(defgeneric ,generic-name ,(clean-arguments arguments)
         (:documentation ,generic-docu))))



  (defun set-node-finalizer (wrapper pointer)
    "Set the finalizer of WRAPPER to recursively deref POINTER if the
pointer of the manager BOUND AT THE TIME OF THE CALL OF #'SET-NODE-FINALIZER
is not a null pointer."
    (let ((manager *manager*))
      (trivial-garbage:finalize
       wrapper
       (lambda (&rest rest)
         (declare (ignore rest))
         (when (not (null-pointer-p (manager-pointer manager)))
           (when (zerop (cudd-node-get-ref-count (manager-pointer manager) pointer))
             (error "Tried to decrease reference count of node that already
has refcount zero"))
           (cuddapi:cudd-recursive-deref
            (manager-pointer manager) pointer))))))

  (defun wrap-and-finalize (pointer type)
    "Wrap the given pointer in a node-wrapper of type TYPE.
Set the finalizer to call cudd-recursive-deref."
    (let ((wrapper (make-instance type :pointer pointer)))
      (set-node-finalizer wrapper pointer)
      wrapper))

  (defun node-function (generic-name arguments native-function node-type
                        dont-wrap-result)
    (labels ((convert-arguments (arguments)
               (mapcar #'convert-argument arguments))
             (convert-argument (arg)
               (if (and (listp arg) (eq (second arg) :node))
                   `(,(first arg) ,node-type)
                   arg))
             (clean-arguments (arguments)
               (mapcar (lambda (arg)
                         (if (and (listp arg) (eq (second arg) :node))
                             (first arg)
                             arg)) arguments))
             (make-funcall (native arguments)
               `(with-pointers
                    ,(loop
                        :for arg :in arguments
                        :when (and (listp arg) (eq (second arg) :node))
                        :collect `(,(first arg) ,(first arg)))
                    (funcall #',native
                             (manager-pointer *manager*)
                             ,@(clean-arguments arguments)))))
      (with-gensyms (pointer)
        `(defmethod ,generic-name ,(convert-arguments arguments)
           ,(if dont-wrap-result
                (make-funcall native-function arguments)
                `(let* ((,pointer
                         ,(make-funcall native-function arguments)))
                   (wrap-and-finalize ,pointer ',node-type)))))))

  (defun add-function (generic-name arguments add-function dont-wrap)
    (node-function generic-name arguments add-function 'add-node dont-wrap))
  (defun bdd-function (generic-name arguments bdd-function dont-wrap)
    (node-function generic-name arguments bdd-function 'bdd-node dont-wrap))
  (defun common-function (generic-name arguments function dont-wrap)
    (node-function generic-name arguments function t dont-wrap))
  (defun set-add-docu (add-function add-docu)
    `(setf (documentation ',add-function 'function) ,add-docu))
  (defun set-bdd-docu (bdd-function bdd-docu)
    `(setf (documentation ',bdd-function 'function) ,bdd-docu))
  (defun set-common-docu (function docu)
    `(setf (documentation ',function 'function) ,docu))


  (defun find-2list (item 2list)
    (cond
      ((endp 2list) nil)
      ((eq (car 2list) item) (cadr 2list))
      (t (find-2list item (cddr 2list)))))

  (defmacro def-cudd-call (generic-name ((&rest functions) &rest arguments)
                           &rest documentation)
    (let* ((add-function    (find-2list :add functions))
           (bdd-function    (find-2list :bdd functions))
           (common-function (find-2list :common functions))
           (generic-docu    (find-2list :generic documentation))
           (add-docu        (or (find-2list :add documentation) generic-docu))
           (bdd-docu        (or (find-2list :bdd documentation) generic-docu))
           (dont-wrap       (find-2list :dont-wrap-result documentation)))
      `(progn
         ,(generic-cudd-function generic-name arguments generic-docu)
         ,(when add-function
                (add-function generic-name arguments add-function dont-wrap))
         ,(when bdd-function
                (bdd-function generic-name arguments bdd-function dont-wrap))
         ,(when common-function
                (common-function generic-name arguments common-function dont-wrap))
         ,(when (and add-docu add-function)
                (set-add-docu add-function add-docu))
         ,(when (and bdd-docu bdd-function)
                (set-bdd-docu bdd-function bdd-docu))
         ,(when common-function
                (set-common-docu common-function generic-docu))))))

(defmacro with-seq-as-array (array seq &body body)
  (let ((i (gensym "i"))
        (e (gensym "e")))
    `(with-foreign-object (,array :pointer (length ,seq))
       (loop :for ,e :being :each :element :of ,seq
             :for ,i  :from 0
             :do (setf (mem-aref ,array :pointer ,i) (node-pointer ,e)))
       ,@body)))

(def-cudd-call node-or ((:add (lambda (mgr f g) (cudd-add-apply mgr +or+ f g))
                         :bdd cudd-bdd-or) (f :node) (g :node))
  :generic "Disjunction of two 0-1 ADDs or two BDDs."
  :add     "Disjunction of two 0-1 ADDs."
  :bdd     "Disjunction of two BDDs.")

(def-cudd-call disable-gc ((:common cudd-disable-garbage-collection))
               :generic "Disables garbage collection. Garbage
collection is initially enabled. This function may be called to
disable it. However, garbage collection will still occur when a new
node must be created and no memory is left, or when garbage collection
is required for correctness. (E.g., before reordering.)"
               :dont-wrap-result t)

(def-cudd-call enable-gc ((:common cudd-enable-garbage-collection))
  :generic "Enables garbage collection. Garbage collection is
initially enabled. Therefore it is necessary to call this function
only if garbage collection has been explicitly disabled."
  :dont-wrap-result t)

(def-cudd-call set-background ((:common cudd-set-background) bck)
  :generic "Sets the background constant of the manager. It assumes
that the DdNode pointer bck is already referenced."
  :dont-wrap-result t)

;;;_ Algebraic Descision Diagrams

(defun node-index (node)
  (cudd-node-read-index (node-pointer node)))

(defgeneric node-equal (a b)
  (:documentation
   "Return true iff A and B are the same graph.

Because CUDD caches all diagrams, this is true if and
only if their pointers are the same."))

(defmethod node-equal ((a node) (b node))
  (cffi:pointer-eq (node-pointer a) (node-pointer b)))

(def-cudd-call add-constant ((:add cudd-add-const) value)
  :generic "Retrieves the ADD for constant c if it already exists, or creates a new ADD.")


(def-cudd-call node-complement ((:add cudd-add-cmpl :bdd cudd-bdd-not) (node :node))
  :generic "Computes the complement of a node a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :add "Computes the complement of an ADD a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :bdd "Complements a DD by flipping the complement attribute of the
pointer (the least significant bit).")


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

(def-cudd-call if-then-else ((:add cudd-add-ite :bdd cudd-bdd-ite) (f :node) (g :node) (h :node))
  :generic "Return a new DD-node for with F being the top-node, G being the then-branch
and H being the else branch"
  :add "Implements ITE(f,g,h). This procedure assumes that f is a 0-1
  ADD."
  :bdd "Implements ITE(f,g,h).")

(defgeneric cube (nodes type)
  (:documentation "Build a cube from a list of nodes. TYPE defines which nodes we have
in the list of nodes: ADD-NODE or BDD-NODE"))

(defmethod cube (nodes (type (eql 'add-node)))
  (wrap-and-finalize (cudd-add-cube (manager-pointer *manager*) (mapcar #'node-pointer nodes)) 'add-node))

(defmethod cube (nodes (type (eql 'bdd-node)))
  (wrap-and-finalize
   (cudd-bdd-cube (manager-pointer *manager*) (mapcar #'node-pointer nodes))
   'bdd-node))

(defmethod cube (nodes (type (eql 'nil)))
  (cube nodes (type-of (first nodes))))

(defgeneric make-var (type &key level nr)
  (:documentation
   "Creates a new DD variable. At most one of nr and level may be given.

If neither nr nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If nr is given, then retrieves the DD variable with index nr if it already exists,
or creates a new DD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."))

(defmethod make-var ((type (eql 'add-node)) &key level nr)
  (wrap-and-finalize
   (add-var (manager-pointer *manager*) :nr nr :level level)
   'add-node))

(defmethod make-var ((type (eql 'bdd-node)) &key level nr)
  (wrap-and-finalize
   (bdd-var (manager-pointer *manager*) :nr nr :level level)
   'bdd-node))

(def-cudd-call node-constant-p ((:common cudd-node-is-constant) (node :node))
  :generic "Return T if the node is constant, NIL otherwise"
  :dont-wrap-result t)

(def-cudd-call node-value ((:common cudd-node-get-value) (node :node))
  :dont-wrap-result t
  :generic "Return the node value of a constant node")

;; Make sure that we only try to read the value of a constant node
(defmethod node-value :before (node)
  (assert (node-constant-p node)))

(def-cudd-call node-then ((:add cudd-node-get-then :bdd cudd-node-get-then) (node :node))
  :generic "Return the then child of an inner node")

(defmethod node-then :before (node)
  (assert (not (node-constant-p node))))

(def-cudd-call node-else ((:add cudd-node-get-else :bdd cudd-node-get-else) (node :node))
  :generic "Return the else child of an inner node")

(defmethod node-else :before (node)
  (assert (not (node-constant-p node))))

(defmacro with-iterator ((next has-next obj) &body body)
  (let ((state (gensym "state"))
        (gobj (gensym "object")))
    `(let* ((,gobj ,obj)
           (,state
            (cond
              ((typep ,gobj 'array)
               0)
              ((typep ,gobj 'list)
               ,gobj)
              (t (error "~A is neither an array nor a list" ,gobj)))))
       (flet ((,next ()
                (cond
                  ((typep ,gobj 'array)
                   (incf ,state)
                   (aref ,gobj ,state))
                  ((typep ,gobj 'list)
                   (let ((e (car ,state)))
                     (setq ,state (cdr ,state))
                     (car ,state)
                     e))))
              (,has-next ()
                (cond
                  ((typep ,gobj 'array)
                   (< ,state (length ,gobj)))
                  ((typep ,gobj 'list)
                   (not (null ,gobj))))))
         ,@body))))

(def-cudd-call node-permute ((:add cudd-add-permute :bdd cudd-bdd-permute) (node :node) permutation)
  :generic
  "Given a permutation in sequence permut, creates a new DD with
  permuted variables. There should be an entry in array permut for
  each variable in the manager. The i-th entry of permut holds the
  index of the variable that is to substitute the i-th variable.")


;;; If the permutation is given as any kind of sequence (this
;;; is what we actually expect), then we first turn the sequence
;;; into an array and then call the actual implementation
(defmethod node-permute :around (node (permutation sequence))
  (with-foreign-object (array :pointer (length permutation))
    (loop :for node :being :each :element :of permutation
          :for i    :from  0
          :do (setf (mem-aref array :pointer i) (node-pointer node)))
    (node-permute node array)))

(defun add-swap-variables (mgr node x y)
  (assert (= (length x) (length y)))
  (with-seq-as-array xa x
    (with-seq-as-array ya y
      (cudd-add-swap-variables mgr node xa ya (length x)))))

(defun bdd-swap-variables (mgr node x y)
  (assert (= (length x) (length y)))
  (with-seq-as-array xa x
    (with-seq-as-array ya y
      (cudd-bdd-swap-variables mgr node xa ya (length x)))))


(def-cudd-call swap-variables ((:add add-swap-variables
                                :bdd bdd-swap-variables) (node :node) x y)
  :generic
  "Swaps two sets of variables of the same size (x and y) in the ADD
   f. The size is given by n. The two sets of variables are assumed to
   be disjoint.")


(def-cudd-call exist-abstract ((:add cudd-add-exist-abstract :bdd cudd-bdd-exist-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from f by summing over all
  possible values taken by the variables. Returns the abstracted
  DD.")

(def-cudd-call or-abstract ((:add cudd-add-or-abstract :bdd cudd-bdd-exist-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the 0-1 ADD f by
  taking the disjunction over all possible values taken by the
  variables.

If abstracting an ADD, we assume that it is an 0-1-ADD
")

(def-cudd-call univ-abstract ((:add cudd-add-univ-abstract :bdd cudd-bdd-univ-abstract)
                              (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the DD f by
  taking the conjunction over all possible values taken by the
  variables.

If abstracting an ADD, we assume that it is an 0-1-ADD")

(def-cudd-call min-abstract ((:add cudd-add-min-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the ADD f by
  taking the minimum over all possible values taken by the
  variables.")

(def-cudd-call max-abstract ((:add cudd-add-max-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the ADD f by
  taking the maximum over all possible values taken by the
  variables.")

(def-cudd-call zero-node ((:common (lambda (dd type)
                                     (wrap-and-finalize
                                      (cudd-read-zero dd)
                                      type))) type)
  :generic "Return the zero node."
  :dont-wrap-result t)

(def-cudd-call one-node ((:common (lambda (dd type)
                                     (wrap-and-finalize
                                      (cudd-read-one dd)
                                      type)))
                         type)
  :generic "Return the one node."
  :dont-wrap-result t)

;;;_. Functions for add-apply
(handler-bind
    ((sb-ext:defconstant-uneql
      (lambda (c) (continue c))))
  (defconstant +plus+ (cffi:foreign-symbol-pointer "Cudd_addPlus")
    "Integer and floating point addition")
  (defconstant +times+ (cffi:foreign-symbol-pointer "Cudd_addTimes")
    "Integer and floating point multiplication.")
  (defconstant +threshold+ (cffi:foreign-symbol-pointer "Cudd_addThreshold")
    "Threshold operator for Apply (f if f >=g; 0 if f<g)")
  (defconstant +set-NZ+ (cffi:foreign-symbol-pointer "Cudd_addSetNZ")
    "This operator sets f to the value of g wherever g != 0.")
  (defconstant +divide+ (cffi:foreign-symbol-pointer "Cudd_addDivide")
    "Integer and floating point division.")
  (defconstant +minus+ (cffi:foreign-symbol-pointer "Cudd_addMinus")
    "Integer and floating point substraction.")
  (defconstant +minimum+ (cffi:foreign-symbol-pointer "Cudd_addMinimum")
    "Integer and floating point minimum.")
  (defconstant +maximum+ (cffi:foreign-symbol-pointer "Cudd_addMaximum")
    "Integer and floating point maximum.")
  (defconstant +one-zero-maximum+ (cffi:foreign-symbol-pointer "Cudd_addOneZeroMaximum")
    "1 if f > g and 0 otherwise.")
  (defconstant +diff+ (cffi:foreign-symbol-pointer "Cudd_addDiff")
    "f op g , where f op g is plusinfinity if f=g; min(f,g) if f!=g.")
  (defconstant +agreement+ (cffi:foreign-symbol-pointer "Cudd_addAgreement")
    "f op g,  where f op g is f if f==g; background if f!=g.")
  (defconstant +or+ (cffi:foreign-symbol-pointer "Cudd_addOr")
    "Disjunction of two 0-1 ADDs.")
  (defconstant +and+ (cffi:foreign-symbol-pointer "Cudd_addAnd")
    "Conjunction of two 0-1 ADDs.")
  (defconstant +nand+ (cffi:foreign-symbol-pointer "Cudd_addNand")
    "NAND of two 0-1 ADDs.")
  (defconstant +nor+ (cffi:foreign-symbol-pointer "Cudd_addNor")
    "NOR of two 0-1 ADDs.")
  (defconstant +xor+ (cffi:foreign-symbol-pointer "Cudd_addXor")
    "XOR of two 0-1 ADDs.")
  (defconstant +xnor+ (cffi:foreign-symbol-pointer "Cudd_addXnor")
    "XNOR of two 0-1 ADDs.")
  (defconstant +equals+ (cffi:foreign-symbol-pointer "Cudd_addEquals")
    "f op g, where f op g is 1 if f==g, 0 otherwise")
  (defconstant +not-equals+ (cffi:foreign-symbol-pointer "Cudd_addNotEquals")
    "f op g, where f op g is 1 if f!=g; 0 otherwise")
  (defconstant +greater-than+ (cffi:foreign-symbol-pointer "Cudd_addGreaterThan")
     "f > g, where f op g is 1 if f!=g; 0 otherwise")
  (defconstant +greater-than-equals+ (cffi:foreign-symbol-pointer "Cudd_addGreaterThanEquals")
    "1 if f >= g, 0 otherwise")
  (defconstant +less-than+ (cffi:foreign-symbol-pointer "Cudd_addLessThan")
    "f < g, where f op g is 1 if f!=g; 0 otherwise")
  (defconstant +less-than-equals+ (cffi:foreign-symbol-pointer "Cudd_addLessThanEquals")
    "f <= g, where f op g is 1 if f!=g; 0 otherwise")
  (defconstant +pow+ (cffi:foreign-symbol-pointer "Cudd_addPow")
    "f to the power of g")
  (defconstant +mod+ (cffi:foreign-symbol-pointer "Cudd_addMod")
    "f modulo g")
  (defconstant +log-x-y+ (cffi:foreign-symbol-pointer "Cudd_addLogXY")
    "log f base g"))


;;;_ Binary decision Diagrams
(defun bdd->add (bdd)
  "Converts a BDD to a 0-1 ADD"
  (assert (typep bdd 'bdd-node))
  (wrap-and-finalize
   (cudd-bdd-to-add (manager-pointer *manager*) (node-pointer bdd))
   'add-node))

(defun add->bdd (add)
  "Converts an ADD to a BDD by replacing all discriminants different from 0 with 1."
  (assert (typep add 'add-node))
  (wrap-and-finalize
   (cudd-add-bdd-pattern (manager-pointer *manager*) (node-pointer add))
   'bdd-node))


;;;_ Common for ADD and BDD
(def-cudd-call cofactor ((:add cudd-cofactor :bdd cudd-cofactor) (f :node) (g :node))
  :generic "Computes the cofactor of f with respect to g; g must be the BDD or the ADD of a cube.")

(defun count-leaves (node)
  "Counts the number of leaves in a DD."
  (cudd-count-leaves (node-pointer node)))

(defun dag-size (node)
  "Counts the number of nodes in a DD."
  (cudd-dag-size (node-pointer node)))

#|
(defun dd-eval (node assignment)
  "Finds the value of a DD for a given variable assignment. The
variable assignment is passed in a sequence of int's, that should
specify a boolean value for each variable in the support of the
function. Returns a constant node."
  (let ((n (length assignment)))
    (cffi:with-foreign-object (array :int n)
      (loop :for i :from 0
            :for v in assignment
            :do (setf (cffi:mem-aref array :int i)
                      (if v 1 0)))
      (ref-call (cudd-eval node array) array))))

(def-cudd-call find-essential (cudd-find-essential f)
  "Returns the cube of the essential variables. A positive literal
means that the variable must be set to 1 for the function to be 1. A
negative literal means that the variable must be set to 0 for the
function to be 1. Returns a pointer to the cube BDD if successful;
NULL otherwise.")

(defun print-debug (node n pr)
  "Prints to the standard output a DD and its statistics. The statistics include the number of nodes, the number of leaves, and the number of minterms. (The number of minterms is the number of assignments to the variables that cause the function to be different from the logical zero (for BDDs) and from the background value (for ADDs.) The statistics are printed if pr > 0. Specifically:
pr = 0 : prints nothing
pr = 1 : prints counts of nodes and minterms
pr = 2 : prints counts + disjoint sum of product
pr = 3 : prints counts + list of nodes
pr > 3 : prints counts + disjoint sum of product + list of nodes
For the purpose of counting the number of minterms, the function is supposed to depend on n variables."
  (cudd-print-debug (manager-pointer *manager*) node (node-pointer n) pr))

(defun dd-constantp (dd)
  "Return T if DD is a leaf node, NIL otherwise"
  (= 0 (cuddapi:cudd-is-non-constant dd)))

(def-cudd-call count-minterm (cudd-count-minterm nodes nvars)
  "Counts the number of minterms of a DD. The function is assumed to
  depend on nvars variables. The minterm count is represented as a
  double, to allow for a larger number of variables. Returns the
  number of minterms of the function rooted at node"
  nvars :result)

|#
