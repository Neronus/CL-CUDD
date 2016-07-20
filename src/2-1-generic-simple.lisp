(in-package :cudd)

(def-cudd-call node-or ((:add (lambda (mgr f g) (cudd-add-apply mgr +or+ f g))
                         :bdd cudd-bdd-or) (f :node) (g :node))
  :generic "Disjunction of two 0-1 ADDs or two BDDs."
  :add     "Disjunction of two 0-1 ADDs."
  :bdd     "Disjunction of two BDDs.")

(def-cudd-call node-and ((:add (lambda (mgr f g) (cudd-add-apply mgr +times+ f g))
                         :bdd cudd-bdd-and) (f :node) (g :node))
  :generic "Conjunction of two 0-1 ADDs or two BDDs."
  :add     "Conjunction of two 0-1 ADDs."
  :bdd     "Conjunction of two BDDs.")

(def-cudd-call node-complement ((:add cudd-add-cmpl :bdd cudd-bdd-not)
                                (node :node))
  :generic "Computes the complement of a node a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :add "Computes the complement of an ADD a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :bdd "Complements a DD by flipping the complement attribute of the
pointer (the least significant bit).")

(def-cudd-call if-then-else ((:add cudd-add-ite :bdd cudd-bdd-ite)
                             (f :node) (g :node) (h :node))
  :generic "Return a new DD-node for with F being the top-node, G being the then-branch
and H being the else branch"
  :add "Implements ITE(f,g,h). This procedure assumes that f is a 0-1
  ADD."
  :bdd "Implements ITE(f,g,h).")

(defgeneric cube (nodes type)
  (:documentation "Build a cube from a list of nodes. TYPE defines which nodes we have
in the list of nodes: ADD-NODE or BDD-NODE"))

(defmethod cube (nodes (type (eql 'add-node)))
  (wrap-and-finalize
   (cudd-add-cube (manager-pointer *manager*) (mapcar #'node-pointer nodes))
   'add-node))

(defmethod cube (nodes (type (eql 'bdd-node)))
  (wrap-and-finalize
   (cudd-bdd-cube (manager-pointer *manager*) (mapcar #'node-pointer nodes))
   'bdd-node))

(defmethod cube (nodes (type (eql 'nil)))
  (cube nodes (type-of (first nodes))))

(defgeneric make-var (type &key level index)
  (:documentation
   "Creates a new DD variable. At most one of index and level may be given.

If neither index nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the DD variable with the index if it already exists,
or creates a new DD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."))

(defmethod make-var ((type (eql 'add-node)) &key level index)
  (wrap-and-finalize
   (add-var (manager-pointer *manager*) :index index :level level)
   'add-node))

(defmethod make-var ((type (eql 'bdd-node)) &key level index)
  (wrap-and-finalize
   (bdd-var (manager-pointer *manager*) :index index :level level)
   'bdd-node))

(def-cudd-call node-then ((:add cudd-node-get-then :bdd cudd-node-get-then)
                          (node :node))
  :generic "Return the then child of an inner node")

(defmethod node-then :before (node)
  (assert (not (node-constant-p node))))

(def-cudd-call node-else ((:add cudd-node-get-else :bdd cudd-node-get-else)
                          (node :node))
  :generic "Return the else child of an inner node")

(defmethod node-else :before (node)
  (assert (not (node-constant-p node))))


(def-cudd-call zero-node ((:add (lambda (dd type) (wrap-and-finalize (cudd-read-zero dd) type))
                           :bdd (lambda (dd type) (wrap-and-finalize (cudd-read-logic-zero dd) type)))
                          type)
  :add "Return the arithmetic zero node (0.0d0)."
  :bdd "Return the logical zero node (boolean 0)."
  :dont-wrap-result t)

(def-cudd-call one-node ((:common (lambda (dd type)
                                     (wrap-and-finalize
                                      (cudd-read-one dd)
                                      type)))
                         type)
  :generic "Return the one node."
  :dont-wrap-result t)

