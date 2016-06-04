(in-package cudd)
(defun node-index (node)
  (cudd-node-read-index (node-pointer node)))

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


(defgeneric node-equal (a b)
  (:documentation
   "Return true iff A and B are the same graph.

Because CUDD caches all diagrams, this is true if and
only if their pointers are the same."))

(defmethod node-equal ((a node) (b node))
  (cffi:pointer-eq (node-pointer a) (node-pointer b)))


(def-cudd-call node-complement ((:add cudd-add-cmpl :bdd cudd-bdd-not) (node :node))
  :generic "Computes the complement of a node a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :add "Computes the complement of an ADD a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
  :bdd "Complements a DD by flipping the complement attribute of the
pointer (the least significant bit).")

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




(def-cudd-call swap-variables ((:add add-swap-variables :bdd bdd-swap-variables) (node :node) x y)
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



(def-cudd-call cofactor ((:add cudd-cofactor :bdd cudd-cofactor) (f :node) (g :node))
  :generic "Computes the cofactor of f with respect to g; g must be the BDD or the ADD of a cube.")


