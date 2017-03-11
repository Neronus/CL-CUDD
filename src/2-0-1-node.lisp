;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Wrapped CUDD node
(defclass node ()
  ((pointer :type foreign-pointer
            :initarg :pointer
            :initform (error "NODE needs to wrap a pointer")
            :reader node-pointer))
  (:documentation
   "A boxed CUDD node class. Top class of all CUDD nodes."))

(defmethod cffi:translate-to-foreign (pointer (node node))
  (node-pointer node))

(defmacro with-pointers (pointers &body body)
  "Create a binding to pointers using a let-like specification.
Takes care that the nodes stay alive during the body.

Example:
 (with-pointers ((f-ptr f)
                 (g-ptr g))
   (cudd-add-apply +or+ f-ptr g-ptr))

This is implemented by increasing the reference count of
every node in the body and decreasing it after the body is run"
  (let ((manager (gensym "MANAGER")))
    `(let* ((,manager (manager-pointer *manager*)))
       (declare (ignorable ,manager))
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
              ;; increase refcount
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

(defun node-index (node)
  (cudd-node-read-index (node-pointer node)))

(defun node-equal (a b)
  "Return true iff A and B are the same graph.

Because CUDD caches all diagrams, this is true if and
only if their pointers are the same."
  (check-type a node)
  (check-type b node)
  (cffi:pointer-eq (node-pointer a) (node-pointer b)))

(defun node-constant-p (node)
  "return t if the node is constant, nil otherwise"
  (with-pointers ((node node))
     (cudd-node-is-constant (manager-pointer *manager*) node)))

(defun node-value (node)
  "Return the node value of a constant node"
  ;; Make sure that we only try to read the value of a constant node
  (assert (node-constant-p node))
  (with-pointers ((node node))
    (cudd-node-get-value (manager-pointer *manager*) node)))


(defclass add-node (node)
  ()
  (:documentation
   "Node of an algebraic decision diagram (ADD)"))

(defclass bdd-node (node)
  ()
  (:documentation
   "Node of a binary decision diagram (BDD)"))

