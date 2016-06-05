;;; 
(in-package :cl-cudd.baseapi)

(defun cudd-bdd-not (manager node)
  (declare (ignore manager))
  ;; TODO What happens on big-endian machines?
  (let ((result (make-pointer (logxor 1 (pointer-address node)))))
    (cudd-ref result)
    result))

(defun cudd-add-cube (manager vars)
  "Build an add cube out of the variables."
  (let ((n (length vars)))
    (with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
            :do (setf (mem-aref array :pointer i) v))
      (cudd-add-compute-cube manager array (null-pointer) n))))

(defun cudd-bdd-cube (manager vars)
  "Build an bdd cube out of the variables."
  (let ((n (length vars)))
    (with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
            :do (setf (mem-aref array :pointer i) v))
      (cudd-bdd-compute-cube manager array (null-pointer) n))))

(defun add-var (manager &key nr level)
  "Creates a new ADD variable. At most one of nr and level may be given.

If neither nr nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If nr is given, then retrieves the ADD variable with index nr if it already exists,
or creates a new ADD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and nr level)
    (error "ADD-VAR accepts at most one of I and LEVEL"))
  (cond
    (nr (cudd-add-ith-var manager nr))
    (level (cudd-add-new-var-at-level manager level))
    (t (cudd-add-new-var manager))))

(defun bdd-var (manager &key nr level)
  "Creates a new BDD variable. At most one of nr and level may be given.

If neither nr nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If nr is given, then retrieves the BDD variable with index nr if it already exists,
or creates a new BDD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and nr level)
    (error "BDD-VAR accepts at most one of I and LEVEL"))
  (cond
    (nr (cudd-bdd-ith-var manager nr))
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))

(defun cudd-regular (node)
  (let ((addr (pointer-address node)))
    (setf (ldb (byte 1 0) addr) 0)
    (make-pointer addr)))

(defparameter +cudd-max-index+
              (if (and (= +sizeof-void-p+ 8) (= +sizeof-int+ 4))
                  ;; ((unsigned int) ~0) >> 1
                  (- (expt 2 31) 1)
                  ;; ((unsigned short) ~0)
                  (- (expt 2 16) 1)))

(defun cudd-node-is-constant (manager node)
  (declare (ignore manager))
  (let ((regular (cudd-regular node)))
    (= (cudd-node-read-index regular) +cudd-max-index+)))

(defun cudd-node-get-value (manager node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (declare (ignore manager))
  (foreign-slot-value
   (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
   '(:union dd-node-type) 'value))

(defun cudd-node-get-then (manager node)
  "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
  (let ((result
         (foreign-slot-value
          (foreign-slot-value
           (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
           '(:union dd-node-type) 'kids)
          '(:struct dd-children) 'T)))
    (cudd-ref result)
    result))

(defun cudd-node-get-else (manager node)
  "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
  (declare (ignore manager))
  (let ((result
         (foreign-slot-value
          (foreign-slot-value
           (foreign-slot-pointer (cudd-regular node) '(:struct dd-node) 'type)
           '(:union dd-node-type) 'kids)
          '(:struct dd-children) 'E)))
    (cudd-ref result)
    result))

(defun cudd-node-get-ref-count (manager node)
  "Return the reference count of the node."
  (declare (ignore manager))
  (foreign-slot-value (cudd-regular node) '(:struct dd-node) 'ref))



