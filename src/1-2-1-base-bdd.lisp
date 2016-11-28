;;; BDD : Binary Decision Diagram
(in-package :cl-cudd.baseapi)

(defun cudd-bdd-not (manager node)
  "Mark the node as a negated node. See CUDD documentation Sec.4.3 Complement Arc"
  (declare (ignore manager))
  ;; TODO What happens on big-endian machines?
  (let ((result (make-pointer (logxor 1 (pointer-address node)))))
    (cudd-ref result)
    result))

(defun cudd-bdd-cube (manager vars)
  "Build an bdd cube out of the variables."
  (let ((n (length vars)))
    (with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
            :do (setf (mem-aref array :pointer i) v))
      (cudd-bdd-compute-cube manager array (null-pointer) n))))

(defun bdd-var (manager &key index level)
  "Creates a new BDD variable. At most one of INDEX and LEVEL may be given.

If neither INDEX nor LEVEL are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the BDD variable with the index if it already exists,
or creates a new BDD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and index level)
    (error "BDD-VAR accepts at most one of I and LEVEL"))
  (cond
    (index (cudd-bdd-ith-var manager index))
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))





