;;; functions on a raw cudd-node pointer
(in-package :cl-cudd.baseapi)

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



