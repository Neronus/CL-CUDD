
(in-package :cl-user)

(defpackage cl-cudd.test
  (:use :cl :cl-cudd :fiveam :iterate))

(in-package cl-cudd.test)

(def-suite :cl-cudd)
(in-suite :cl-cudd)

(defvar *models* (directory (merge-pathnames "*.tests" (asdf:system-relative-pathname :cl-cudd "test/"))))

(defun parse-bdd (path)
  (fresh-line)
  (with-manager ()
    (iter (with f = (one-node 'bdd-node))
          (print f)
          (for line in-file path using #'read-line)
          (princ line) (fresh-line)
          (iter (with g = (one-node 'bdd-node))
                (for c in-vector line)
                (for index from (1- (length line)) downto 0)
                (for h = (ecase c
                           (#\0 (node-complement
                                 (make-var 'bdd-node :level index)))
                           (#\1 (make-var 'bdd-node :level index))))
                (print h)
                (setf g (node-and g h))
                (print g)
                (finally
                 (setf f (node-or f g))))
          (print f)
          (finally
           (cl-cudd.baseapi:dump-dot
            ;; *manager*
            ;; bdd
            (manager-pointer *manager*)
            (node-pointer f)
            (namestring (make-pathname :type "dot" :defaults path)))))))

(test bdd
  (dolist (m *models*)
    (finishes
      (parse-bdd m))))
