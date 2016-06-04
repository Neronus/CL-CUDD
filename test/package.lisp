
(in-package :cl-user)

(defpackage cl-cudd.test
  (:use :cl :cl-cudd :fiveam :iterate))

(in-package cl-cudd.test)

(def-suite :cl-cudd)
(in-suite :cl-cudd)

(defvar *models* (directory (merge-pathnames "*.tests" (asdf:system-relative-pathname :cl-cudd "test/"))))

(defun parse-bdd (path)
  (with-manager ()
    (let ((bdd (one-node 'bdd-node)))
      (iter (for line in-file path using #'read-line)
            (iter (for c in-vector line)
                  (for index from (1- (length line)) downto 0)
                  (setf bdd (node-and bdd
                                      (ecase c
                                        (#\0 (make-var 'bdd-node :nr index))
                                        (#\1 (node-complement (make-var 'bdd-node :nr index))))))))
      (print bdd)
      (cl-cudd.baseapi:dump-dot
       ;; *manager*
       ;; bdd
       (manager-pointer *manager*)
       (node-pointer bdd)
       (namestring (make-pathname :type "dot" :defaults path))))))

(test bdd
  (dolist (m *models*)
    (finishes
      (parse-bdd m))))
