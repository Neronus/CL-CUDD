
(in-package :cl-user)

(defpackage cl-cudd.test
  (:use :cl :cl-cudd :cl-cudd.baseapi :fiveam :iterate)
  (:shadow :next))

(in-package cl-cudd.test)

(def-suite :cl-cudd)
(in-suite :cl-cudd)

(defun models ()
  (directory (merge-pathnames "*.tests" (asdf:system-relative-pathname :cl-cudd "test/"))))

(defun parse-bdd (path)
  (fresh-line)
  (flet ((dump (name f)
           (cl-cudd.baseapi:dump-dot
            ;; *manager*
            ;; bdd
            (manager-pointer *manager*)
            (cudd-regular (node-pointer f))
            (namestring (make-pathname :name name :type "dot" :defaults path)))))
    (with-manager ()
      (iter (with f = (zero-node 'bdd-node))
            (for line in-file path using #'read-line)
            (print line)
            (iter (with g = (one-node 'bdd-node))
                  (for c in-vector line)
                  (for index from 0)
                  (for h = (ecase c
                             (#\0 (node-complement
                                   (make-var 'bdd-node :nr index)))
                             (#\1 (make-var 'bdd-node :nr index))))
                  (setf g (node-and h g))
                  (finally
                   (setf f (node-or f g))))
            (print f)
            ;; (finish)
            (finally
             (dump (pathname-name path) f)
             ;; since BDDs may contain complemented edges, it is slightly hard to understand.
             ;; Usually converting it into ADDs will improve the output
             (dump (format nil "~a-as-ADD" (pathname-name path))
                   (bdd->add f)))))))

(test bdd
  (dolist (m (models))
    (finishes
      (parse-bdd m)))
  (uiop:run-program (format nil "make -C ~a" (asdf:system-relative-pathname :cl-cudd "test/"))
                    :ignore-error-status t
                    :output t
                    :error-output t))
