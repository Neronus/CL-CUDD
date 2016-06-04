
(in-package :cl-cudd.api)

(define-foreign-library libcudd
  (t (:default "libcudd")))

(use-foreign-library libcudd)

