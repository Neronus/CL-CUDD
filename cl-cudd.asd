;;;; cl-cudd.asd

(asdf:defsystem #:cl-cudd
  :serial t
  :depends-on (#:cffi
               #:alexandria
               #:trivial-garbage)
  :components ((:file "package")
               (:file "cuddapi" :depends-on ("package"))
               (:file "cudd" :depends-on ("package" "cuddapi"))
))

