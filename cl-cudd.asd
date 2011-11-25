;;;; cl-cudd.asd

(asdf:defsystem #:cl-cudd
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :license "BSD Style (see LICENSE)"
  :depends-on (#:cffi
               #:alexandria
               #:trivial-garbage)
  :components ((:file "package")
               (:file "cuddapi" :depends-on ("package"))
               (:file "cudd" :depends-on ("package" "cuddapi"))
  :description ("A two-layered binding to the CUDD binary decision diagram library.

See README.md for more details.")
))

