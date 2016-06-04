
(defsystem cl-cudd.build
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :description "Build CUDD"
  :perform (compile-op (op c)
                       (uiop:run-program (format nil "make -C ~a"
                                                 (asdf:system-source-directory :cl-cudd.build))
                                         :output t :error-output t)))
