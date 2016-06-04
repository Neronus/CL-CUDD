
(defsystem cl-cudd.build
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :description "Build CUDD"
  :depends-on (:cffi)
  :perform (compile-op (op c)
                       (format t "~%downloading and building CUDD v3.0.0...~%")
                       (uiop:run-program (format nil "make -C ~a"
                                                 (asdf:system-source-directory :cl-cudd.build))
                                         :output :interactive :error-output :interactive))
  :perform (load-op (op c)
                    (pushnew
                     (asdf:system-relative-pathname :cl-cudd.build "cudd-3.0.0/cudd/.libs/")
                     (symbol-value (read-from-string "cffi:*foreign-library-directories*"))
                     :test #'equal)))

