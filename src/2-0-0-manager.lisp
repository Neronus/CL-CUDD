;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.")

;; TODO: convert to struct
(defclass manager ()
  ((pointer :type cffi:foreign-pointer
            :initarg :pointer
            :initform (error "MANAGER needs to wrap a pointer")
            :accessor manager-pointer))
  (:documentation
   "A boxed CUDD manager class"))

(defmethod cffi:translate-to-foreign (pointer (manager manager))
  (manager-pointer manager))

(defmacro with-manager ((&key 
                         (initial-num-vars 0)
                         (initial-num-slots 256)
                         (cache-size 262144)
                         (max-memory 0))
                        &body body)
  "Bind a freshly generated manager to *manager*"
  `(let* ((*manager*
          (make-instance
           'manager
           :pointer
           (cudd-init ,initial-num-vars 0 ,initial-num-slots ,cache-size ,max-memory))))
     (unwind-protect
          (progn ,@body)
       (cudd-quit (manager-pointer *manager*))
       (setf (manager-pointer *manager*) (cffi:null-pointer)))))

