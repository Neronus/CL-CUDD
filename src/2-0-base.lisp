;;; base class definitions and macros for defining APIs
(in-package cudd)

;;; Manager

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.")

(defclass manager ()
  ((pointer :type cffi:foreign-pointer
            :initarg :pointer
            :initform (error "MANAGER needs to wrap a pointer")
            :accessor manager-pointer))
  (:documentation
   "CUDD manager"))

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

;;; Wrapped CUDD node
(defclass node ()
  ((pointer :type cffi:foreign-pointer
            :initarg :pointer
            :initform (error "NODE needs to wrap a pointer")
            :reader node-pointer))
  (:documentation
   "Top class of all CUDD nodes."))

(defmacro with-pointers (pointers &body body)
  "Create a binding to pointers using a let-like specification.
Takes care that the nodes stay alive during the body.

Example:
 (with-pointers ((f-ptr f)
                 (g-ptr g))
   (cudd-add-apply +or+ f-ptr g-ptr))

This is implemented by increasing the reference count of
every node in the body and decreasing it after the body is run"
  (let ((manager (gensym "manager")))
    `(let* ((,manager (manager-pointer *manager*)))
       (progn
         ;; Reference all pointers
         ;; Effectively, we call node-pointer twice on every wrapper.
         ;; The reason is that we have to keep holding on to
         ;; the wrapper until after all referencing is done, i.e.,
         ;; until after the following code block ran.
         ;; Therefore, we first call `(cudd-ref (node-pointer wrapper))`
         ;; for each wrapper. Then later we create the binding for
         ;; the pointers.
         ,@(loop
              :for binding :in pointers
              :unless (and (listp binding) (= (length binding) 2))
              :do (error "Binding ~A is mal-formed" binding)              
              :collect `(cudd-ref (node-pointer ,(cadr binding))))
         (let
             ;; Create bindings
             ,(loop
                 :for binding :in pointers
                 :collect `(,(car binding) (node-pointer ,(cadr binding))))
           (unwind-protect
                (progn
                  ,@body)
             ,@(loop
                  :for binding :in pointers
                  :collect `(cudd-recursive-deref ,manager ,(car binding)))))))))

;; TODO Print more information, like the value if it is a leaf node
(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type (type-of object) :identity nil)
    (format stream "~A " (cudd-node-read-index (node-pointer object)))
    (if (node-constant-p object)
        (format stream "LEAF (VALUE ~A)" (node-value object))
        (format stream "INNER 0x~x" (pointer-address (node-pointer object))))
    (format stream " REF-COUNT ~d" (cudd-node-get-ref-count (manager-pointer *manager*) (node-pointer object)))))

(defclass add-node (node)
  ()
  (:documentation
   "Node of an algebraic decision diagram (ADD)"))

(defclass bdd-node (node)
  ()
  (:documentation
   "Node of a binary decision diagram (BDD)"))

;;; Utilities for def-cudd-call

(defun generic-cudd-function (generic-name arguments generic-docu)
  (flet ((clean-arguments (arguments)
           (mapcar (lambda (arg)
                     (if (and (listp arg) (eq (second arg) :node))
                         (first arg)
                         arg))
                   arguments)))
    `(defgeneric ,generic-name ,(clean-arguments arguments)
       (:documentation ,generic-docu))))



(defun set-node-finalizer (wrapper pointer)
  "Set the finalizer of WRAPPER to recursively deref POINTER if the
pointer of the manager BOUND AT THE TIME OF THE CALL OF #'SET-NODE-FINALIZER
is not a null pointer."
  (let ((manager *manager*))
    (trivial-garbage:finalize
     wrapper
     (lambda (&rest rest)
       (declare (ignore rest))
       (when (not (null-pointer-p (manager-pointer manager)))
         (when (zerop (cudd-node-get-ref-count (manager-pointer manager) pointer))
           (error "Tried to decrease reference count of node that already
has refcount zero"))
         (cudd-recursive-deref
          (manager-pointer manager) pointer))))))

(defun wrap-and-finalize (pointer type)
  "Wrap the given pointer in a node-wrapper of type TYPE.
Set the finalizer to call cudd-recursive-deref."
  (let ((wrapper (make-instance type :pointer pointer)))
    (set-node-finalizer wrapper pointer)
    wrapper))

(defun node-function (generic-name arguments native-function node-type
                      dont-wrap-result)
  (labels ((convert-arguments (arguments)
             (mapcar #'convert-argument arguments))
           (convert-argument (arg)
             (if (and (listp arg) (eq (second arg) :node))
                 `(,(first arg) ,node-type)
                 arg))
           (clean-arguments (arguments)
             (mapcar (lambda (arg)
                       (if (and (listp arg) (eq (second arg) :node))
                           (first arg)
                           arg)) arguments))
           (make-funcall (native arguments)
             `(with-pointers
                  ,(loop
                     :for arg :in arguments
                     :when (and (listp arg) (eq (second arg) :node))
                     :collect `(,(first arg) ,(first arg)))
                (funcall #',native
                         (manager-pointer *manager*)
                         ,@(clean-arguments arguments)))))
    (with-gensyms (pointer)
      `(defmethod ,generic-name ,(convert-arguments arguments)
         ,(if dont-wrap-result
              (make-funcall native-function arguments)
              `(let* ((,pointer
                       ,(make-funcall native-function arguments)))
                 (wrap-and-finalize ,pointer ',node-type)))))))

(defun add-function (generic-name arguments add-function dont-wrap)
  (node-function generic-name arguments add-function 'add-node dont-wrap))
(defun bdd-function (generic-name arguments bdd-function dont-wrap)
  (node-function generic-name arguments bdd-function 'bdd-node dont-wrap))
(defun common-function (generic-name arguments function dont-wrap)
  (node-function generic-name arguments function t dont-wrap))
(defun set-add-docu (add-function add-docu)
  (when (symbolp add-function)
    `(setf (documentation ',add-function 'function) ,add-docu)))
(defun set-bdd-docu (bdd-function bdd-docu)
  (when (symbolp bdd-function)
    `(setf (documentation ',bdd-function 'function) ,bdd-docu)))
(defun set-common-docu (function docu)
  (when (symbolp function)
    `(setf (documentation ',function 'function) ,docu)))


(defun find-2list (item 2list)
  (cond
    ((endp 2list) nil)
    ((eq (car 2list) item) (cadr 2list))
    (t (find-2list item (cddr 2list)))))

(defmacro def-cudd-call (generic-name ((&rest functions) &rest arguments)
                         &rest documentation)
  (let* ((add-function    (find-2list :add functions))
         (bdd-function    (find-2list :bdd functions))
         (common-function (find-2list :common functions))
         (generic-docu    (find-2list :generic documentation))
         (add-docu        (or (find-2list :add documentation) generic-docu))
         (bdd-docu        (or (find-2list :bdd documentation) generic-docu))
         (dont-wrap       (find-2list :dont-wrap-result documentation)))
    `(progn
       ,(generic-cudd-function generic-name arguments generic-docu)
       ,(when add-function
          (add-function generic-name arguments add-function dont-wrap))
       ,(when bdd-function
          (bdd-function generic-name arguments bdd-function dont-wrap))
       ,(when common-function
          (common-function generic-name arguments common-function dont-wrap))
       ,(when (and add-docu add-function)
          (set-add-docu add-function add-docu))
       ,(when (and bdd-docu bdd-function)
          (set-bdd-docu bdd-function bdd-docu))
       ,(when common-function
          (set-common-docu common-function generic-docu)))))

(defmacro with-seq-as-array (array seq &body body)
  (let ((i (gensym "i"))
        (e (gensym "e")))
    `(with-foreign-object (,array :pointer (length ,seq))
       (let ((,i 0))
         (map nil
              (lambda (,e)
                (setf (mem-aref ,array :pointer ,i) (node-pointer ,e)
                      ,i (+ ,i 1)))
              ,seq
              ))

       #+nil (loop :for ,e :being :each :element :of ,seq
          :for ,i  :from 0
          :do (setf (mem-aref ,array :pointer ,i) (node-pointer ,e)))
       ,@body)))

