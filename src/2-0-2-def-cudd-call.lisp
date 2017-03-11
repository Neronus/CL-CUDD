;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Utilities for def-cudd-call

(defun generic-cudd-function (generic-name arguments generic-docu)
  (flet ((clean-arguments (arguments)
           (mapcar (lambda (arg)
                     (if (and (listp arg) (eq (second arg) :node))
                         (first arg)
                         arg))
                   arguments)))
    `(defgeneric ,generic-name ,(clean-arguments arguments)
       ,@(when generic-docu
           `((:documentation ,generic-docu))))))



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
           (error "Tried to decrease reference count of node that already has refcount zero"))
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

