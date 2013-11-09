/* swig interface wrapper file for cffi binding generation -*- lisp -*- */
/* swig -cffi interface for CUDD */
/* (c) 2009 Utz-Uwe Haus */
/* (c) 2011 Christian von Essen */

%module "cuddapi"

%feature("intern_function","1");   // use swig-lispify
%feature("export");                // export wrapped things

%insert("lisphead")%{
;;; Auto-generated -*- lisp -*- file
;;; generated from $Id:$
(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize (speed 3) (debug 1) (safety 1))))

(cl:in-package :swig-macros)

(cl:defun swig-lispify (name flag cl:&optional (package (find-package :cuddapi)))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      ((cl:char-equal c #\-)
                       (helper (cl:cdr lst) '- (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))
%}

%{

/* includes that SWIG needs to see to parse the cudd.h file go here */

%}

%insert ("swiglisp") %{


(cl:in-package :cuddapi)
%}
%insert ("swiglisp") %{
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:in-package :cuddapi)

  (define-condition cudd-condition (condition) ())
  (define-condition cudd-error (cudd-condition error) ())
  (define-condition cudd-null-pointer-error (cudd-error) ()
    (:report (lambda (condition stream)
                 (declare (ignore condition))
                 (format stream "A CUDD function returned a null-pointer"))))
  (define-condition cudd-null-manager-error (cudd-error) ()
    (:report (lambda (condition stream)
                 (declare (ignore condition))
                 (format stream "Could not construct a CUDD manager"))))

  (defctype node :pointer
    "A DD-node returned by CUDD")
  (defctype manager :pointer
    "A manager of CUDD")

  
  (define-foreign-type node-type ()
    ()
    (:actual-type :pointer)
    (:simple-parser node))

  (define-foreign-type manager-type ()
    ()
    (:actual-type :pointer)
    (:simple-parser manager))

  (defmethod expand-to-foreign (value (type node-type))
    value)
  (defmethod expand-from-foreign (value (type node-type))
    (let ((gvalue (gensym "value")))
      `(let ((,gvalue ,value))
         (if (null-pointer-p ,gvalue)
             (error 'cudd-null-pointer-error)
             (progn
               (cudd-ref ,gvalue)
               ,gvalue)))))

  (defmethod expand-to-foreign (value (type manager-type))
    value)
  (defmethod expand-from-foreign (value (type manager-type))
    (let ((gvalue (gensym "value")))
      `(let ((,gvalue ,value))
         (if (null-pointer-p ,gvalue)
             (error 'cudd-null-manager-error)
             ,gvalue)))))

%}

%define TYPEMAP_WRAPPED_POINTER(PTRTYPE,LISPCLASS)

%insert ("swiglisp") %{
%}
%typemap (cin) PTRTYPE ":pointer"
%typemap (cout) PTRTYPE ":pointer"
%enddef

%typemap (cin)  DdManager * "manager"
%typemap (cout) DdManager * "manager"
%typemap (cin)  DdNode * "node"
%typemap (cout) DdNode * "node"


/* Now parse and wrap the API header */
%insert ("swiglisp") %{

(cffi:define-foreign-library libcudd
  (t (:default "libcudd")))

(let ((libdir "/opt/local/lib/cudd/"))
  (when (probe-file libdir)
    (pushnew libdir cffi:*foreign-library-directories* :test #'string=)))
                

(cffi:use-foreign-library libcudd)

(eval-when (:compile-toplevel :load-toplevel)
  ;; Muffle compiler-notes globally
  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:defconstant-uneql))

    %}
    %include "cudd/cudd.h"
    %insert ("swiglisp")
    %{ ) ;; end of eval-when to avoid top-level export

(defun cudd-bdd-not (manager node)
  (declare (ignore manager))
  ;; TODO What happens on big-endian machines?
  (let ((result (cffi:make-pointer (logxor 1 (cffi:pointer-address node)))))
    (cudd-ref result)
    result))

(defun cudd-add-cube (manager vars)
  "Build an add cube out of the variables."
  (let ((n (length vars)))
    (cffi:with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
         :do (setf (cffi:mem-aref array :pointer i) v))
      (cudd-add-compute-cube manager array (cffi:null-pointer) n))))

(defun cudd-bdd-cube (manager vars)
  "Build an bdd cube out of the variables."
  (let ((n (length vars)))
    (cffi:with-foreign-object (array :pointer n)
      (loop :for v :in vars
            :for i :from 0
         :do (setf (cffi:mem-aref array :pointer i) v))
      (cudd-bdd-compute-cube manager array (cffi:null-pointer) n))))



(cffi:defcfun "fopen" :pointer (path :string) (mode :string))
(cffi:defcfun "fclose" :pointer (file :pointer))

(defun dump-dot (manager nodes pathname &key inames onames)
  "Writes a file representing the argument DDs in a format suitable
  for the graph drawing program dot. Returns nil.

  dump-dot uses a minimal unique subset of the
  hexadecimal address of a node as name for it. If the argument inames
  is non-nil, it is assumed to hold the the names of the
  inputs. Similarly for onames. dump-dot uses the following
  convention to draw arcs:

  * solid line: THEN arcs;
  * dotted line: complement arcs;
  * dashed line: regular ELSE arcs.

  The dot options are chosen so that the drawing fits on a letter-size sheet."
  (let* ((nodes (if (typep nodes 'sequence) nodes (list nodes)))
         (n (length nodes))
         (n-names (if inames (length inames) 0)))
    (cffi:with-foreign-objects
        ((node-array :pointer n)
         (iname-array :string n-names)
         (oname-array :string n))
        (loop :for i :from 0 :below n
              :for node :in nodes
              :do (setf (cffi:mem-aref node-array :pointer i) node))
        (when inames
          (loop :for i :from 0 :below n-names
                :for name :across inames
                :do (setf (cffi:mem-aref iname-array :string i)
                          (or name (cffi:null-pointer)))))
        (when onames
          (loop :for i :from 0 :below n
                :for name :across onames
                :do (setf (cffi:mem-aref oname-array :string i)
                          (or name (cffi:null-pointer)))))
        (let ((file (fopen (coerce pathname 'string) "w")))
          (if (cffi:null-pointer-p file)
              (error "Could not open file for writing")
              (unwind-protect
                   (let ((result
                          (cudd-dump-dot manager n node-array
                                         (if inames iname-array (cffi:null-pointer))
                                         (if onames oname-array (cffi:null-pointer))
                                         file)))
                     (if (= result 1)
                         nil
                         (error "Could not dump to dot file")))
                (fclose file)))))))

(defun add-var (manager &key nr level)
  "Creates a new ADD variable. At most one of nr and level may be given.

If neither nr nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If nr is given, then retrieves the ADD variable with index nr if it already exists,
or creates a new ADD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and nr level)
    (error "ADD-VAR accepts at most one of I and LEVEL"))
  (cond
    (nr (cudd-add-ith-var manager nr))
    (level (cudd-add-new-var-at-level manager level))
    (t (cudd-add-new-var manager))))

(defun bdd-var (manager &key nr level)
  "Creates a new BDD variable. At most one of nr and level may be given.

If neither nr nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If nr is given, then retrieves the BDD variable with index nr if it already exists,
or creates a new BDD variable

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a pointer to the new variable if successful;
invokes a signal otherwise.

An ADD variable differs from a BDD variable because it points to the arithmetic zero,
instead of having a complement pointer to 1."
  (when (and nr level)
    (error "BDD-VAR accepts at most one of I and LEVEL"))
  (cond
    (nr (cudd-bdd-ith-var manager nr))
    (level (cudd-bdd-new-var-at-level manager level))
    (t (cudd-bdd-new-var manager))))

(defun print-info (manager pathname)
  (let ((file (fopen (coerce pathname 'string) "w")))
    (if (cffi:null-pointer-p file)
        (error "Could not open file for writing")
        (unwind-protect
             (let ((result
                    (cudd-print-info manager file)))
               (if (= result 1)
                   nil
                   (error "Could not dump to dot file")))
          (fclose file))))
  (with-open-file (in pathname :direction :input)
    (loop :for c = (read-char in nil :eof)
       :until (eq c :eof)
       :do (write-char c))))

(defun cudd-regular (node)
  (let ((addr (pointer-address node)))
    (setf (ldb (byte 1 0) addr) 0)
    (make-pointer addr)))

(defparameter +cudd-max-index+
  (if (and (= sizeof-void-p 8) (= sizeof-int 4))
      ;; ((unsigned int) ~0) >> 1
      (- (expt 2 31) 1)
      ;; ((unsigned short) ~0)
      (- (expt 2 16) 1)))

(defun cudd-node-is-constant (manager node)
  (declare (ignore manager))
  (let ((regular (cudd-regular node)))
    (= (cudd-node-read-index regular) +cudd-max-index+)))

(defun cudd-node-get-value (manager node)
  "Return the value of a leaf node.

Warning: Undefined behaviour if DD is not a leaf node"
  (declare (ignore manager))
  (cffi:foreign-slot-value
   (cffi:foreign-slot-pointer (cudd-regular node) 'dd-node 'type)
      'dd-node-type 'value))

(defun cudd-node-get-then (manager node)
    "Return the then-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
    (declare (ignore manager))
    (let ((result
           (cffi:foreign-slot-value
            (cffi:foreign-slot-value
             (cffi:foreign-slot-pointer (cudd-regular node) 'dd-node 'type)
             'dd-node-type 'kids)
            'dd-children 'T)))
      (cudd-ref result)
      result))

(defun cudd-node-get-else (manager node)
    "Return the else-child of an inner node.

Warning: Undefined behaviour if DD is a leaf node"
    (declare (ignore manager))
    (let ((result
           (cffi:foreign-slot-value
            (cffi:foreign-slot-value
             (cffi:foreign-slot-pointer (cudd-regular node) 'dd-node 'type)
             'dd-node-type 'kids)
            'dd-children 'E)))
      (cudd-ref result)
      result))

(defun cudd-node-get-ref-count (manager node)
    "Return the reference count of the node."
    (declare (ignore manager))
    (cffi:foreign-slot-value (cudd-regular node) 'dd-node 'ref))
%}
