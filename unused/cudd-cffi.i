/* swig interface wrapper file for cffi binding generation -*- lisp -*- */
/* swig -cffi interface for CUDD */
/* (c) 2009 Utz-Uwe Haus */
/* (c) 2011 Christian von Essen */

%module "cl-cudd.api"

%feature("intern_function","1");   // use swig-lispify
%feature("export");                // export wrapped things

%insert("lisphead")%{
;;; Auto-generated -*- lisp -*- file
;;; generated from $Id:$
(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize (speed 3) (debug 1) (safety 1))))

(cl:in-package :cl-cudd.swig-macros)

(cl:defun swig-lispify (name flag cl:&optional (package (find-package :cl-cudd.api)))
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


(cl:in-package :cl-cudd.api)
%}
%insert ("swiglisp") %{
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:in-package :cl-cudd.api)

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

(eval-when (:compile-toplevel :load-toplevel)
  ;; Muffle compiler-notes globally
  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:defconstant-uneql))

    %}
    %include "cudd/cudd.h"
    %insert ("swiglisp")
    %{ ) ;; end of eval-when to avoid top-level export

%}
