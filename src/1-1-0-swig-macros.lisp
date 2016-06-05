(in-package :cl-cudd.swig-macros)
(defun lispify (name flag &optional (package (find-package :cl-cudd.baseapi)))
  (labels ((helper (lst last rest &aux (c (car lst)))
             (cond
               ((null lst)
                rest)
               ((upper-case-p c)
                (helper (cdr lst) 'upper
                        (case last
                          ((lower digit) (list* c #\- rest))
                          (t (cons c rest)))))
               ((lower-case-p c)
                (helper (cdr lst) 'lower (cons (char-upcase c) rest)))
               ((digit-char-p c)
                (helper (cdr lst) 'digit 
                        (case last
                          ((upper lower) (list* c #\- rest))
                          (t (cons c rest)))))
               ((char-equal c #\_)
                (helper (cdr lst) '_ (cons #\- rest)))
               ((char-equal c #\-)
                (helper (cdr lst) '- (cons #\- rest)))
               (t
                (error "Invalid character: ~A" c)))))
    (let ((fix (case flag
                 ((:constant :enumvalue) "+")
                 (:variable "*")
                 (t ""))))
      (intern
       (concatenate
        'string
        fix
        (nreverse (helper (concatenate 'list name) nil nil))
        fix)
       package))))
