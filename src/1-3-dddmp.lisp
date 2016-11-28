;; DDDMP: tools for visualizing Decision Diagrams

(in-package :cl-cudd.baseapi)

(defcfun "fopen" :pointer (path :string) (mode :string))
(defcfun "fclose" :pointer (file :pointer))

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
    (with-foreign-objects
        ((node-array :pointer n)
         (iname-array :string n-names)
         (oname-array :string n))
      (loop :for i :from 0 :below n
            :for node :in nodes
            :do (setf (mem-aref node-array :pointer i) node))
      (when inames
        (loop :for i :from 0 :below n-names
              :for name :across inames
              :do (setf (mem-aref iname-array :string i)
                        (or name (null-pointer)))))
      (when onames
        (loop :for i :from 0 :below n
              :for name :across onames
              :do (setf (mem-aref oname-array :string i)
                        (or name (null-pointer)))))
      (let ((file (fopen (coerce pathname 'string) "w")))
        (if (null-pointer-p file)
            (error "Could not open file for writing")
            (unwind-protect
                (let ((result
                       (cudd-dump-dot manager n node-array
                                      (if inames iname-array (null-pointer))
                                      (if onames oname-array (null-pointer))
                                      file)))
                  (if (= result 1)
                      nil
                      (error "Could not dump to dot file")))
              (fclose file)))))))

(defun print-info (manager pathname)
  (let ((file (fopen (coerce pathname 'string) "w")))
    (if (null-pointer-p file)
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
