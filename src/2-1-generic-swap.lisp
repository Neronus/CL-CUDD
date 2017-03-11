(in-package :cudd)

(defmacro with-iterator ((next has-next obj) &body body)
  (let ((state (gensym "state"))
        (gobj (gensym "object")))
    `(let* ((,gobj ,obj)
           (,state
            (cond
              ((typep ,gobj 'array)
               0)
              ((typep ,gobj 'list)
               ,gobj)
              (t (error "~A is neither an array nor a list" ,gobj)))))
       (flet ((,next ()
                (cond
                  ((typep ,gobj 'array)
                   (incf ,state)
                   (aref ,gobj ,state))
                  ((typep ,gobj 'list)
                   (let ((e (car ,state)))
                     (setq ,state (cdr ,state))
                     (car ,state)
                     e))))
              (,has-next ()
                (cond
                  ((typep ,gobj 'array)
                   (< ,state (length ,gobj)))
                  ((typep ,gobj 'list)
                   (not (null ,gobj))))))
         ,@body))))

(def-cudd-call node-permute ((:add cudd-add-permute :bdd cudd-bdd-permute) (node :node) permutation)
  :generic
  "Given a permutation in sequence permut, creates a new DD with
  permuted variables. There should be an entry in array permut for
  each variable in the manager. The i-th entry of permut holds the
  index of the variable that is to substitute the i-th variable.

For example, (2 0 1) results in a mapping

 0  1  2
---------
 2  0  1

and CUDD performs iterative swaps between variables in order to achieve the specified results.
")

;;; If the permutation is given as any kind of sequence (this
;;; is what we actually expect), then we first turn the sequence
;;; into an array and then call the actual implementation
(defmethod node-permute :around (node (permutation sequence))
  (let ((len (length permutation)))
    (with-foreign-object (array :pointer len)
      (loop :for i :from  0 :below (length permutation)
            :do (setf (mem-aref array :pointer i) (node-pointer (elt permutation i))))
      (node-permute node array))))

(defun add-swap-variables (mgr node x y)
  (assert (= (length x) (length y)))
  (with-seq-as-array xa x
    (with-seq-as-array ya y
      (cudd-add-swap-variables mgr node xa ya (length x)))))

(defun bdd-swap-variables (mgr node x y)
  (assert (= (length x) (length y)))
  (with-seq-as-array xa x
    (with-seq-as-array ya y
      (cudd-bdd-swap-variables mgr node xa ya (length x)))))




(def-cudd-call swap-variables ((:add add-swap-variables :bdd bdd-swap-variables) (node :node) x y)
  :generic
  "Swaps two sets of variables of the same size (x and y) in the ADD
   f. The size is given by n. The two sets of variables are assumed to
   be disjoint.")

