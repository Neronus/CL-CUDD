
(defun dd-eval (node assignment)
  "Finds the value of a DD for a given variable assignment. The
variable assignment is passed in a sequence of int's, that should
specify a boolean value for each variable in the support of the
function. Returns a constant node."
  (let ((n (length assignment)))
    (cffi:with-foreign-object (array :int n)
      (loop :for i :from 0
            :for v in assignment
            :do (setf (cffi:mem-aref array :int i)
                      (if v 1 0)))
      (ref-call (cudd-eval node array) array))))

(def-cudd-call find-essential (cudd-find-essential f)
  "Returns the cube of the essential variables. A positive literal
means that the variable must be set to 1 for the function to be 1. A
negative literal means that the variable must be set to 0 for the
function to be 1. Returns a pointer to the cube BDD if successful;
NULL otherwise.")

(defun print-debug (node n pr)
  "Prints to the standard output a DD and its statistics. The statistics include the number of nodes, the number of leaves, and the number of minterms. (The number of minterms is the number of assignments to the variables that cause the function to be different from the logical zero (for BDDs) and from the background value (for ADDs.) The statistics are printed if pr > 0. Specifically:
pr = 0 : prints nothing
pr = 1 : prints counts of nodes and minterms
pr = 2 : prints counts + disjoint sum of product
pr = 3 : prints counts + list of nodes
pr > 3 : prints counts + disjoint sum of product + list of nodes
For the purpose of counting the number of minterms, the function is supposed to depend on n variables."
  (cudd-print-debug (manager-pointer *manager*) node (node-pointer n) pr))

(defun dd-constantp (dd)
  "Return T if DD is a leaf node, NIL otherwise"
  (= 0 (cl-cudd.api:cudd-is-non-constant dd)))

(def-cudd-call count-minterm (cudd-count-minterm nodes nvars)
  "Counts the number of minterms of a DD. The function is assumed to
  depend on nvars variables. The minterm count is represented as a
  double, to allow for a larger number of variables. Returns the
  number of minterms of the function rooted at node"
  nvars :result)



;;; These functions do not exist in cudd.h; I'm not sure where they came
;;; from. [2014/06/12:rpg]

(def-cudd-call min-abstract ((:add cudd-add-min-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the ADD f by
  taking the minimum over all possible values taken by the
  variables.")

(def-cudd-call max-abstract ((:add cudd-add-max-abstract) (f :node) (cube :node))
  :generic "Abstracts all the variables in cube from the ADD f by
  taking the maximum over all possible values taken by the
  variables.")

