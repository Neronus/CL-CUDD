(in-package :cudd)

(defun bdd->add (bdd)
  "Converts a BDD to a 0-1 ADD"
  (assert (typep bdd 'bdd-node))
  (wrap-and-finalize
   (cudd-bdd-to-add (manager-pointer *manager*) (node-pointer bdd))
   'add-node))

(defun add->bdd (add)
  "Converts an ADD to a BDD by replacing all discriminants different from 0 with 1."
  (assert (typep add 'add-node))
  (wrap-and-finalize
   (cudd-add-bdd-pattern (manager-pointer *manager*) (node-pointer add))
   'bdd-node))

(def-cudd-call add->bdd-interval (() (f :node) lower upper)
  :generic "Converts an ADD to a BDD by replacing all discriminants greater than or equal to lower and less
  than or equal to upper with 1, and all other discriminants with 0.")

(defmethod add->bdd-interval ((f add-node) lower upper)
  (wrap-and-finalize (cudd-add-bdd-interval
                      (manager-pointer *manager*)
                      (node-pointer f)
                      lower upper)
                     'bdd-node))


(def-cudd-call add->bdd-strict-threshold (() (f :node) threshold)
  :generic "Converts an ADD to a BDD by replacing all discriminants STRICTLY greater than value with 1, and
  all other discriminants with 0.")

(defmethod add->bdd-strict-threshold ((f add-node) threshold)
  (wrap-and-finalize (cudd-add-bdd-strict-threshold
                      (manager-pointer *manager*)
                      (node-pointer f)
                      threshold)
                     'bdd-node))

(def-cudd-call add->bdd-threshold (() (f :node) threshold)
  :generic "Converts an ADD to a BDD by replacing all discriminants greater than or equal to value with 1, and
  all other discriminants with 0.")

(defmethod add->bdd-threshold ((f add-node) threshold)
  (wrap-and-finalize (cudd-add-bdd-threshold
                      (manager-pointer *manager*)
                      (node-pointer f)
                      threshold)
                     'bdd-node))

