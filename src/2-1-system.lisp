(in-package cudd)

(def-cudd-call disable-gc ((:common cudd-disable-garbage-collection))
               :generic "Disables garbage collection. Garbage
collection is initially enabled. This function may be called to
disable it. However, garbage collection will still occur when a new
node must be created and no memory is left, or when garbage collection
is required for correctness. (E.g., before reordering.)"
               :dont-wrap-result t)

(def-cudd-call enable-gc ((:common cudd-enable-garbage-collection))
  :generic "Enables garbage collection. Garbage collection is
initially enabled. Therefore it is necessary to call this function
only if garbage collection has been explicitly disabled."
  :dont-wrap-result t)

(def-cudd-call set-background ((:common cudd-set-background) bck)
  :generic "Sets the background constant of the manager. It assumes
that the DdNode pointer bck is already referenced."
  :dont-wrap-result t)

(defun count-leaves (node)
  "Counts the number of leaves in a DD."
  (cudd-count-leaves (node-pointer node)))

(defun dag-size (node)
  "Counts the number of nodes in a DD."
  (cudd-dag-size (node-pointer node)))

(defmethod cffi:translate-to-foreign (pointer (node node))
  (node-pointer node))

(defmethod cffi:translate-to-foreign (pointer (manager manager))
  (manager-pointer manager))

