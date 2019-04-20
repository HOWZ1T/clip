(defpackage collections
  (:use :cl)
  (:export
   :make-dlist
   :copy-dlist
   :dlist-prepend
   :dlistify
   :dpop
   :dpush
   :dpeek
   :dsize
   :dclear
   :dequals))
(in-package :collections)

;;; allows circular structures like node to print
(setf *print-circle* t)


;;; ------------------------- PRIVATE SYMBOLS BELOW -------------------------- ;;;


;;; error conidition for invalid types
(define-condition invalid-type (error)
  ((msg :initarg :msg :reader msg)))

;;; node structure for storing data and linking to the next and previous nodes
(defstruct (node (:constructor make-node (&optional data prev next)))
  (data nil)
  (prev nil :type (or null node))
  (next nil :type (or null node)))

;;; appends node b to node a
;;; PARAMETERS:
;;;     a - node
;;;     b - node
;;; RETURNS:
;;;     b
(defmethod node ((a node) (b node))
  (setf (node-next a) b)
  (setf (node-prev b) a) b)

;;; doubly linked list structure for storing nodes which allow quick prepending and appending
(defstruct (dlist (:constructor make-dlist ()))
  (tail nil :type (or null node))
  (head nil :type (or null node))
  (size 0 :type integer))

;;; appends node n to the end of dlist lst
;;; PARAMETERS:
;;;     lst - dlist
;;;     n - node
;;; RETURNS:
;;;     lst
(defmethod dlist-append ((lst dlist) (n node))
  (if (and (not (dlist-tail lst)) (not (dlist-head lst)))
      (progn
	(setf (dlist-tail lst) n)
	(setf (dlist-head lst) n)
	(setf (dlist-size lst) (+ (dlist-size lst) 1)))
      (progn
	(let ((cur-n nil))
	  (setf cur-n (dlist-tail lst))
	  (loop while (node-next cur-n) do
	       (setf cur-n (node-next cur-n)))
	  (node cur-n n)
	  (setf (dlist-tail lst) n)
	  (setf (dlist-size lst) (+ (dlist-size lst) 1))))) lst)


;;; ------------------------- EXPORTED SYMBOLS BELOW ------------------------- ;;;


;;; prepends node n to the begging of dlist lst
;;; PARAMETERS:
;;;     lst - dlist
;;;     n - node
;;; RETURNS:
;;;     lst
(defmethod dlist-prepend ((lst dlist) (n node))
  (if (and (not (dlist-tail lst)) (not (dlist-head lst)))
      (progn
	(setf (dlist-tail lst) n)
	(setf (dlist-head lst) n)
	(setf (dlist-size lst) (+ (dlist-size lst) 1)))
      (progn
	(let ((cur-n nil))
	  (setf cur-n (dlist-head lst))
	  (loop while (node-prev cur-n) do
	       (setf cur-n (node-prev cur-n)))
	  (node n cur-n)
	  (setf (dlist-head lst) n)
	  (setf (dlist-size lst) (+ (dlist-size lst) 1))))) lst)


;;; converts a dlist into a standard common lisp list
;;; PARAMETERS:
;;;     lst - dlist
;;; RETURNS:
;;;     li - common lisp list
(defmethod dlistify ((lst dlist))
  (let ((cur-n nil) (li nil))
    (setf cur-n (dlist-tail lst))
    (loop while cur-n do
	 (push (node-data cur-n) li)
	 (setf cur-n (node-prev cur-n))) li))

;;; appends the data to the end of the list
;;; PARAMETERS:
;;;     lst - dlist
;;;     data - the data that is being appended to the list
;;; RETURNS:
;;;     lst - dlist
(defmethod dpush ((lst dlist) data)
  (dlist-append lst (make-node data)))

;;; removes the last item from the dlist and returns (pops) it
;;; NOTE:
;;;     function will return nil if applied on an empty dlist !
;;; PARAMETERS:
;;;     lst - dlist
;;; RETURNS:
;;;     data - the data contained in the popped node
(defmethod dpop ((lst dlist))
  (let ((old-node (copy-structure (dlist-tail lst))))
    (setf (dlist-tail lst) (node-prev (dlist-tail lst)))
    (setf (node-next (dlist-tail lst)) nil)
    (setf (dlist-size lst) (- (dlist-size lst) 1))
    (node-data old-node)))

;;; returns the size of the dlist
;;; PARAMETERS:
;;;      lst - dlist
;;; RETURNS:
;;;      size - integer size of the list
(defmethod dsize ((lst dlist)) (dlist-size lst))

;;; returns the last item from the dlist without removing it
;;; PARAMETERS:
;;;     lst - dlist
;;; RETURNS:
;;;     data - the data contained in the last node
(defmethod dpeek ((lst dlist)) (node-data (dlist-tail lst)))

;;; clears all items from the dlist
;;; PARAMETERS:
;;;     lst - dlist
;;; RETURNS:
;;;     lst - dlist (which is now cleared / empty)
(defmethod dclear ((lst dlist))
  (setf (dlist-head lst) nil)
  (setf (dlist-tail lst) nil)
  (setf (dlist-size lst) 0) lst)

;;; compares dlist a to dlist b
;;; PARAMETERS:
;;;     a - dlist
;;;     b - dlist
;;; RETURNS:
;;;     result - boolean
(defmethod dequals ((a dlist) (b dlist))                 ;; TODO THIS FUNCTION IS NOT OPTIMAL, #NEEDS OPTIMIZATION !
  (let ((lst-a (dlistify a)) (lst-b (dlistify b)))
    (equal lst-a lst-b)))
    
;;; TODO ADD FUNCTIONS (REMOVE-AT, INSERT-AT, GET-AT)

