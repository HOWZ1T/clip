(defpackage collections
  (:use :cl)
  (:export
   :make-node
   :node
   :make-dlist
   :dlist
   :dlist-append
   :dlist-prepend
   :dlist-dump
   :dlistify
   :dpush
   :dpop
   :dsize
   :dpeek
   ))
(in-package :collections)

;;; allows circular structures like node to print
(setf *print-circle* t)

;;; error conidition for invalid types
(define-condition invalid-type (error)
  ((msg :initarg :msg :reader msg)))


;;; ------------------------- EXPORT SYMBOLS BELOW ------------------------- ;;;


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

;;; dumps the data in the list
;;; PARAMETERS:
;;;     lst - dlist
;;; RETURNS:
;;;     lst
(defmethod dlist-dump ((lst dlist)) ;; TODO - UPDATE TO PRIN1 the listified version of dlist
  (let ((cur-n nil))
    (setf cur-n (dlist-head lst))
    (loop while cur-n do
	 (prin1 (node-data cur-n))
	 (princ " ")
	 (setf cur-n (node-next cur-n)))) lst)

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

;;; TODO ADD FUNCTIONS (REMOVE-AT, INSERT-AT, GET-AT)

