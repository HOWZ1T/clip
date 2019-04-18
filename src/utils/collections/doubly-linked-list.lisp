(defpackage collections
  (:use :cl)
  (:export
   :make-node
   :node
   :make-dlist
   :dlist
   :dlist-prepend
   :dlist-dump
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
(defmethod dlist ((lst dlist) (n node))
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

;;; TODO ADD FUNCTIONS (POP, REMOVE, SIZE, REMOVE-AT, INSERT-AT, GET-AT, LISTIFY)
