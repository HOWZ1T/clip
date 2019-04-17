(defpackage collections
  (:use :cl)
  (:export :node :make-node))
(in-package :collections)

(defstruct (node (:constructor make-node (&optional (data next prev))))
  (data nil)
  (next nil :type node)
  (prev nil :type node))

