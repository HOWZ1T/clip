(defpackage decoders
  (:use :cl)
  (:export :read-file))
(in-package :decoders)

(defvar *PNG-SIG* '(137 80 78 71 13 10 26 10))

(defun read-file (path)
  (let ((bytes nil))
    (with-open-file
	(stream path
		:direction :input
		:element-type '(unsigned-byte 8)
		:if-exists :supersede)
      (let ((byte nil))
	(setf byte (read-byte stream nil))
	(loop while byte do
	     (cons byte bytes)
	     (reverse bytes)
	     (setf byte (read-byte stream nil))))) bytes))
