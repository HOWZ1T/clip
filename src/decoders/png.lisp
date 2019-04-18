(defpackage decoders
  (:use :cl)
  (:export :read-png :check-png-sig))
(in-package :decoders)

(define-condition invalid-png-signature (error)
  ((message :initarg :message :reader message)))

(defvar *PNG-SIG* '(137 80 78 71 13 10 26 10))

(defun check-png-sig (sig)
  (equal sig *PNG-SIG*))

(defun read-png (path)
  (let ((bytes nil))
    (with-open-file
	(stream path
		:direction :input
		:element-type '(unsigned-byte 8)
		:if-exists :supersede)
      (let ((byte nil) (i 0))
	(setf byte (read-byte stream nil))
	(loop while byte do
	     (push byte bytes)
	     (+ i 1)
	     (if (= i 8)
		 (if (not (check-png-sig (reverse bytes)))
		     (error 'invalid-png-signature :message "invalid png file signature")))
	     (setf byte (read-byte stream nil))))) (reverse bytes)))
