(defpackage decoders
  (:use :cl :collections)
  (:export :read-png))
(in-package :decoders)

(define-condition invalid-png-signature (error)
  ((message :initarg :message :reader message)))

(defvar *PNG-SIG* (to-dlist '(137 80 78 71 13 10 26 10)))

(defun check-png-sig (sig)
  (dequal sig *PNG-SIG*))

;; TODO after sig check start parsing image file
(defun read-png (path)
  (let ((bytes (make-dlist)))
    (with-open-file
	(stream path
		:direction :input
		:element-type '(unsigned-byte 8)
		:if-exists :supersede)
      (let ((byte nil) (i 0) (has-valid-sig nil))
	(setf byte (read-byte stream nil))
	(loop while byte do
	     (dpush byte bytes)
	     (setf byte (read-byte stream nil))
	     (if (= (dsize bytes) 8)
		 (progn
		   (if (check-png-sig bytes)
		       (setf has-valid-sig t)
		       (error 'invalid-png-signature :message (format t "Invalid png signature in file: ~A" path))))))
	(if has-valid-sig
	    (return-from read-png (dlistify bytes))
	    (return-from read-png nil))))))
