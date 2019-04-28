(defpackage decoders
  (:use :cl :collections :babel)
  (:export :read-png))
(in-package :decoders)

(define-condition invalid-png-signature (error)
  ((message :initarg :message :reader message)))

(define-condition ihdr-not-first-chunk (error)
  ((message :initarg :message :reader message)))

(define-condition bad-chunk-size (error)
  ((message :initarg :message :reader message)))

(define-condition bad-chunk (error)
  ((message :initarg :message :reader message)))

(defvar *PNG-SIG* (to-dlist '(137 80 78 71 13 10 26 10)))

(defun check-png-sig (stream)
  (let ((bytes (make-dlist)) (byte nil))
    (loop for i from 1 to 8 do
	 (setf byte (read-byte stream nil))
	 (if (not byte)
	     (error 'bad-chunk-size :message "missing png header bytes!"))
	 (dpush byte bytes))
    (return-from check-png-sig (dequal bytes *PNG-SIG*))))
    
(defun read-length (stream)
  (let ((byte nil) (len 0))
    (loop for i from 1 to 4 do
	 (setf byte (read-byte stream nil))
	 (if (not byte)
	     (error 'bad-chunk-size :message "missing chunk-length descriptor bytes!"))
	 (incf len byte))
    (return-from read-length len)))

(defun read-chunk-type (stream)
  (let ((byte nil) (vec-bytes (make-array 4 :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for i from 1 to 4 do
	 (setf byte (read-byte stream nil))
	 (if (not byte)
	     (error 'bad-chunk-size :message "missing chunk-type descriptor bytes!"))
	 (vector-push byte vec-bytes))
    (return-from read-chunk-type (babel:octets-to-string vec-bytes))))

(defun read-img-dimensions (stream)
  (let ((byte nil) (width 0) (height 0))
    (loop for i from 1 to 4 do
	 (setf byte (read-byte stream nil))
	 (if (not byte)
	     (error 'bad-chunk-size :message "missing chunk-image-width descriptor bytes!"))
	 (incf width byte))
    (loop for i from 1 to 4 do
	 (setf byte (read-byte stream nil))
	 (if (not byte)
	     (error 'bad-chunk-size :message "missing chunk-image-height descriptor bytes!"))
	 (incf height byte))
    (return-from read-img-dimensions (values width height))))
    
;; TODO after sig check start parsing image file
(defun read-png (path)
  (let ((bytes (make-dlist))
	(byte nil)
	(img-width -1)
	(img-height -1)
	(bit-depth -1)
	(color-type -1)
	(compression-method -1)
	(filter-method -1)
	(interlace-method -1))
    (with-open-file
	(stream path
		:direction :input
		:element-type '(unsigned-byte 8)
		:if-exists :supersede)
      (let ((has-valid-sig nil))
	;; checks signature
        (setf has-valid-sig (check-png-sig stream))
	(if (not has-valid-sig)
	    (error 'invalid-png-signature :message (format t "Invalid png signature in file: ~A" path))))
      (block read-chunks
	(block read-ihdr
	  (let ((chnk-len (read-length stream))
		(chnk-type (read-chunk-type stream)))
	    (if (not (equal chnk-type "IHDR")) ; NOTE: CHUNK TYPE IS CASE-SENSITIVE!
		(error 'ihdr-not-first-chunk :message (format t "IHDR is not first chunk in png file: ~A" path)))
	    (if (not (= chnk-len 13)) ; NOTE: IHDR always has 13 bytes of data!
		(error 'bad-chunk-size :message (format t "IHDR does not have 13 data bytes in png file: ~A" path)))
	    ;; read image data from IHDR
	    ;; in this order
	    ;; image-width        (4 bytes)
	    ;; image-height       (4 bytes)
	    ;; bit-depth          (1 byte)
	    ;; color-type         (1 byte)
	    ;; compression method (1 byte)
	    ;; filter method      (1 byte)
	    ;; interlace method   (1 byte)
	    ;; -- end
            ;; TOTAL              (13 bytes)
	    (multiple-value-bind (iw ih) (read-img-dimensions stream)
	      (setf img-width iw)
	      (setf img-height ih))
	    
	    ;; reading bit depth
	    (setf byte (read-byte stream nil))
	    (if (not byte)
		(error 'bad-chunk-size :message (format t "missing bit depth in IHDR chunk in png file: ~A" path)))
	    (setf bit-depth byte)

	    ;; reading color-type
	    (setf byte (read-byte stream nil))
	    (if (not byte)
		(error 'bad-chunk-size :message (format t "missing color-type in IHDR chunk in png file: ~A" path)))
	    (setf color-type byte)

	    ;; reading compression method
	    (setf byte (read-byte stream nil))
	    (if (not byte)
		(error 'bad-chunk-size :message (format t "missing compression method in IHDR chunk in png file: ~A" path)))
	    (setf compression-method byte)

	    ;; reading filter method
	    (setf byte (read-byte stream nil))
	    (if (not byte)
		(error 'bad-chunk-size :message (format t "missing filter method in IHDR chunk in png file: ~A" path)))
	    (setf filter-method byte)

	    ;; reading interlace-method
	    (setf byte (read-byte stream nil))
	    (if (not byte)
		(error 'bad-chunk-size :message (format t "missing interlace method in IHDR chunk in png file: ~A" path)))
	    (setf interlace-method byte)

	    ;; TODO : BUG BIT-DEPTH NOT READING CORRECTLY ? 
	    (print (format t "~30a: ~30d~%~30a: ~30d~%~30a: ~30d~%~30a: ~30d~%~30a: ~30d~%~30a: ~30d~%~30a: ~30d~%"
			   "width" img-width  "height" img-height "bit-depth" bit-depth "color-type" color-type
			   "compression-method" compression-method "filter-method" filter-method
			   "interlace-method" interlace-method))))))))
