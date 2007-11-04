(defpackage captured-stream
  (:use :common-lisp)
  (:export
   make-captured-stream
   disconnect-captured-stream
   captured-stream-vector
   ensure-captured-stream-fill))

(in-package :captured-stream)

(defclass captured-stream (sequence standard-object)
  ((array :accessor cstr-array :initarg :array
	  :initform (make-array 0 :adjustable t :fill-pointer nil :displaced-to nil))))

(defclass disconnected-captured-stream (captured-stream)
  ())

(defclass connected-captured-stream (captured-stream)
  ((stream :accessor cstr-stream :initarg :stream :type stream)))
    
(defun make-captured-stream (s &key allocate-full)
  (declare (type stream s))
  (make-instance 'connected-captured-stream
   :stream s
   :array (make-array (if allocate-full (file-length s) 0)
	   :adjustable t :fill-pointer 0 :displaced-to nil
	   :element-type (stream-element-type s))))

(defun disconnect-captured-stream (captured-stream)
  (declare (type connected-captured-stream captured-stream))
  (close (cstr-stream captured-stream))
  (make-instance 'disconnected-captured-stream
   :array (cstr-array captured-stream)))

(defun captured-stream-vector (captured-stream)
  (cstr-array captured-stream))

(defun front-array-upto-p (o index)
  (< index (length (cstr-array o))))

(defun update-front-array-until (o index)
  (let ((read-start (length (cstr-array o)))
	(dimension (array-dimension (cstr-array o) 0))
	(dimension-req (1+ index)))
    (if (> dimension-req dimension)
	(setf (cstr-array o)
	      (adjust-array (cstr-array o) dimension-req
	       :fill-pointer dimension-req))
	(setf (fill-pointer (cstr-array o)) dimension-req))
    (read-sequence (cstr-array o) (cstr-stream o) :start read-start :end (1+ index))))

(defgeneric ensure-captured-stream-fill (o index)
  (:method ((o connected-captured-stream) index)
    (unless (front-array-upto-p o index)
      (update-front-array-until o index))))

(defmethod sequence:length ((o captured-stream))
  (array-dimension (cstr-array o) 0))

(defmethod sequence:elt ((o captured-stream) index)
  (ensure-captured-stream-fill o index)
  (aref (cstr-array o) index))

(defmethod (setf sequence:elt) (new-value (o captured-stream) index)
  (if (front-array-upto-p o index)
      (setf (aref (cstr-array o) index) new-value)
      (error "Trying to modify future of a captured stream.")))

(defmethod sequence:make-sequence-like ((o captured-stream) length &key (initial-element nil iep) (initial-contents nil icp))
  (cond
    ((and icp iep)
     (error "Meaninglessly attempting to supply both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
    (icp (make-instance 'disconnected-captured-stream
	  :array (make-array length :initial-contents initial-contents
				    :element-type (array-element-type (cstr-array o)))))
    (iep (make-instance 'disconnected-captured-stream
	  :array (make-array length :initial-element initial-element
				    :element-type (array-element-type (cstr-array o)))))
    (t   (make-instance 'disconnected-captured-stream
	  :array (make-array length :element-type (array-element-type (cstr-array o)))))))

(defmethod sequence:adjust-sequence ((o captured-stream) length &key initial-element (initial-contents nil icp))
  (sequence:adjust-sequence (cstr-array o) length :initial-element initial-element)
  (when icp (replace (cstr-array o) initial-contents)))

(defmethod sequence:subseq ((o captured-stream) start &optional end)
  (let* ((length (length (cstr-array o)))
	 (end (or end (1- length))))
    (ensure-captured-stream-fill o end)
    (make-instance 'disconnected-captured-stream
     :array (make-array (- end start)
	     :element-type (array-element-type (cstr-array o))
	     :displaced-to (cstr-array o)
	     :displaced-index-offset start))))