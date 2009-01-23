;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CAPTURED-STREAM; Base: 10 -*-
;;;

(defpackage captured-stream
  (:use :common-lisp)
  (:export
   #:make-captured-stream
   #:disconnect-captured-stream
   #:captured-stream-vector
   #:ensure-captured-stream-fill))
