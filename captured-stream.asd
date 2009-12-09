;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :captured-stream
  :depends-on ()
  :components
  ((:file "packages")
   ;;
   (:file "captured-stream" :depends-on "packages")))
