;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :captured-stream
  :components
  ((:file "packages")
   ;;
   (:file "captured-stream" :depends-on "packages")))
