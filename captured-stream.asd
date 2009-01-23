(defpackage :captured-stream.system
  (:use :cl :asdf))

(in-package :captured-stream.system)

(defsystem :captured-stream
  :depends-on ()
  :components
  ((:file "packages")
   ;;
   (:file "captured-stream" :depends-on "packages")))
