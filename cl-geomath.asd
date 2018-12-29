;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2018-12-28 20:11:05>

(defsystem "cl-geomath"
  :description "Basic math"
  :default-component-class cl-source-file.cl
  :depends-on ("log2")
  :serial t
  :components ((:file "package")
               (:file "units")
               (:file "geomath")
               (:file "printer")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

