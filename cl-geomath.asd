;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2021-03-22 23:26:26>

(defsystem "cl-geomath"
  :description "Basic math"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "makros")
  :serial t
  :components ((:file "package")
               (:file "units")
               (:file "datatypes")
               (:file "geomath")
               (:file "fast-dist")
               (:file "printer")
               (:file "places")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

