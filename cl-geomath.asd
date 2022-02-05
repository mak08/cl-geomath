;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2022-01-30 11:50:03>

(defsystem "cl-geomath"
  :description "Basic math"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "makros")
  :serial t
  :components ((:file "package")
               (:file "datatypes")
               (:file "units")
               (:file "polygons")
               (:file "geomath")
               (:file "fast-dist")
               (:file "printer")
               (:file "places")
               (:file "test")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

