;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2022-01-30 11:50:03>

(defsystem "cl-geomath"
  :description "Basic math"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "makros")
  :in-order-to ((test-op (test-op "cl-geomath/test")))
  :serial t
  :components ((:file "package")
               (:file "datatypes")
               (:file "units")
               (:file "polygons")
               (:file "geomath")
               (:file "fast-dist")
               (:file "printer")
               (:file "places")))

(defsystem "cl-geomath/test"
  :depends-on ("cl-geomath" "fiveam")
  :default-component-class cl-source-file.cl
  :components ((:file "tests/test"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (unless (uiop:symbol-call :fiveam :run!
                                       (find-symbol "CL-GEOMATH-TESTS" "CL-GEOMATH"))
               (error "cl-geomath tests failed"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
