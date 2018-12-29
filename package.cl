;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-29 02:05:19>

(defpackage "CL-GEOMATH"
  (:use "COMMON-LISP" "CFFI")
  (:export "LATLNG"
           "MAKE-LATLNG"
           "COPY-LATLNG"
           "LATLNG-LATR"
           "LATLNG-LNGR"
           "LATLNG-LAT"
           "LATLNG-LNG"

           "ROUND-TO-DIGITS"

           "DEG"
           "RAD"

           "HEADING"
           
           "ANGLE-R"
           "ANGLE"
           "ENORM"
           "P2UV"
           "P2C"

           "NORMALIZE-HEADING"
           "NORMALIZE-ANGLE"

           "FRACTION-INDEX"
           "BILINEAR-UNIT"
           
           "BILINEAR"
           "LINEAR"

           "GC-ANGLE"
           "GC-DISTANCE"
           "COURSE-ANGLE"
           "COURSE-ANGLE-D"
           "COURSE-DISTANCE"

           "ADD-DISTANCE-EXACT"
           "ADD-DISTANCE-ESTIMATE" ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
