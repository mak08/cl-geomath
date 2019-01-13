;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-10 23:55:56>

(defpackage "CL-GEOMATH"
  (:use "COMMON-LISP" "CFFI")
  (:export "+RADIUS+"
           "+NAUTICAL-MILE+"
           "+STANDARD-NAUTICAL-MILE+"
           
           "LATLNG"
           "MAKE-LATLNG"
           "COPY-LATLNG"
           "LATLNG-LATR"
           "LATLNG-LNGR"
           "LATLNG-LAT"
           "LATLNG-LNG"

           "LONGITUDE-BETWEEN"
           "HEADING-BETWEEN"

           "ROUND-TO-DIGITS"

           "KNOTS-TO-M/S"
           "M/S-TO-KNOTS"
           "M/S-TO-KM/H"
           
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
           "FAST-COURSE-DISTANCE"

           "ADD-DISTANCE-EXACT"
           "ADD-DISTANCE-ESTIMATE"

           "FIND-PLACE"
           "+ALICANTE+"
           "+CAPETOWN+"
           "+CARTHAGO+"
           "+FEHMARN+"
           "+FREMANTLE+"
           "+GIBRALTAR+"
           "+LACORUNA+"
           "+LESSABLES+"
           "+LISBON+"
           "+LIZARD-POINT+"
           "+MARSEILLE+"
           "+NEW-YORK+"
           "+OUESSANT+"
           "+PORTOSANTO+"
           "+YSTAD+"
           ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
