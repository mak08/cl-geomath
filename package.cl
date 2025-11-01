;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2023-02-28 22:34:36>

(defpackage "CL-GEOMATH"
  (:use "COMMON-LISP" "CFFI" "MACROS")
  (:export "+RADIUS+"
           "+NAUTICAL-MILE+"
           "+STANDARD-NAUTICAL-MILE+"
           
           "LATLNG"
           "MAKE-LATLNG"
           "FORMAT-LATLNG"
           "COPY-LATLNG"
           "LATLNG-LATR"
           "LATLNG-LNGR"
           "LATLNG-LAT"
           "LATLNG-LNG"

           "LONGITUDE-BETWEEN"
           "LONGITUDINAL-DIRECTION"
           "HEADING-BETWEEN"

           "ROUND-TO-DIGITS"

           "KNOTS-TO-M/S"
           "M/S-TO-KNOTS"
           "M/S-TO-KM/H"

           "MAKE-DMS"
           "DECIMAL-TO-DMS"
           "DMS-TO-DECIMAL"
           
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
           "INORMALIZE-ANGLE"

           "FRACTION-INDEX"
           "BILINEAR-UNIT"
           
           "BILINEAR"
           "LINEAR"

           "POINT-IN-POLY-P"

           "GC-ANGLE"
           "GC-DISTANCE"
           "COURSE-ANGLE"
           "COURSE-ANGLE-D"
           "COURSE-DISTANCE"
           "FAST-COURSE-DISTANCE"

           "SEGMENT-INTERSECTS"

           "ADD-DISTANCE-EXACT"
           "ADD-DISTANCE-ESTIMATE"

           "FIND-PLACE"
           "+ALICANTE+"
           "+CAPETOWN+"
           "+CARTHAGO+"
           "+FEHMARN+"
           "+FREMANTLE+"
           "+GIBRALTAR+"
           "+HONOLULU+"
           "+JOAO-PESSOA+"
           "+LACORUNA+"
           "+LESSABLES+"
           "+LISBON+"
           "+LIZARD-POINT+"
           "+LOS-ANGELES+"
           "+MARSEILLE+"
           "+NEW-YORK+"
           "+OUESSANT+"
           "+PORTOSANTO+"
           "+YSTAD+"
           ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
