;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-01-12 22:09:39>

(declaim (optimize (speed 3) (debug 0)  (space 0) (safety 0)))

(in-package :cl-geomath)

(defconstant +latitude-distance+
   +radius+)

(declaim (inline longitude-distance))
(defun longitude-distance (latitude)
  (declare (double-float latitude +latitude-distance+))
  (the double-float (* +latitude-distance+ (cos latitude))))
(declaim (notinline longitude-distance))

(defun fast-course-distance (origin target)
  (declare (inline latlng-latr latlng-lngr)
           (type latlng origin target)
           (ftype (function (latlng) double-float)  latlng-latr latlng-lngr))
  (let ((dlat (- (latlng-latr target) (latlng-latr origin)))
        (dlng (- (latlng-lngr target) (latlng-lngr origin))))
    (declare (double-float dlat dlng)
             (inline enorm longitude-distance))
    (enorm (* dlng (longitude-distance (latlng-latr origin)))
           (* dlat +latitude-distance+))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
