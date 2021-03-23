;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-03-21 17:59:41>

(in-package :cl-geomath)


(defconstant +latitude-distance+
   +radius+)

(defconstant +radius+
  6371009d0 ; IUGG Mean Radius
  ;; 6218884d0 ; VR
  )

(declaim (inline longitude-distance))
(defun longitude-distance (latitude)
  (declare (double-float +latitude-distance+)
           (double-float  latitude))
  (the double-float (* +latitude-distance+ (cos latitude))))

(declaim (inline fast-course-distance))
(defun fast-course-distance (origin target)
  (declare (inline latlng-latr latlng-lngr)
           (type latlng origin target)
           (ftype (function (latlng) double-float)  latlng-latr latlng-lngr)
           (ftype (function (float) double-float) longitude-distance))
  (let ((dlat (- (latlng-latr target) (latlng-latr origin)))
        (dlng (- (latlng-lngr target) (latlng-lngr origin))))
    (declare (double-float dlat dlng)
             (inline enorm longitude-distance))
    (enorm (* dlng (* +latitude-distance+  (latlng-latr origin)))
           (* dlat +latitude-distance+))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
