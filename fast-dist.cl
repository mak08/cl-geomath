;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-07-23 21:39:05>

(in-package :cl-geomath)

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
  (let* ((lat0 (latlng-latr origin))
         (lon0 (latlng-lngr origin))
         (lat1 (latlng-latr target))
         (lon1 (latlng-lngr target))
         (delta-lat (- lat1 lat0))
         (delta-lon (- lon1 lon0))
         (dist-lat (* (deg delta-lat) +latitude-distance+))
         (dist-lon (* (deg delta-lon) (/ (+ (longitude-distance lat0)
                                            (longitude-distance lat1))
                                         2))))
    (enorm dist-lat dist-lon)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
