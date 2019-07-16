;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-03-16 23:39:37>

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :execute)
  (defmacro check-delta-equal (form result &optional (delta  0.00001))
    `(progn
       (log2:info "Checking ~a => ~a" ',form ',result)
       (assert (< (abs (- ,form ,result)) ,delta))
       t)
    )

  (defmacro check-equal (form result)
    `(progn
       (log2:info "Checking ~a => ~a" ',form ',result)
       (assert (equal ,form ,result))
       t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal (bilinear 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear 0d0 1d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear 1d0 0d0 1d0 2d0 3d0 4d0)
             3.0d0)
(check-equal (bilinear 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)

(check-equal (bilinear 0d0 0d0 900d0 901d0 910d0 911d0)
             900d0)
(check-equal (bilinear 1d0 0d0 900d0 901d0 910d0 911d0)
             910.d0)
(check-equal (bilinear 0d0 1d0 900d0 901d0 910d0 911d0)
             901d0)
(check-equal (bilinear 1d0 1d0 900d0 901d0 910d0 911d0)
             911.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal (bilinear-unit 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear-unit 0d0 1d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear-unit 1d0 0d0 1d0 2d0 3d0 4d0)
             3.0d0)
(check-equal (bilinear-unit 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positive u is from the west
(check-equal (angle 1d0 0d0)
             270d0)
;; Positive v is form the south
(check-equal (angle 0d0 1d0)
             180d0)

(check-equal (angle 0d0 -1d0)
             0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positive u is from the west
(check-equal (angle-r 1d0 0d0)
             (* (/ 3.0 2.0) pi))

;; Positive v is form the south
(check-equal (angle-r 0d0 1d0)
             pi)

(check-equal (angle-r 0d0 -1d0)
             0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal (enorm 0d0 0d0)
             0d0)

(check-equal (enorm 0d0 2d0)
             2d0)

(check-equal (enorm 2d0 2d0)
             (* 2d0 (sqrt 2d0)))

(check-equal (enorm 3d0 4d0)
             5d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal (p2c (angle-r 0d0 10d0) (enorm 0d0 10d0)) 0d0)
(check-delta-equal (p2c (angle-r 5d0 10d0) (enorm 5d0 10d0)) 5d0)
(check-delta-equal (p2c (angle-r 10d0 10d0) (enorm 10d0 10d0)) 10d0)
(check-delta-equal (p2c (angle-r 10d0 5d0) (enorm 10d0 5d0)) 10d0)
(check-delta-equal (p2c (angle-r 10d0 0d0) (enorm 10d0 0d0)) 10d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad 1d0)))
  44.99563d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% 1d0))
 90.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad 1d0)))
 135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% -1d0 :lngr% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad -1d0)))
 -135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad -1d0)))
 -44.9956364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 1d0 :lngr% 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad 1d0)))
  44.9956365d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% (rad 1d0)))
 90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad 1d0)))
 135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad -1d0)))
 -135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% (rad -1d0)))
 -90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad -1d0)))
 -44.995636455d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal (/ (course-distance +new-york+ +lizard-point+) +nautical-mile+)
                   2910.838895539127d0 2d0)
(check-delta-equal (/ (course-distance +capetown+ +lizard-point+) +nautical-mile+)
                   5086.220135360448d0 3d0)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
