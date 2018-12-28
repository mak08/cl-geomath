;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 02:34:50>

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :execute)
  (defmacro check-delta-equal (form result)
    `(progn
       (assert (equal (ftruncate ,form 0.00001) (ftruncate ,result 0.00001)))
       t)
    )

  (defmacro check-equal (form result)
    `(progn
       (assert (equal ,form ,result))
       t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal (bilinear 1d0 0d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear 0d0 1d0 1d0 2d0 3d0 4d0)
             3.0d0)
(check-equal (bilinear 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal (bilinear-unit 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear-unit 1d0 0d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear-unit 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)
(check-equal (bilinear-unit 0d0 1d0 1d0 2d0 3d0 4d0)
             3.0d0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Positive u is from the west
(check-equal (angle 1d0 0d0)
             270d0)
;; Positive v is form the south
(check-equal (angle 0d0 1d0)
             180d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% 1d0))
  44.99563d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 0d0 :lng% 1d0))
 90.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% 1d0))
 135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% -1d0))
 -135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 0d0 :lng% -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% -1d0))
 -44.9956364d0)

(check-delta-equal
 (course-angle (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% 1d0))
  44.9956365d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 0d0 :lng% 1d0))
 90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% 1d0))
 135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% -1d0 :lng% -1d0))
 -135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 0d0 :lng% -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% -1d0))
 -44.995636455d0)

(check-delta-equal
 (course-angle-d (make-latlng :lat% 0d0 :lng% 0d0) (make-latlng :lat% 1d0 :lng% 0d0))
 0.0d0)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
