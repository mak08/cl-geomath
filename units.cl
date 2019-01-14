;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-13 22:39:04>

(declaim (optimize (speed 3) (debug 0)  (space 1) (safety 1)))

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconstant +radius+
  ;; 6371009d0 IUGG Mean Radius
  6218884d0 ; VR 
  )

(defconstant +standard-nautical-mile+ 1852.216d0)

(defconstant +nautical-mile+
  (/ (* 2 pi +radius+) (* 360d0 60d0)))

(defconstant +pi/180+
  (/ pi 180d0))

(defconstant +deg-length+
  (/ (* 2 pi +radius+) 360d0)
  "Distance of 1° at the equator")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit conversion

(defconstant +knots-to-m/s+
  (/ +nautical-mile+ 3600d0))

(defun knots-to-m/s (knots)
  ;; (* 463.0 (/ knots 900.0)))
  (* knots +knots-to-m/s+))

(defun m/s-to-knots (m/s)
  ;; (* 900.0 (/ m/s 463.0)))
  (/ m/s +knots-to-m/s+))

(defun m/s-to-kM/h (m/s)
  (* m/s 3.6))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units

(declaim (inline rad))
(defun rad (x)
  (declare (double-float x))
  (* (* 2d0 pi) (/ x 360d0)))
(declaim (notinline rad))

(declaim (inline deg))
(defun deg (x)
  (declare (double-float x))
  (* 360d0 (/ x (* 2d0 pi))))
(declaim (notinline deg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(defstruct dms (degrees 0) (minutes 0) (seconds 0))

(defun dms2decimal (dms)
  (+ (dms-degrees dms)
     (/ (dms-minutes dms) 60)
     (/ (dms-seconds dms) 3600)))

(defun decimal2dms (dec)
  (multiple-value-bind (d r1)
      (truncate dec)
    (multiple-value-bind (m r2)
        (truncate (* r1 60d0))
      (make-dms :degrees d :minutes m :seconds (* r2 60d0)))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
