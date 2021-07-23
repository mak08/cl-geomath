;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-07-23 21:43:04>

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconstant +radius+
  6371009d0 ; IUGG Mean Radius
  ;; 6218884d0 ; VR
  )

(defconstant +1/radius+
  (/ 1 6371009d0))

(defconstant +standard-nautical-mile+ 1852.216d0)

(defconstant +nautical-mile+
  ;; (/ (* 2 pi +radius+) (* 360d0 60d0))
  +standard-nautical-mile+
  )

(defconstant +pi/180+
  (/ pi 180d0))

(defconstant +deg-length+
  (rad +radius+)
  "Distance of 1° at the equator")

(defconstant +latitude-distance+
  (rad +radius+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit conversion

(defconstant +knots-to-m/s+
  (/ +standard-nautical-mile+ 3600d0))

(defun knots-to-m/s (knots)
  ;; (* 463.0 (/ knots 900.0)))
  (* knots +knots-to-m/s+))

(defun m/s-to-knots (m/s)
  ;; (* 900.0 (/ m/s 463.0)))
  (/ m/s +knots-to-m/s+))

(defun m/s-to-kM/h (m/s)
  (* m/s 3.6))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates

(defstruct dms (u 1) d m s (cs 0d0))

(defun decimal-to-dms (deg)
  (declare (double-float deg))
  (let* ((u (signum deg))
         (n (abs deg)))
    (multiple-value-bind (d rest)
        (floor n)
      (multiple-value-bind (m rest)
          (floor rest 1/60)
        (multiple-value-bind (s rest)
            (floor rest 1/3600)
          (make-dms :u u :d d :m m :s s :cs (* rest 3600))))))) 

(defun dms-to-decimal (dms)
  (* (dms-u dms)
     (+ (dms-d dms)
        (/ (dms-m dms) 60)
        (/ (dms-s dms) 3600)
        (/ (dms-cs dms) 3600))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
