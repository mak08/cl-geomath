;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-01-12 18:46:36>

(declaim (optimize (speed 3) (debug 0)  (space 1) (safety 1)))

(in-package :cl-geomath)

(deftype rad-angle () (list 'double-float (- (* 2 pi)) (* 2 pi)))
(deftype latlng () (list 'simple-array 'rad-angle 1))


(declaim (inline make-latlng))
(defun make-latlng (&key (latr% 0d0) (lngr% 0d0))
  (let ((ll (make-array 2 :element-type 'rad-angle)))
    (setf (aref ll 0) latr%)
    (setf (aref ll 1) lngr%)
    ll))

(defun copy-latlng (ll)
  (declare (type latlng ll))  
  (make-array 2 :element-type 'rad-angle :initial-contents ll))

(declaim (inline latlng-latr))
(defun latlng-latr (ll)
  (declare (type latlng ll))
  (aref ll 0))

(declaim (inline latlng-lngr))
(defun latlng-lngr (ll)
  (declare (type latlng ll))
  (aref ll 1))

(declaim (inline latlng-lat))
(defun latlng-lat (ll)
  (declare (type latlng ll))
  (deg (aref ll 0)))

(declaim (inline latlng-lng))
(defun latlng-lng (ll)
  (declare (type latlng ll))
  (deg (aref ll 1)))



;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
