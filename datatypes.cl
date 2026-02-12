;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2025-11-18 00:56:26>

(in-package :cl-geomath)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric units

(defconstant +1/360+ (/ 1 360d0))
(defconstant +1/180+ (/ 1 180d0))
(defconstant +1/PI+ (/ 1 PI))

(declaim (inline rad))
(defun rad (x)
  (declare (double-float x))
  (* pi (* x +1/180+)))

(declaim (inline deg))
(defun deg (x)
  (declare (double-float x))
  (* 180d0 (* x +1/pi+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datatypes

(deftype rad-angle () (list 'double-float (- (* 3 pi)) (* 3 pi)))
(deftype latlng () (list 'simple-array 'rad-angle 1))


(declaim (inline make-latlng))
(defun make-latlng (&key (lat 0d0) (lng 0d0)
                      (latr% (the double-float (rad lat)))
                      (lngr% (the double-float (rad lng))))
  (let ((ll (make-array 2 :element-type 'rad-angle)))
    (setf (aref ll 0) latr%)
    (setf (aref ll 1) lngr%)
    ll))

(defun copy-latlng (ll)
  (declare (type latlng ll))  
  (make-array 2 :element-type 'rad-angle :initial-contents ll))

(declaim (inline latlng-latr))
(defun-t latlng-latr double-float ((ll latlng))
  (aref ll 0))

(declaim (inline latlng-lngr))
(defun-t latlng-lngr double-float ((ll latlng))
  (aref ll 1))

(declaim (inline latlng-lat))
(defun-t latlng-lat double-float ((ll latlng))
  (deg (aref ll 0)))

(declaim (inline latlng-lng))
(defun-t latlng-lng double-float ((ll latlng))
  (deg (aref ll 1)))


(defun format-latlng (stream ll)
  (let ((lat (latlng-lat ll))
        (lng (latlng-lng ll)))
    (declare (type latlng ll)
             (double-float lat lng))
    (format stream "~a~:[N~;S~] ~a~:[E~;W~]"
            (decimal-to-dms (abs lat))
            (<= lat 0d0)
            (decimal-to-dms (abs lng))
            (<= lng 0d0))))
            
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
