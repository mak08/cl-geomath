;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-03-21 13:22:24>

(in-package :cl-geomath)

(deftype rad-angle () (list 'double-float (- (* 2 pi)) (* 2 pi)))
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
  (the double-float (deg (aref ll 0))))

(declaim (inline latlng-lng))
(defun latlng-lng (ll)
  (declare (type latlng ll))
  (the double-float (deg (aref ll 1))))


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
