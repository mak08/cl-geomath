;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 03:02:55>

(in-package :cl-geomath)


(declaim (optimize (speed 3) (debug 1)  (space 0) (safety 1)))

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
  (* 360 (/ x (* 2 pi))))
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
