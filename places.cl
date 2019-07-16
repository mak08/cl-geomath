;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-04-01 21:40:31>

(in-package :cl-geomath)

(defun find-place (name)
  (let ((symbol
         (find-symbol (format nil "+~@:(~a~)+" name) :virtualhelm)))
    (if symbol
        (symbol-value symbol)
        (make-latlng :latr% (rad 0d0) :lngr% (rad 0d0)))))

(defvar +ALICANTE+
  (make-latlng :latr% (rad 38.322222d0) :lngr% (rad  -0.48444d0)))

(defvar +CAPETOWN+
  (make-latlng :latr% (rad -33.91028d0) :lngr% (rad 18.300278d0)))

(defvar +CARTHAGO+
  (make-latlng :latr% (rad 36.795d0) :lngr% (rad 10.392d0)))

(defvar +FEHMARN+
  (make-latlng :latr% (rad 54.434403d0) :lngr% (rad 11.361632d0)))

(defvar +FREMANTLE+
  (make-latlng :latr% (rad -32.060833d0) :lngr% (rad 115.72361d0)))

(defvar +GIBRALTAR+
  (make-latlng :latr% (rad 35.961877d0) :lngr% (rad -5.570280d0)))

(defvar +LACORUNA+
  (make-latlng :latr% (rad 43.484812d0) :lngr% (rad -8.589764d0)))

(defvar +LESSABLES+
  (make-latlng :latr% (rad 46.479120d0) :lngr% (rad -1.794153d0)))

(defvar +LISBON+
  (make-latlng :latr% (rad 38.561906d0) :lngr% (rad -9.379930d0)))

(defvar +LIZARD-POINT+
  (make-latlng :latr% (rad 48.37852d0) :lngr% (rad -4.489018d0)))

(defvar +HONOLULU+
  (make-latlng :latr% (rad 21.333d0) :lngr% (rad -157.550d0)))

(defvar +LOS-ANGELES+
  (make-latlng :latr% (rad 33.665d0) :lngr% (rad -119.357d0)))

(defvar +MARSEILLE+
  (make-latlng :latr% (rad 43.2456d0) :lngr% (rad 5.30359d0)))

(defvar +NEW-YORK+
  (make-latlng :latr% (rad 40.43943d0) :lngr% (rad -73.92772d0)))

(defvar +OUESSANT+
  (make-latlng :LATR% (RAD 48.44861d0) :LNGR% (RAD -5.142778d0)))

(defvar +PORTOSANTO+
  (make-latlng :latr% (rad 33.005019d0) :lngr% (rad -16.42808d0)))

(defvar +YSTAD+
  (make-latlng :latr% (rad 55.391123d0) :lngr% (rad 13.792635d0)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
