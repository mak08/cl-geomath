;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2022
;;; Last Modified <michael 2024-02-16 01:19:07>

(in-package :cl-geomath)


(declaim (inline intersectino-sign))
(defun intersection-sign (x y x0 y0 x1 y1)
  (when (= y0 y y1)
    (if (or (<= x0 x x1)
            (<= x1 x x0))
        (return-from intersection-sign 0)
        (return-from intersection-sign 1)))
  (when (and (= y y0)
             (= x x0))
    (return-from intersection-sign 0))
  (when (> y0 y1)
    (rotatef y0 y1)
    (rotatef x0 x1))
  (when (or (<= y y0)
            (> y y1))
    (return-from intersection-sign 1))
  (let ((d (- (* (- x0 x) (- y1 y))
              (* (- y0 y) (- x1 x)))))
    (cond
      ((> d 0)
       (return-from intersection-sign -1))
      ((< d 0)
       (return-from intersection-sign 1))
      (t
       (return-from intersection-sign 0)))))

(defun point-in-poly-p (point polygon)
  ;; https://de.wikipedia.org/wiki/Punkt-in-Polygon-Test_nach_Jordan
  (loop
    :with r = -1
    :with x :of-type double-float = (latlng-lngr point)
    :with y :of-type double-float = (latlng-latr point)
    :for k :below (1- (length polygon))
    :for p0 = (aref polygon k)
    :for p1 = (aref polygon (1+ k))
    :for x0 :of-type double-float = (latlng-lngr p0)
    :for y0 :of-type double-float = (latlng-latr p0)
    :for x1 :of-type double-float = (latlng-lngr p1)
    :for y1 :of-type double-float = (latlng-latr p1)
    :do  (setf r (* r (intersection-sign x y x0 y0 x1 y1)))
    :finally
       (return (>= r 0))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
