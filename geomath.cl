;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-04-06 18:29:39>

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(declaim (float 180/pi))
(defconstant 180/pi (coerce (/ 180d0 pi) 'double-float))
(defconstant pi/2 (coerce (/  pi 2d0) 'double-float))
(defconstant 2pi (coerce (*  pi 2d0) 'double-float))

(declaim (inline angle)
         (ftype (function (double-float double-float) double-float) angle))
(defun-t angle double-float ((u double-float) (v double-float))
  (declare (double-float u v 180/pi))
  (let ((a
         (+ 180d0 (* 180/pi (atan u v)))))
    (declare (double-float a))
    (if (< a 360d0) a (- a 360d0))))

;; (declaim (notinline angle))

(declaim (inline angle-r))
(defun angle-r (u v)
  (declare (double-float u v))
  (let ((angle
         (+ pi (atan u v))))
    (if (< angle (* 2d0 pi))
        angle
        (- angle (* 2d0 pi)))))
;; (declaim (notinline angle-r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm
(declaim (inline enorm))
(defun-t enorm double-float ((x double-float) (y double-float))
  (sqrt  (+ (* x x)
            (* y y))))

;; (declaim (notinline enorm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polar to Cartesian

(declaim (inline p2c))
(defun p2c (a r)
  (declare (double-float a r))
  (let ((c (the complex (cis a))))
    (values 
     (- (* r (imagpart c)))
     (- (* r (realpart c))))))
;; (declaim (notinline p2c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation

(declaim (inline linear))
(defun-t linear double-float ((fraction double-float)
                              (a double-float)
                              (b double-float))
  (+ a (* fraction (- b a))))
;; (declaim (notinline linear))

(declaim (inline bilinear))
(defun-t bilinear double-float ((u   double-float) 
                                (v  double-float)
                                (w00    double-float)
                                (w01    double-float)
                                (w10    double-float)
                                (w11 double-float))
  "Bilinear interpolation at point (u v) given values  w_ik = f(u_i, v_k)"
  (let* ((w0
          (+ w00 (* u (- w10 w00))))
         (w1
          (+ w01 (* u (- w11 w01))))
         (w
          (+ w0 (* v (- w1 w0)))))
    w))
;; (declaim (notinline bilinear))

(declaim (inline fraction-index))
(defun-t fraction-index (values fixnum float) ((value double-float) (steps t))
  (declare (simple-array steps))
  (loop
     :for step :across steps
     :for index :from 0
     :while (and (< index (length steps))
                 (<= step value))
     :finally (return (values (1- index)
                              (/ (- value (aref steps (1- index)))
                                 (- step (aref steps (1- index))))))))
;; (declaim (notinline fraction-index))

(declaim (inline bilinear-unit))
(defun-t bilinear-unit double-float ((x  double-float) (y double-float) (f00  double-float) (f01  double-float) (f10  double-float) (f11  double-float))
  (+ (* f00 (- 1d0 x) (- 1d0 y))
     (* f01 (- 1d0 x) y)
     (* f10 x (- 1d0 y))
     (* f11 x y)))
;; (declaim (notinline bilinear-unit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defun round-to-digits (x d)
  (declare ((integer 0 10) d) (double-float x))
  (let ((divisor (expt 10 d)))
    (/ (the double-float (fround x (the double-float (/ 1d0 divisor)))) divisor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;;; http://www.kompf.de/gps/distcalc.html
;;; http://www.kompf.de/trekka/distance.php?lat1=52.5164&lon1=13.3777&lat2=38.692668&lon2=-9.177944
;;; http://de.wikipedia.org/wiki/Gro%C3%9Fkreis

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Great circle angle

(declaim (inline gc-angle))
(defun-t gc-angle double-float ((origin latlng) (target latlng))
  "Compute great circle angle between origin and target"
  (let* ((lat1 (latlng-latr origin))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target))
         (d (- lon2 lon1)))
    (declare (double-float lat1 lat2 lon1 lon2 d))
    (the double-float
         (acos
          (+ (* (sin lat1) (sin lat2))
             (* (cos lat1) (cos lat2) (cos d)))))))
;; (declaim (notinline gc-angle))

(declaim (inline  gc-angle-hvs))
(defun-t gc-angle-hvs double-float ((origin latlng) (target latlng))
  "Compute great circle angle using the haversine formula"
  (let* ((lat1 (latlng-latr origin))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target))
         (dlat/2 (/ (- lat2 lat1) 2))
         (dlon/2 (/ (- lon2 lon1) 2)))
    (declare (double-float lat1 lat2 lon1 lon2 dlat/2 dlon/2))
    (flet ((sin2 (x)
             (let ((sinx (sin x)))
               (* sinx sinx))))
      (declare (inline sin2))
      (* 2d0 (the double-float
                  (asin
                   (the double-float
                        (sqrt (the (double-float 0d0)
                                   (+
                                    (sin2 dlat/2)
                                    (* (cos lat1)
                                       (cos lat2)
                                       (sin2 dlon/2))))))))))))
;; (declaim (notinline gc-angle-hvs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spherical distance

(declaim (inline add-distance-exact))
(defun add-distance-exact (pos distance alpha)
  ;; Exact calculation on the spherical Earth
  (declare (double-float distance alpha)
           (inline latlng-latr latlng-lngr))
  (let ((lat-r (latlng-latr pos))
        (lon-r (latlng-lngr pos)))
    (declare (double-float lat-r lon-r))
    (let* ((d (* distance +1/radius+))
           (cis-d (cis d))
           (cos-d (realpart cis-d))
           (sin-d (imagpart cis-d))
           (a (rad alpha))
           (sin-a (sin a))
           (cis-lat-r (cis lat-r))
           (cos-lat-r (realpart cis-lat-r))
           (sin-lat-r (imagpart cis-lat-r))
           (lat-new-r (asin (+ (* sin-lat-r cos-d)
                               (* cos-lat-r sin-d (cos a)))))
           (lon-new-r (+ lon-r
                         (the double-float
                              (asin (/ (* sin-a sin-d)
                                       (cos lat-new-r)))))))
      (declare (double-float d a cos-d sin-d cos-lat-r sin-lat-r lat-new-r lon-new-r))
      (make-latlng :latr% lat-new-r
                   :lngr% lon-new-r))))
;; (declaim (notinline add-distance-exact))

(declaim (inline add-distance-estimate))
(defun add-distance-estimate (pos distance alpha)
  ;; Approximation for short distances (<< 100km)
  (declare (double-float distance alpha)
           (inline latlng-latr latlng-lngr))
  (let ((lat-r (latlng-latr pos))
        (lon-r (latlng-lngr pos)))
    (declare (double-float lat-r lon-r))
    (let* ((d (* distance +1/radius+))
           (a (rad alpha))
           (d-lat-r (* d (cos a)))
           (d-lon-r (* d (/ (sin a) (cos (+ lat-r d-lat-r))))))
      (declare (double-float a))
      (make-latlng :latr% (+ lat-r d-lat-r)
                   :lngr% (+ lon-r d-lon-r)))))
;; (declaim (notinline add-distance-estimate))

(declaim (inline longitudinal-distance))
(defun longitudinal-distance (latlng1 latlng2)
  (let* ((l1 (latlng-lngr latlng1))
         (l2 (latlng-lngr latlng2))
         (d (abs (- l1 l2))))
    (declare (double-float l1 l2 d)
             (ftype (function (float) double-float) deg))
    (if (<= d pi)
        (deg d)
        (- 360d0 (deg d)))))
;; (declaim (notinline longitudinal-distance))

(declaim (inline  longitudinal-direction))
(defun longitudinal-direction (start dest)
  (let* ((start-lng (latlng-lngr start))
         (dest-lng (latlng-lngr dest))
         (sign (- start-lng dest-lng))
         (delta (abs sign)))
    (declare (double-float sign delta))
    (if (<= delta pi)
        (if (<= sign 0) 1d0 -1d0)
        (if (<= sign 0) -1d0 1d0))))
;; (declaim (notinline  longitudinal-direction))

(declaim (inline course-distance))
(defun course-distance (origin target)
  (declare (ftype (function (t) double-float) latlng-latr latlng-lngr))
  (let* ((lat1 (latlng-latr origin))
         (lon1 (latlng-lngr origin))
         (lat2 (latlng-latr target))
         (lon2 (latlng-lngr target))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (cis-lat2 (cis lat2))
         (cos-lat2 (realpart cis-lat2))
         (sin-lat2 (imagpart cis-lat2)))
    (declare (double-float +radius+ lat1 lon1 lat2 lon2 cos-lat1 sin-lat1 cos-lat2 sin-lat2))
    (* (the double-float +radius+)
       (the double-float
            (acos (the double-float
                       (+ (* sin-lat1 sin-lat2)
                          (* cos-lat1 cos-lat2 (cos (- lon2 lon1))))))))))
;; (declaim (notinline course-distance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - HEADING values range from 0..360 (clockwise from north)
;;; - ANGLE  values range from -179..180 (starboard downwind to port downwind)  

(deftype heading () `(integer 0 360))
(deftype angle () `(float -179.99999999d0 180.0d0))

(declaim (inline normalize-heading))
(defun-t normalize-heading double-float ((value double-float))
  (if (> value 360d0)
      (- value 360d0)
      (if (< value 0d0)
          (+ value 360d0)
          value)))

(declaim (inline normalize-angle))
(declaim (ftype (function (double-float) double-float) normalize-angle))
(defun normalize-angle (value)
  (declare (double-float value))
  (if (<= value -180d0)
      (the double-float (+ value 360d0))
      (if (> value 180d0)
          (the double-float (- value 360d0))
          value)))
;; (declaim (notinline normalize-angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Course angle

(declaim (inline course-angle))
(defun course-angle (origin target &optional (xi (gc-angle-hvs origin target)))
  "Compute course angle using central angle" 
  (declare (inline latlng-latr latlng-lngr longitudinal-direction)
           (type latlng origin target))
  (let* ((lat1 (latlng-latr origin))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target)))
    (declare (double-float lat1 cos-lat1 sin-lat1 lon1 lat2 lon2 xi))
    (cond
      ((and
        (eql lat1 lat2)
        (eql lon1 lon2))
       (error "Distance is zero between ~a and ~a" origin target))
      (t
       (let* ((cos-omega
               (/ (- (sin lat2) (* sin-lat1 (cos xi)))
                  (* cos-lat1 (sin xi))))
              (omega
                (acos
                 (min 1.0d0
                      (max -1.0d0 cos-omega))))
              (ld (longitudinal-direction origin target)))
         (declare (double-float cos-omega omega))
         (normalize-angle
          (deg
           (if (= ld 1)
               omega
               (- (* PI 2) omega)))))))))
;; (declaim (notinline course-angle))

(declaim (inline course-angle-d))
(defun course-angle-d (origin target &optional (dist (course-distance origin target)))
  "Compute course angle using great circle distance"
  (declare (inline latlng-latr latlng-lngr longitudinal-direction normalize-angle)
           (type latlng origin target))
  (let* ((lat1 (latlng-latr origin))
         (cis-lat1 (cis lat1))
         (cos-lat1 (realpart cis-lat1))
         (sin-lat1 (imagpart cis-lat1))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target)))
    (declare (double-float dist lat1 cos-lat1 sin-lat1 lon1 lat2 lon2))
    (cond
      ((and
        (eql lat1 lat2)
        (eql lon1 lon2))
       (error "Distance is zero between ~a and ~a" origin target))
      (t
       (when (eql dist 0d0)
         (error "Distance is zero between ~a and ~a" origin target))
       (let* ((e (* dist +1/radius+))
              (cos-omega
               (/ (- (sin lat2) (* sin-lat1 (cos e)))
                  (* cos-lat1 (sin e))))
              (omega
                (acos
                 (min 1.0d0
                      (max -1.0d0 cos-omega))))
              (ld (longitudinal-direction origin target)))
         (declare (double-float cos-omega omega ld))
         (normalize-angle
          (the double-float
               (deg
                (if (= ld 1d0)
                    omega
                    (- (* PI 2d0) omega))))))))))
;; (declaim (notinline course-angle-d))

#|
(let ((omega%
       (acos cos-omega)))
  (if (complexp omega%)
      (realpart omega%)
      omega%))
|#

;; Test if line segment (p1, p2) and (q1, q2) intersect.
;; Consider parallel lines non-intersecting even if they coincide.
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
(declaim (inline segment-intersects))
(defun segment-intersects (p1 p2 q1 q2)
  (let ((x1 (latlng-lngr p1))
        (y1 (latlng-latr p1))
        (x2 (latlng-lngr p2))
        (y2 (latlng-latr p2))
        (x3 (latlng-lngr q1))
        (y3 (latlng-latr q1))
        (x4 (latlng-lngr q2))
        (y4 (latlng-latr q2)))
    (declare (double-float x1 x2 x3 x4 y1 y2 y3 y4))
    ;; (when (< x1 0d0) (incf x1 2pi)) (when (< x2 0d0) (incf x2 2pi)) (when (< x3 0d0) (incf x3 2pi)) (when (< x4 0d0) (incf x4 2pi))
    (when (or (> (abs (- x2 x1)) pi)
              (> (abs (- x3 x4)) pi))
      (when (< x1 (- pi/2)) (incf x1 2pi))
      (when (< x2 (- pi/2)) (incf x2 2pi))
      (when (< x3 (- pi/2)) (incf x3 2pi))
      (when (< x4 (- pi/2)) (incf x4 2pi)))
    (let* ((dx12 (- x1 x2))
           (dx13 (- x1 x3))
           (dx34 (- x3 x4))
           (dy12 (- y1 y2))
           (dy13 (- y1 y3))
           (dy34 (- y3 y4))
           (px12y34 (* dx12 dy34))
           (px12y13 (* dx12 dy13))
           (px13y34 (* dx13 dy34))
           (py12x34 (* dy12 dx34))
           (py12x13 (* dy12 dx13))
           (py13x34 (* dy13 dx34))
           (denom
            (- px12y34 py12x34))
           (nom-t
            (- px13y34 py13x34))
           (nom-u
            (- px12y13 py12x13)))

      (declare (double-float dx12 dx13 dx34 dy12 dy13 dy34
                             px12y34 px12y13 px13y34
                             py12x34 py12x13 py13x34))
      (or 
          (and (> denom 0)
               (< 0 nom-t denom)
               (> 0 nom-u (- denom)))
          (and (< denom 0)
               (< denom nom-t 0)
               (< 0 nom-u (- denom)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defun longitude-between (west east longitude)
  (declare (double-float west east longitude))
  (or (<= west longitude  east)
      (and (<= east west)
           (not (<= east longitude west)))))

(declaim (inline heading-between))
(defun heading-between (left right heading)
  (declare (double-float left right heading))
  (cond ((<= left right)
         (<= left heading right))
        (t
         (or
          (<= left heading 360d0)
          (<= 0d0 heading right)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
