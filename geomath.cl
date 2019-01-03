;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-03 01:04:44>
(declaim (optimize (speed 3) (debug 1)  (space 0) (safety 1)))

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lat&Lng
;; TODO: A latlng should only be used to represent Google Maps coordinates.

(defstruct latlng
  (lat% nil)
  (lng% nil)
  (latr% 0d0 :type double-float)
  (lngr% 0d0 :type double-float))

(declaim (inline latlng-lat))
(defun latlng-lat (latlng)
  (or (latlng-lat% latlng)
      (setf (latlng-lat% latlng)
            (deg (latlng-latr latlng)))))
(declaim (notinline latlng-lat))

(declaim (inline latlng-lng))
(defun latlng-lng (latlng)
  (or (latlng-lng% latlng)
      (setf (latlng-lng% latlng)
            (deg (latlng-lngr latlng)))))
(declaim (notinline latlng-lng))

(declaim (inline latlng-latr))
(defun latlng-latr (latlng)
  (declare (inline rad))
  (cond ((eql (latlng-latr% latlng) 0d0)
         (setf (latlng-latr% latlng)
               (rad (latlng-lat latlng))))
        (t
         (latlng-latr% latlng))))
(declaim (inline latlng-latr))

(declaim (inline latlng-lngr))
(defun latlng-lngr (latlng)
  (declare (inline rad))
  (cond ((eql (latlng-lngr% latlng) 0d0)
         (setf (latlng-lngr% latlng)
               (rad (latlng-lng latlng))))
        (t
         (latlng-lngr% latlng))))
(declaim (notinline latlng-lngr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(defconstant 180/pi (/ 180d0 pi))

(declaim (inline angle))
(defun angle (u v)
  (declare (double-float u v))
  (let ((angle
         (+ 180d0 (* 180/pi (atan u v)))))
    (if (< angle 360d0)
        angle
        (- angle 360d0))))
(declaim (notinline angle))

(declaim (inline angle-r))
(defun angle-r (u v)
  (declare (double-float u v))
  (let ((angle
         (+ pi (atan u v))))
    (if (< angle (* 2 pi))
        angle
        (- angle (* 2 pi)))))
(declaim (notinline angle-r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm
(declaim (inline enorm))
(defun enorm (x y)
  (declare (double-float x y))
  (sqrt (+ (* x x) (* y y))))
(declaim (notinline enorm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polar to Cartesian

(declaim (inline p2c))
(defun p2c (a r)
  (let ((c (cis a)))
    (values 
     (- (* r (imagpart c)))
     (- (* r (realpart c))))))
(declaim (notinline p2c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation

(declaim (inline linear))
(defun linear (fraction a b)
  (+ a (* fraction (- b a))))
(declaim (notinline linear))

(declaim (inline bilinear))
(defun bilinear (dw da v00 v01 v10 v11)
  ;; Bilinear interpolation at P=(w a) given values f(w0, a0) = v00 etc.
  ;; If a0=a1 (w0=w1) interpolate at the resp. midpoints of v00, v01, v10, v11
  (declare (double-float dw da v00 v10 v01 v11))
  (let* ((v0
          (+ v00 (* dw (- v01 v00))))
         (v1
          (+ v10 (* dw (- v11 v10))))
         (v
          (+ v0 (* da (- v1 v0)))))
    (declare (double-float dw v0 v1 da v))
    v))
(declaim (notinline bilinear))
 
(declaim (inline fraction-index))
(defun fraction-index (value steps)
  (loop
     :for step :across steps
     :for index :from 0
     :while (and (< index (length steps))
                 (<= step value))
     :finally (return (values (1- index)
                              (/ (- value (aref steps (1- index)))
                                 (- step (aref steps (1- index))))))))
(declaim (notinline fraction-index))

(declaim (inline bilinear-unit))
(defun bilinear-unit (x y f00 f01 f10 f11)
  ;; (declare (double-float x y f00 f01 f10 f11))
  (+ (* f00 (- 1d0 x) (- 1d0 y))
     (* f01 x (- 1d0 y))
     (* f10 (- 1d0 x) y)
     (* f11 x y)))
(declaim (notinline bilinear-unit))

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
(defun gc-angle (origin target)
  "Compute great circle angle between origin and target"
  (let* ((lat1 (latlng-latr origin))
         (lat2 (latlng-latr target))
         (lon1 (latlng-lngr origin))
         (lon2 (latlng-lngr target))
         (d (- lon2 lon1)))
    (declare (double-float lat1 lat2 lon1 lon2 d))
    (acos
     (+ (* (sin lat1) (sin lat2))
        (* (cos lat1) (cos lat2) (cos d))))))
(declaim (notinline gc-angle))

(declaim (inline  gc-angle-hvs))
(defun gc-angle-hvs (origin target)
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
      (* 2 (asin (sqrt (+ (sin2 dlat/2) (* (cos lat1) (cos lat2) (sin2 dlon/2)))))))))
(declaim (notinline gc-angle-hvs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spherical distance

(declaim (inline add-distance-exact add-distance-estimate))
(defun add-distance-exact (pos distance alpha)
  ;; Exact calculation on the spherical Earth
  (let ((lat-r (latlng-latr pos))
        (lon-r (latlng-lngr pos)))
    (declare (double-float lat-r lon-r distance alpha))
    (let* ((d (/ distance +radius+))
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
                         (asin (/ (* sin-a sin-d)
                                  (cos lat-new-r))))))
      (declare (double-float d a cos-d sin-d cos-lat-r sin-lat-r lat-new-r lon-new-r))
      (make-latlng :latr% lat-new-r
                   :lngr% lon-new-r))))
(declaim (notinline add-distance-exact))

(declaim (inline add-distance-estimate))
(defun add-distance-estimate (pos distance alpha)
  ;; Approximation for short distances (<< 100km)
  (let ((lat-r (latlng-latr pos))
        (lon-r (latlng-lngr pos)))
    (declare (double-float lat-r lon-r distance alpha))
    (let* ((d (/ distance +radius+))
           (a (rad alpha))
           (d-lat-r (* d (cos a)))
           (d-lon-r (* d (/ (sin a) (cos (+ lat-r d-lat-r))))))
      (declare (double-float a))
      (make-latlng :latr% (+ lat-r d-lat-r)
                   :lngr% (+ lon-r d-lon-r)))))
(declaim (notinline add-distance-estimate))

(defun longitudinal-distance (latlng1 latlng2)
  (let* ((l1 (latlng-lng latlng1))
         (l2 (latlng-lng latlng2))
         (d (abs (- l1 l2))))
    (if (<= d 180)
        d
        (- 360 d))))

(declaim (inline  longitudinal-direction))
(defun longitudinal-direction (start dest)
  (let* ((start-lng (latlng-lng start))
         (dest-lng (latlng-lng dest))
         (sign (- start-lng dest-lng))
         (delta (abs sign)))
    (if (<= delta 180)
        (if (<= sign 0) 1 -1)
        (if (<= sign 0) -1 1))))
(declaim (notinline  longitudinal-direction))

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
    (declare (double-float lat1 lon1 lat2 lon2 cos-lat1 sin-lat1 cos-lat2 sin-lat2))
    (* +radius+
       (acos (+ (* sin-lat1 sin-lat2)
                (* cos-lat1 cos-lat2 (cos (- lon2 lon1))))))))
(declaim (notinline course-distance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - HEADING values range from 0..360 (clockwise from north)
;;; - ANGLE  values range from -179..180 (starboard downwind to port downwind)  

(deftype heading () `(integer 0 360))
(deftype angle () `(double-float -179.99999999d0 180.0d0))

(declaim (inline normalize-heading))
(defun normalize-heading (value)
  (declare (double-float value))
  (if (> value 360d0)
      (- value 360d0)
      (if (< value 0d0)
          (+ value 360d0)
          value)))
(declaim (notinline normalize-heading))

(declaim (inline normalize-angle))
(defun normalize-angle (value)
  (if (<= value -180d0)
      (+ value 360d0)
      (if (> value 180d0)
          (- value 360d0)
          value)))
(declaim (notinline normalize-angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Course angle

(declaim (inline course-angle))
(defun course-angle (origin target &optional (xi (gc-angle-hvs origin target)))
  "Compute course angle using central angle" 
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
               (let ((omega%
                      (acos cos-omega)))
                 (if (complexp omega%)
                     (realpart omega%)
                     omega%)))
              (ld (longitudinal-direction origin target)))
         (declare (double-float cos-omega omega))
         (normalize-angle
          (deg
           (if (= ld 1)
               omega
               (- (* PI 2) omega)))))))))
(declaim (notinline course-angle))

(declaim (inline course-angle-d))
(defun course-angle-d (origin target &optional (dist (course-distance origin target)))
  "Compute course angle using great circle distance"
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
       (let* ((e (/ dist +radius+))
              (cos-omega
               (/ (- (sin lat2) (* sin-lat1 (cos e)))
                  (* cos-lat1 (sin e))))
              (omega
               (let ((omega%
                      (acos cos-omega)))
                 (if (complexp omega%)
                     (realpart omega%)
                     omega%)))
              (ld (longitudinal-direction origin target)))
         (declare (double-float cos-omega omega))
         (normalize-angle
          (deg
           (if (= ld 1)
               omega
               (- (* PI 2) omega)))))))))
(declaim (notinline course-angle-d))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
