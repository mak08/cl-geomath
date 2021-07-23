;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-07-22 23:17:16>

(in-package :cl-geomath)

(CL-GEOMATH:ADD-DISTANCE-ESTIMATE (make-latlng :latr% 1.57059d0 :lngr% -2d0) 1440d0 24.0d0)
(CL-GEOMATH:ADD-DISTANCE-EXACT (make-latlng :latr% 1.57059d0 :lngr% -2d0) 1440d0 24.0d0)

(CL-GEOMATH:ADD-DISTANCE-ESTIMATE (make-latlng :latr% 1.5705d0 :lngr% -2d0) 1440d0 23.0d0)
(CL-GEOMATH:ADD-DISTANCE-EXACT (make-latlng :latr% 1.5705d0 :lngr% -2d0) 1440d0 23.0d0)

(CL-GEOMATH:ADD-DISTANCE-ESTIMATE (make-latlng :latr% 1.570d0 :lngr% -2d0) 1440d0 23.0d0)
(CL-GEOMATH:ADD-DISTANCE-EXACT (make-latlng :latr% 1.570d0 :lngr% -2d0) 1440d0 23.0d0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :execute)
  (defmacro check-delta-equal (form result &optional (delta  0.00001))
    `(progn
       (log2:info "Checking ~a => ~a" ',form ',result)
       (assert (< (abs (- ,form ,result)) ,delta))
       t)
    )

  (defmacro check-equal (form result)
    `(progn
       (log2:info "Checking ~a => ~a" ',form ',result)
       (assert (equal ,form ,result))
       t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  bilinear x y v00 v01 v10 v11
;;;       x  x+1
;;;  y   00  10
;;;  y+1 01  11

(check-equal (bilinear 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear 0d0 1d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear 1d0 0d0 1d0 2d0 3d0 4d0)
             3.0d0)
(check-equal (bilinear 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)

(check-equal (bilinear 0d0 0d0 900d0 901d0 910d0 911d0)
             900d0)
(check-equal (bilinear 1d0 0d0 900d0 901d0 910d0 911d0)
             910.d0)
(check-equal (bilinear 0d0 1d0 900d0 901d0 910d0 911d0)
             901d0)
(check-equal (bilinear 1d0 1d0 900d0 901d0 910d0 911d0)
             911.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal (bilinear-unit 0d0 0d0 1d0 2d0 3d0 4d0)
             1.0d0)
(check-equal (bilinear-unit 0d0 1d0 1d0 2d0 3d0 4d0)
             2.0d0)
(check-equal (bilinear-unit 1d0 0d0 1d0 2d0 3d0 4d0)
             3.0d0)
(check-equal (bilinear-unit 1d0 1d0 1d0 2d0 3d0 4d0)
             4.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positive u is from the west
(check-equal (angle 1d0 0d0)
             270d0)
;; Positive v is form the south
(check-equal (angle 0d0 1d0)
             180d0)

(check-equal (angle 0d0 -1d0)
             0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positive u is from the west
(check-equal (angle-r 1d0 0d0)
             (* (/ 3.0 2.0) pi))

;; Positive v is form the south
(check-equal (angle-r 0d0 1d0)
             pi)

(check-equal (angle-r 0d0 -1d0)
             0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal (enorm 0d0 0d0)
             0d0)

(check-equal (enorm 0d0 2d0)
             2d0)

(check-equal (enorm 2d0 2d0)
             (* 2d0 (sqrt 2d0)))

(check-equal (enorm 3d0 4d0)
             5d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal (p2c (angle-r 0d0 10d0) (enorm 0d0 10d0)) 0d0)
(check-delta-equal (p2c (angle-r 5d0 10d0) (enorm 5d0 10d0)) 5d0)
(check-delta-equal (p2c (angle-r 10d0 10d0) (enorm 10d0 10d0)) 10d0)
(check-delta-equal (p2c (angle-r 10d0 5d0) (enorm 10d0 5d0)) 10d0)
(check-delta-equal (p2c (angle-r 10d0 0d0) (enorm 10d0 0d0)) 10d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad 1d0)))
  44.99563d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :lat 1d0 :lng 1d0))
 44.99563d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% 1d0))
 90.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad 1d0)))
 135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% -1d0 :lngr% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad -1d0)))
 -135.004364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% -1d0))
 -90.0d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad -1d0)))
 -44.9956364d0)

(check-delta-equal
 (course-angle (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 1d0 :lngr% 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad 1d0)))
  44.9956365d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% (rad 1d0)))
 90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad 1d0)))
 135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% 0d0))
 180.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad -1d0) :lngr% (rad -1d0)))
 -135.00436354d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% 0d0 :lngr% (rad -1d0)))
 -90.0d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% (rad -1d0)))
 -44.995636455d0)

(check-delta-equal
 (course-angle-d (make-latlng :latr% 0d0 :lngr% 0d0) (make-latlng :latr% (rad 1d0) :lngr% 0d0))
 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-delta-equal (/ (course-distance +new-york+ +lizard-point+) +nautical-mile+)
                   2910.838895539127d0 2d0)
(check-delta-equal (/ (course-distance +capetown+ +lizard-point+) +nautical-mile+)
                   5086.220135360448d0 3d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50.5d0 :lng 179.5d0)
                                 (make-latlng :lat 49.5d0 :lng 179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50.5d0 :lng -179.5d0)
                                 (make-latlng :lat 49.5d0 :lng -179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50.5d0 :lng 179.5d0)
                                 (make-latlng :lat 49.5d0 :lng -179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50.5d0 :lng -179.5d0)
                                 (make-latlng :lat 49.5d0 :lng 179.5d0))
             t)


(check-equal (segment-intersects (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50.5d0 :lng 179.5d0)
                                 (make-latlng :lat 49.5d0 :lng 179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50.5d0 :lng -179.5d0)
                                 (make-latlng :lat 49.5d0 :lng -179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50.5d0 :lng 179.5d0)
                                 (make-latlng :lat 49.5d0 :lng -179.5d0))
             t)

(check-equal (segment-intersects (make-latlng :lat 50d0 :lng -179d0)
                                 (make-latlng :lat 50d0 :lng 179d0)
                                 (make-latlng :lat 50.5d0 :lng -179.5d0)
                                 (make-latlng :lat 49.5d0 :lng 179.5d0))
             t)

;; 55°59'36"S 179°59'38"E -  56°01'07"S 180°01'12"E


(defparameter *limits*
  (let ((values (list
                 (make-latlng :latr% -0.9773843811168246d0 :lngr% -3.141592653589793d0)
                 (make-latlng :latr% -0.9846571411586946d0 :lngr% -3.0543261909900767d0)
                 (make-latlng :latr% -0.9817477042468103d0 :lngr% -2.96705972839036d0)
                 (make-latlng :latr% -0.9715671717687372d0 :lngr% -2.8797932657906435d0)
                 (make-latlng :latr% -0.9555677654668954d0 :lngr% -2.792526803190927d0)
                 (make-latlng :latr% -0.9410239098588364d0 :lngr% -2.705260340591211d0)
                 (make-latlng :latr% -0.9366605867288506d0 :lngr% -2.6179938779914944d0)
                 (make-latlng :latr% -0.9366605867288506d0 :lngr% -2.530727415391778d0)
                 (make-latlng :latr% -0.9366605867288506d0 :lngr% -2.443460952792061d0)
                 (make-latlng :latr% -0.9366605867288506d0 :lngr% -2.356194490192345d0)
                 (make-latlng :latr% -0.9410239098588364d0 :lngr% -2.2689280275926285d0)
                 (make-latlng :latr% -0.9439316822950394d0 :lngr% -2.1816615649929116d0)
                 (make-latlng :latr% -0.9468411192069237d0 :lngr% -2.0943951023931953d0)
                 (make-latlng :latr% -0.9541138792487939d0 :lngr% -2.007128639793479d0)
                 (make-latlng :latr% -0.9642944117268671d0 :lngr% -1.9198621771937625d0)
                 (make-latlng :latr% -0.9730210579868387d0 :lngr% -1.8325957145940461d0)
                 (make-latlng :latr% -0.9817477042468103d0 :lngr% -1.7453292519943295d0)
                 (make-latlng :latr% -0.9948376736367678d0 :lngr% -1.6580627893946132d0)
                 (make-latlng :latr% -1.0079276430267254d0 :lngr% -1.5707963267948966d0)
                 (make-latlng :latr% -1.0210176124166828d0 :lngr% -1.48352986419518d0)
                 (make-latlng :latr% -1.0297442586766543d0 :lngr% -1.3962634015954636d0)
                 (make-latlng :latr% -1.0253809355466685d0 :lngr% -1.3089969389957472d0)
                 (make-latlng :latr% -1.0122909661567112d0 :lngr% -1.2217304763960306d0)
                 (make-latlng :latr% -0.9948376736367678d0 :lngr% -1.1344640137963142d0)
                 (make-latlng :latr% -0.9773843811168246d0 :lngr% -1.0471975511965976d0)
                 (make-latlng :latr% -0.9599310885968813d0 :lngr% -0.9599310885968813d0)
                 (make-latlng :latr% -0.8726646259971648d0 :lngr% -0.8726646259971648d0)
                 (make-latlng :latr% -0.8290313946973066d0 :lngr% -0.7853981633974483d0)
                 (make-latlng :latr% -0.8028514559173915d0 :lngr% -0.6981317007977318d0)
                 (make-latlng :latr% -0.7766715171374766d0 :lngr% -0.6108652381980153d0)
                 (make-latlng :latr% -0.767944870877505d0  :lngr% -0.5235987755982988d0)
                 (make-latlng :latr% -0.7592182246175333d0 :lngr% -0.4363323129985824d0)
                 (make-latlng :latr% -0.7592182246175333d0 :lngr% -0.3490658503988659d0)
                 (make-latlng :latr% -0.7592182246175333d0 :lngr% -0.2617993877991494d0)
                 (make-latlng :latr% -0.767944870877505d0  :lngr% -0.17453292519943295d0)
                 (make-latlng :latr% -0.7766715171374766d0 :lngr% -0.08726646259971647d0)
                 (make-latlng :latr% -0.7766715171374766d0 :lngr% 0.0d0)
                 (make-latlng :latr% -0.779580954049361d0  :lngr% 0.08726646259971647d0)
                 (make-latlng :latr% -0.782488726485564d0  :lngr% 0.17453292519943295d0)
                 (make-latlng :latr% -0.7853981633974483d0 :lngr% 0.2617993877991494d0)
                 (make-latlng :latr% -0.7883076003093326d0 :lngr% 0.3490658503988659d0)
                 (make-latlng :latr% -0.789761486527434d0  :lngr% 0.4363323129985824d0)
                 (make-latlng :latr% -0.7853981633974483d0 :lngr% 0.5235987755982988d0)
                 (make-latlng :latr% -0.7737620802255923d0 :lngr% 0.6108652381980153d0)
                 (make-latlng :latr% -0.7592182246175333d0 :lngr% 0.6981317007977318d0)
                 (make-latlng :latr% -0.7446743690094744d0 :lngr% 0.7853981633974483d0)
                 (make-latlng :latr% -0.7446743690094744d0 :lngr% 0.8726646259971648d0)
                 (make-latlng :latr% -0.7766715171374766d0 :lngr% 0.9599310885968813d0)
                 (make-latlng :latr% -0.8493941241291341d0 :lngr% 1.0471975511965976d0)
                 (make-latlng :latr% -0.8828451584752379d0 :lngr% 1.1344640137963142d0)
                 (make-latlng :latr% -0.8886640322990065d0 :lngr% 1.2217304763960306d0)
                 (make-latlng :latr% -0.8886640322990065d0 :lngr% 1.3089969389957472d0)
                 (make-latlng :latr% -0.8843007091690207d0 :lngr% 1.3962634015954636d0)
                 (make-latlng :latr% -0.8799373860390349d0 :lngr% 1.48352986419518d0)
                 (make-latlng :latr% -0.8726646259971648d0 :lngr% 1.5707963267948966d0)
                 (make-latlng :latr% -0.8552113334772213d0 :lngr% 1.6580627893946132d0)
                 (make-latlng :latr% -0.8333947178272924d0 :lngr% 1.7453292519943295d0)
                 (make-latlng :latr% -0.8028514559173915d0 :lngr% 1.8325957145940461d0)
                 (make-latlng :latr% -0.8028514559173915d0 :lngr% 1.9198621771937625d0)
                 (make-latlng :latr% -0.8028514559173915d0 :lngr% 2.007128639793479d0)
                 (make-latlng :latr% -0.8028514559173915d0 :lngr% 2.0943951023931953d0)
                 (make-latlng :latr% -0.8523018965653371d0 :lngr% 2.1816615649929116d0)
                 (make-latlng :latr% -0.8857545953871222d0 :lngr% 2.2689280275926285d0)
                 (make-latlng :latr% -0.9061173248189498d0 :lngr% 2.356194490192345d0)
                 (make-latlng :latr% -0.9119345341670373d0 :lngr% 2.443460952792061d0)
                 (make-latlng :latr% -0.9148439710789216d0 :lngr% 2.530727415391778d0)
                 (make-latlng :latr% -0.9206611804270088d0 :lngr% 2.6179938779914944d0)
                 (make-latlng :latr% -0.9293878266869805d0 :lngr% 2.705260340591211d0)
                 (make-latlng :latr% -0.9395683591650535d0 :lngr% 2.792526803190927d0)
                 (make-latlng :latr% -0.9468411192069237d0 :lngr% 2.8797932657906435d0)
                 (make-latlng :latr% -0.957021651684997d0  :lngr% 2.96705972839036d0)
                 (make-latlng :latr% -0.9672038486387514d0 :lngr% 3.0543261909900767d0)
                 (make-latlng :latr% -0.9773843811168246d0 :lngr% 3.141592653589793d0)
                 (make-latlng :latr% -0.9846571411586946d0 :lngr% -3.0543261909900767d0)
                 )))
    (make-array (length values) :initial-contents values)))

(let* ((lat0 (dms-to-decimal (make-dms :u -1d0 :d 55  :m 59 :s 36 :cs 0)))
       (lng0 (dms-to-decimal (make-dms :u 1d0 :d 179 :m 59 :s 38 :cs 0)))
       (lat1 (dms-to-decimal (make-dms :u -1d0 :d 56  :m 01 :s 07 :cs 0)))
       (lng1 (dms-to-decimal (make-dms :u 1d0 :d 180 :m 01 :s 12 :cs 0)))
       (p0 (make-latlng :lat lat0 :lng lng0))
       (p1 (make-latlng :lat lat1 :lng lng1)))
  (format t "p0: ~a, p1: ~a~%" p0 p1)
  (loop
     :for k :from 1 :below (length *limits*)
     :never (segment-intersects p0 p1 (aref *limits* (1- k)) (aref *limits* k))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
