;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 03:04:29>

(in-package :cl-geomath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DMS format:
;;;     ddd°mm'ss"
;;;
;;; Reading fails (and probably produces strange errors) if the coordinate
;;; is not in this format.

(defmethod print-object ((thing dms) stream)
  (format stream  "~3,'0d°~2,'0d'~2,'0d\""
          (dms-degrees thing)
          (dms-minutes thing)
          (round (dms-seconds thing))))

(defun read-dms (stream)
  (flet ((decode (c0 c1 c2)
           (+ (* (- (char-code c0) 48) 100d0)
              (* (- (char-code c1) 48) 10d0)
              (- (char-code c2) 48))))
    (let ((chars
           (loop :for k :below 10 :collect (read-char stream t nil nil))))
      (make-dms :degrees (decode (nth 0 chars) (nth 1 chars) (nth 2 chars)) 
                :minutes (decode #\0 (nth 4 chars) (nth 5 chars))
                :seconds (decode #\0 (nth 7 chars) (nth 8 chars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates format:
;;;     #p[ddd°mm'ss"X, ddd°mm'ss"Y]
;;; where X = N|S, Y = E|W

(defmethod print-object ((thing latlng) stream)
  (let ((lat-value
         (decimal2dms (abs (latlng-lat thing))))
        (lng-value
         (decimal2dms (abs (latlng-lng thing))))
        (lat-marker
         (if (< (latlng-lat thing) 0) "S" "N"))
        (lng-marker
         (if (< (latlng-lng thing) 0) "W" "E")))
    (format stream "#p[~a~a, ~a~a]" lat-value lat-marker lng-value lng-marker)))

(set-dispatch-macro-character #\# #\p 'read-latlng)

(defun read-latlng (stream sub-char arg)
  (declare (ignore sub-char arg))
  (let ((open-bracket (read-char stream))
        (latitude (read-dms stream))
        (lat-marker (read-char stream))
        (comma (read-char stream))
        (space (read-char stream))
        (longitude (read-dms stream))
        (lng-marker (read-char stream))
        (close-bracket (read-char stream)))
    (declare (ignore open-bracket comma space close-bracket))
    (let ((lat-sign (ecase lat-marker (#\N 1) (#\S -1)))
          (lng-sign (ecase lng-marker (#\E 1) (#\W -1))))
      (make-latlng :lat% (* lat-sign (dms2decimal latitude))
                   :lng% (* lng-sign (dms2decimal longitude))))))
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
