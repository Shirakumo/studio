#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)


(defun date-range (date &optional (page 1))
  (multiple-value-bind (ss mm hh d m y day dl-p tz) (decode-universal-time date 0)
    (declare (ignore ss mm hh d day dl-p))
    (values (multiple-value-bind (y+ m) (floor (- m page) 12)
              (encode-universal-time 0 0 0 1 (1+ m) (+ y y+) tz))
            (multiple-value-bind (y+ m) (floor (- (1+ m) page) 12)
              (encode-universal-time 0 0 0 1 (1+ m) (+ y y+) tz)))))

(defun format-month (upload)
  (multiple-value-bind (ss mm hh d m y)
      (decode-universal-time (dm:field (ensure-upload upload) "time"))
    (declare (ignore ss mm hh d))
    (format NIL "~2d.~4d" m y)))

(defun visibility->int (visibility)
  (ecase visibility
    (:public 0)
    (:hidden 1)
    (:private 2)))

(define-compiler-macro visibility->int (&whole whole visibility &environment env)
  (if (constantp visibility env)
      `(load-time-value (ecase ,visibility
                          (:public 0)
                          (:hidden 1)
                          (:private 2)))
      whole))

(defun ->visibility (visibility)
  (case visibility
    ((0 :public) :public)
    ((1 :hidden) :hidden)
    ((2 :private) :private)
    (T (cond ((string= visibility "public") :public)
             ((string= visibility "hidden") :hidden)
             ((string= visibility "private") :private)
             (T (error "Invalid visibility: ~s" visibility))))))

(defun visibility->icon (visibility)
  (ecase visibility
    (:public "fa-globe")
    (:hidden "fa-eye-slash")
    (:private "fa-lock")))

(defun collection->name (collection)
  (ecase collection
    (uploads "upload")
    (galleries "gallery")
    (tags "tag")
    (files "file")))
