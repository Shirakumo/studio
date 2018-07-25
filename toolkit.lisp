#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(defun maybe-parse-integer (thing &optional default)
  (if (and thing (string/= thing ""))
      (parse-integer thing)
      default))

(defun parse-date (date)
  (let (y m)
    (handler-case
        (let ((dot (position #\. date)))
          (setf m (parse-integer date :start 0 :end dot)
                y (parse-integer date :start (1+ dot))))
      (error (e)
        (declare (ignore e))
        (multiple-value-bind (ss mm hh d cm cy)
            (decode-universal-time (get-universal-time) 0)
          (declare (ignore ss mm hh d))
          (setf y cy m cm))))
    (list (encode-universal-time 0 0 0 1 m y 0)
          (multiple-value-bind (y+ m) (floor (1+ m) 12)
            (encode-universal-time 0 0 0 1 m (+ y y+) 0)))))

(defun format-date (time)
  (multiple-value-bind (ss mm hh d m y) (decode-universal-time time 0)
    (declare (ignore ss mm hh d))
    (format NIL "~d.~d" m y)))

(defun adjust-date (time months)
  (multiple-value-bind (ss mm hh d m y) (decode-universal-time time 0)
    (declare (ignore ss mm hh d))
    (multiple-value-bind (y+ m) (floor (+ m -1 months) 12)
      (encode-universal-time 0 0 0 1 (1+ m) (+ y y+) 0))))

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

(defmacro mktable (&rest entries)
  (let ((table (gensym "TABLE")))
    `(let ((,table (make-hash-table :test 'eql)))
       ,@(loop for (key val) on entries by #'cddr
               collect `(setf (gethash ',key ,table) ,val))
       ,table)))
