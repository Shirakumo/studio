#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(defvar *import-jobs* (make-hash-table :test 'eql))

(defun import-job (user)
  (gethash (user:id user) *import-jobs*))

(defun (setf import-job) (job user)
  (if job
      (setf (gethash (user:id user) *import-jobs*) job)
      (remhash (user:id user) *import-jobs*)))

(defclass import-job ()
  ((thread :initform NIL :accessor thread)
   (status :initform :created :accessor status)
   (user :initarg :user :accessor user)
   (start-time :initform (get-universal-time) :accessor start-time)
   (results :initform () :accessor results))
  (:default-initargs :user (error "USER required.")))

(defmethod run-import :around ((job import-job))
  (setf (status job) :running)
  (restart-case
      (prog1 (call-next-method)
        (setf (status job) :completed))
    (abort-job ()
      :report "Abort the import job."
      (setf (status job) :aborted))))

(defmethod start-import ((job import-job))
  (when (and (thread job) (bt:thread-alive-p (thread job)))
    (error "Job already started."))
  (when (and (import-job (user job))
             (case (status (import-job (user job)))
               ((:created :running) T)))
    (error "A job for this user is already running."))
  (setf (import-job (user job)) job)
  (setf (thread job) (bt:make-thread (lambda () (run-import job))
                                     :name (format NIL "~d import job" (user job))))
  job)

(defmethod stop-import ((job import-job))
  (when (and (thread job) (bt:thread-alive-p (thread job)))
    (let ((thread (shiftf (thread job) NIL)))
      (loop repeat 100
            while (bt:thread-alive-p thread)
            do (sleep 0.001)
            finally (when (bt:thread-alive-p thread)
                      (bt:interrupt-thread thread (lambda () (invoke-restart 'abort-job))))))
    (setf (import-job (user job)) NIL))
  job)

(define-page import-overview "studio/^import$" (:clip "import.ctml")
  (check-permitted :import)
  (r-clip:process
   T :author (user:username (auth:current))
     :job (import-job (auth:current))))

(define-api studio/import/status () ()
  (check-permitted :import)
  (let ((job (import-job (auth:current))))
    (api-output (when job
                  (mktable :status (status job)
                           :user (user job)
                           :start-time (start-time job)
                           :results (mapcar #'upload->table (results job)))))))

(define-api studio/import/stop () ()
  (check-permitted :import)
  (let ((job (import-job (auth:current))))
    (cond (job
           (stop-import job)
           (if (string= "true" (post/get "browser"))
               (redirect (referer))
               (api-output T :message "Import job stopped.")))
          (T
           (if (string= "true" (post/get "browser"))
               (redirect (referer))
               (api-output NIL :message "No import job active."))))))
