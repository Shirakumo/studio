#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-api studio/file (id) ()
  (handler-case
      (let ((file (ensure-file (db:ensure-id id))))
        (setf (header "Cache-Control") "public, max-age=31536000")
        (serve-file (file-pathname file) (dm:field file "type")))
    (error (e)
      (error 'request-not-found :message (princ-to-string e)))))

(defun api-upload (upload)
  (alexandria:plist-hash-table
   (list :id (princ-to-string (dm:id upload))
         :url (upload-link upload)
         :title (dm:field upload "title")
         :author (user:username (dm:field upload "author"))
         :tags (upload-tags upload)
         :time (dm:field upload "time")
         :files (mapcar #'dm:id (upload-files upload)))))

(define-api studio/uploads (&optional user tag date skip amount) ()
  (let ((skip (if skip (parse-integer skip) 0))
        (amount (if amount (parse-integer amount))))
    (api-output (mapcar #'api-upload (uploads :user user :tag tag :date date :skip skip :amount amount)))))

(define-api studio/upload (id) ()
  (api-output (api-upload (ensure-upload id))))

(define-api studio/upload/create (title file[] &optional description tags) ()
  ;; FIXME: Check permissions
  (unless (<= 1 (length title) 64)
    (error "Title must be between 1 and 64 characters long."))
  (let ((upload (make-upload title file[] :description description
                                          :tags (cl-ppcre:split "(\\s*,\\s*)+" tags))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (api-upload upload)))))

(define-api studio/upload/edit (id &optional title description delete[] file[] tags) ()
  ;; FIXME: Check permissions
  (let ((upload (update-upload id
                               :title title
                               :description description
                               :delete-files delete[]
                               :new-files file[]
                               :tags (cl-ppcre:split "(\\s*,\\s*)+" tags))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (api-upload upload)))))

(define-api studio/upload/delete (id) ()
  ;; FIXME: Check permissions
  (let ((upload (ensure-upload id)))
    (delete-upload id)
    (if (string= (post/get "browser") "true")
        (redirect (gallery-link :user (dm:field upload "author"))))))
