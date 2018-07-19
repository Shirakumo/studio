#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-api studio/file (id &optional thumb) ()
  (handler-case
      (let ((file (ensure-file (db:ensure-id id))))
        (setf (header "Cache-Control") "public, max-age=31536000")
        (serve-file (file-pathname file :thumb (or* thumb)) (dm:field file "type")))
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

(define-api studio/gallery/list (&optional skip amount) ()
  (let ((skip (if skip (parse-integer skip) 0))
        (amount (if amount (parse-integer amount))))
    (api-output (mapcar #'api-upload (uploads :user user :tag tag :date date :skip skip :amount amount)))))

(define-api studio/upload (id) ()
  ;; FIXME: Check permissions
  (api-output (api-upload (ensure-upload id))))

(define-api studio/upload/list (user &optional tag date skip amount) ()
  ;; FIXME: Check permissions
  (let ((skip (if skip (parse-integer skip) 0))
        (amount (if amount (parse-integer amount))))
    (api-output (mapcar #'api-upload (uploads user :tag tag :date date :skip skip :amount amount)))))

(define-api studio/upload/create (title file[] &optional description tags visibility) ()
  ;; FIXME: Check permissions
  (unless (<= 1 (length title) 64)
    (error "Title must be between 1 and 64 characters long."))
  (let ((upload (make-upload title file[] :description description
                                          :tags (cl-ppcre:split "(\\s*,\\s*)+" tags)
                                          :visibility (->visibility visibility))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (api-upload upload)))))

(define-api studio/upload/edit (upload &optional title description file[] tags visibility) ()
  ;; FIXME: Check permissions
  (let ((upload (update-upload upload
                               :title title
                               :description description
                               :files file[]
                               :tags (cl-ppcre:split "(\\s*,\\s*)+" tags)
                               :visibility (when visibility (->visibility visibility)))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (api-upload upload)))))

(define-api studio/upload/delete (upload) ()
  ;; FIXME: Check permissions
  (let ((upload (ensure-upload upload)))
    (delete-upload upload)
    (if (string= (post/get "browser") "true")
        (redirect (gallery-link (dm:field upload "author")))
        (api-output "OK"))))
