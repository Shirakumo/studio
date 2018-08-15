#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-page file-data ("studio/api/studio/file" 1000) ()
  (handler-case
      (let ((file (ensure-file (db:ensure-id (post/get "id")))))
        (setf (header "Cache-Control") "public, max-age=31536000")
        (serve-file (file-pathname file :thumb (or* (post/get "thumb"))) (dm:field file "type")))
    (error (e)
      (error 'request-not-found :message (princ-to-string e)))))

(defun gallery->table (gallery)
  (mktable :author (mktable :id (princ-to-string (dm:field gallery "author"))
                            :username (user:username (dm:field gallery "author")))
           :url (gallery-link (dm:field gallery "author"))
           :description (dm:field gallery "description")
           :cover (when (dm:field gallery "cover")
                    (princ-to-string (dm:field gallery "cover")))))

(defun upload->table (upload)
  (mktable :id (princ-to-string (dm:id upload))
           :url (upload-link upload)
           :title (dm:field upload "title")
           :author (user:username (dm:field upload "author"))
           :tags (upload-tags upload)
           :time (dm:field upload "time")
           :visibility (string-downcase (->visibility (dm:field upload "visibility")))
           :description (dm:field upload "description")
           :files (mapcar #'dm:id (upload-files upload))))

(define-api studio/gallery (author) ()
  (api-output (gallery->table (ensure-gallery author))))

(define-api studio/gallery/list (&optional skip amount) ()
  (let ((skip (if skip (parse-integer skip) 0))
        (amount (ensure-amount amount (config :per-page :galleries))))
    (api-output (mapcar #'gallery->table (galleries :skip skip :amount amount)))))

(define-api studio/gallery/create (&optional description) ()
  (check-permitted :create-gallery)
  (when (ensure-gallery (auth:current) NIL)
    (error 'api-error :message "This user already has a gallery."))
  (let ((gallery (make-gallery (auth:current) :description description)))
    (if (string= (post/get "browser") "true")
        (redirect (gallery-link (auth:current)))
        (api-output (gallery->table gallery)))))

(define-api studio/gallery/edit (author &optional description cover) ()
  (let ((gallery (ensure-gallery author)))
    (check-permitted :edit-gallery gallery)
    (setf gallery (if cover
                      (update-gallery author
                                      :description description
                                      :cover (when (string/= "" cover) (db:ensure-id cover)))
                      (update-gallery author
                                      :description description)))
    (if (string= (post/get "browser") "true")
        (redirect (gallery-link author))
        (api-output (gallery->table gallery)))))

(define-api studio/gallery/set-cover (upload &optional author) ()
  (let ((gallery (ensure-gallery (or author (auth:current)))))
    (check-permitted :edit-gallery gallery)
    (update-gallery gallery :cover (when (string/= "_" upload) (db:ensure-id upload)))
    (redirect (referer))))

(define-api studio/gallery/delete (author) ()
  (let ((gallery (ensure-gallery author)))
    (check-permitted :delete-gallery gallery)
    (delete-gallery author)
    (if (string= (post/get "browser") "true")
        (redirect #@"studio/")
        (api-output T :message "Gallery deleted."))))

(define-api studio/gallery/atom (user &optional tag) ()
  (let* ((gallery (ensure-gallery user))
         (uploads (uploads (dm:field gallery "author") :tag tag :author-p NIL)))
    (setf (content-type *response*) "application/atom+xml; charset=utf-8")
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'muffle-warning))
      (let ((plump:*tag-dispatchers* plump:*xml-tags*))
        (plump:serialize
         (r-clip:process
          (@template "atom.ctml")
          :author user
          :title (format NIL "~a's Gallery" user)
          :description (dm:field gallery "description")
          :tag tag
          :updated (if uploads (dm:field (first uploads) "time") 0)
          :uploads uploads)
         NIL)))))

(define-api studio/upload (id) ()
  (let ((upload (ensure-upload id)))
    (check-permitted :view upload)
    (api-output (upload->table upload))))

(define-api studio/upload/list (user &optional tag min-date max-date date skip amount) ()
  (let* ((skip (if skip (parse-integer skip) 0))
         (amount (ensure-amount amount (config :per-page :uploads)))
         (date (cond (date
                      (parse-date date))
                     ((and min-date max-date)
                      (list (parse-integer min-date) (parse-integer max-date)))))
         (uploads (uploads user :tag tag :date date :skip skip :amount amount)))
    (multiple-value-bind (older newer) (page-marks uploads date skip (user:id user))
      (api-output (mktable :uploads (map 'vector #'upload->table uploads)
                           :older (when older (mktable :date (first older) :offset (second older)))
                           :newer (when newer (mktable :date (first newer) :offset (second newer))))))))

(define-api studio/upload/create (title file[] &optional description tags visibility) ()
  (check-permitted :create)
  (unless (<= 1 (length title) 64)
    (error "Title must be between 1 and 64 characters long."))
  (let ((upload (make-upload title file[] :description description
                                          :tags (when tags (cl-ppcre:split "(\\s*,\\s*)+" tags))
                                          :visibility (when visibility (->visibility visibility)))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (upload->table upload)))))

(define-api studio/upload/edit (upload &optional title description file[] tags visibility) ()
  (let ((upload (ensure-upload upload)))
    (check-permitted :edit upload)
    (setf upload (if tags
                     (update-upload upload
                                    :title title
                                    :description description
                                    :files file[]
                                    :tags (cl-ppcre:split "(\\s*,\\s*)+" tags)
                                    :visibility (when visibility (->visibility visibility)))
                     (update-upload upload
                                    :title title
                                    :description description
                                    :files file[]
                                    :visibility (when visibility (->visibility visibility)))))
    (if (string= (post/get "browser") "true")
        (redirect (upload-link upload))
        (api-output (upload->table upload)))))

(define-api studio/upload/delete (upload) ()
  (let ((upload (ensure-upload upload)))
    (check-permitted :delete upload)
    (delete-upload upload)
    (if (string= (post/get "browser") "true")
        (redirect (gallery-link (dm:field upload "author")))
        (api-output "OK"))))
