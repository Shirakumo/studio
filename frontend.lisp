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

;; FIXME: What about empty months?

(defun prev-link (user date offset &optional tag)
  (cond ((< 0 offset)
         (gallery-link user :tag tag
                            :date (first date)
                            :offset (max 0 (- offset (config :per-page :uploads)))))
        ((< (second date) (get-universal-time))
         (gallery-link user :tag tag
                            :date (second date)))))

(defun next-link (user uploads date offset &optional tag)
  (if (< (length uploads) (config :per-page :uploads))
      (gallery-link user :tag tag :date (adjust-date (first date) -1))
      (gallery-link user :tag tag :date (first date) :offset (+ offset (config :per-page :uploads)))))

(define-page front "studio/^([0-9]+)?$" (:uri-groups (page) :clip "front.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (galleries (galleries :skip (* (config :per-page :galleries) (1- page)))))
    (r-clip:process T
                    :galleries galleries
                    :prev (when (< 1 page)
                            (uri-to-url (radiance:make-uri :domains '("studio")
                                                           :path (princ-to-string (1- page)))
                                        :representation :external))
                    :next (uri-to-url (radiance:make-uri :domains '("studio")
                                                         :path (princ-to-string (1+ page)))
                                      :representation :external))))

(define-page settings "studio/settings" (:clip "settings.ctml")
  (let ((gallery (ensure-gallery (auth:current) NIL)))
    (check-permitted :edit-gallery gallery)
    (r-clip:process T
                    :author (user:username (auth:current))
                    :description (when gallery (dm:field gallery "description"))
                    :exists gallery)))

(define-page gallery "studio/^gallery/([^/]+)(?:/([0-9.]+)(?:[+ ]([0-9]+))?)?" (:uri-groups (user date offset) :clip "gallery.ctml")
  (let* ((date (parse-date date))
         (offset (maybe-parse-integer offset 0))
         (gallery (ensure-gallery user))
         (uploads (uploads user :date date :skip offset)))
    (r-clip:process T
                    :description (dm:field gallery "description")
                    :cover (when (dm:field gallery "cover")
                             (ensure-upload (dm:field gallery "cover") NIL))
                    :author user
                    :uploads uploads
                    :prev (prev-link user date offset)
                    :next (next-link user uploads date offset))))

(define-page tag-gallery "studio/^gallery/([^/]+)/tag/(.+?)(?:/([0-9.]+)(?:[ +]([0-9]+))?)?$" (:uri-groups (user tag date offset) :clip "gallery.ctml")
  (let* ((date (parse-date date))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads user :tag tag :date date :skip offset)))
    (r-clip:process T
                    :author user
                    :tag tag
                    :uploads uploads
                    :prev (prev-link user date offset tag)
                    :next (next-link user uploads date offset tag))))

(define-page view-image "studio/^view/(.+)" (:uri-groups (id) :clip "view.ctml")
  (let* ((upload (ensure-upload (db:ensure-id id)))
         (gallery (ensure-gallery (dm:field upload "author"))))
    (check-permitted :view upload)
    (r-clip:process T
                    :upload upload
                    :id (dm:id upload)
                    :title  (dm:field upload "title")
                    :visibility (->visibility (dm:field upload "visibility"))
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (dm:field upload "description")
                    :cover-p (equal (dm:id upload) (dm:field gallery "cover")))))

(define-page edit-image "studio/^edit/(.+)" (:uri-groups (id) :clip "upload.ctml")
  (let ((upload (ensure-upload (db:ensure-id id))))
    (check-permitted :edit upload)
    (r-clip:process T
                    :upload upload
                    :id (dm:id upload)
                    :title (dm:field upload "title")
                    :visibility (->visibility (dm:field upload "visibility"))
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (dm:field upload "description"))))

(define-page upload "studio/^upload" (:clip "upload.ctml")
  (check-permitted :create)
  (r-clip:process T
                  :author (user:username (auth:current "anonymous"))))
