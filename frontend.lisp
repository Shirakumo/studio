#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

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
    (multiple-value-bind (older newer) (page-marks uploads date offset (dm:field gallery "author"))
      (r-clip:process T
                      :description (dm:field gallery "description")
                      :cover (when (dm:field gallery "cover")
                               (ensure-upload (dm:field gallery "cover") NIL))
                      :author user
                      :uploads uploads
                      :date date
                      :next (when older (gallery-link user :date (first older) :offset (second older)))
                      :prev (when newer (gallery-link user :date (first newer) :offset (second newer)))))))

(define-page tag-gallery "studio/^gallery/([^/]+)/tag/(.+?)(?:/([0-9.]+)(?:[ +]([0-9]+))?)?$" (:uri-groups (user tag date offset) :clip "gallery.ctml")
  (let* ((date (parse-date date))
         (offset (maybe-parse-integer offset 0))
         (gallery (ensure-gallery user))
         (uploads (uploads user :tag tag :date date :skip offset)))
    (multiple-value-bind (older newer) (page-marks uploads date offset (user:id user) tag)
      (r-clip:process T
                      :description (dm:field gallery "description")
                      :cover (when (dm:field gallery "cover")
                               (ensure-upload (dm:field gallery "cover") NIL))
                      :author user
                      :tag tag
                      :uploads uploads
                      :date date
                      :next (when older (gallery-link user :tag tag :date (first older) :offset (second older)))
                      :prev (when newer (gallery-link user :tag tag :date (first newer) :offset (second newer)))))))

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
