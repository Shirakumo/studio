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
                    :licenses (list-licenses)
                    :license (when gallery (dm:field gallery "license"))
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
                      :pins (when (= 0 offset) (pins user))
                      :date date
                      :next (when older (gallery-link user :date (first older) :offset (second older)))
                      :prev (when newer (gallery-link user :date (first newer) :offset (second newer)))))))

(define-page tag-list ("studio/^gallery/([^/]+)/tag/?$" 1) (:uri-groups (user) :clip "tags.ctml")
  (let* ((gallery (ensure-gallery user))
         (tags (tags user :sort (cond ((string-equal (post/get "sort") "count") :count)
                                      (T :name))
                          :direction (cond ((string-equal (post/get "direction") "asc") :asc)
                                           ((string-equal (post/get "direction") "dsc") :dsc)))))
    (r-clip:process T
                    :description (dm:field gallery "description")
                    :cover (when (dm:field gallery "cover")
                             (ensure-upload (dm:field gallery "cover") NIL))
                    :author user
                    :tags tags)))

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

(defun render-description (text)
  (plump:parse
   (with-output-to-string (string)
     (3bmd:parse-string-and-print-to-stream text string))))

(define-page view-image "studio/^view/(.+)" (:uri-groups (id) :clip "view.ctml")
  (let* ((upload (ensure-upload (db:ensure-id id)))
         (gallery (ensure-gallery (dm:field upload "author")))
         (license (when (dm:field upload "license") (ensure-license (dm:field upload "license")))))
    (check-permitted :view upload)
    (r-clip:process T
                    :upload upload
                    :id (dm:id upload)
                    :title  (dm:field upload "title")
                    :visibility (->visibility (dm:field upload "visibility"))
                    :arrangement (->arrangement (dm:field upload "arrangement"))
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (render-description (dm:field upload "description"))
                    :cover-p (equal (dm:id upload) (dm:field gallery "cover"))
                    :pinned-p (= 1 (db:count 'pins (db:query (:= 'upload (dm:id upload)))))
                    :prior (prior-upload upload)
                    :later (later-upload upload)
                    :license license)))

(define-page edit-image "studio/^edit/(.+)" (:uri-groups (id) :clip "upload.ctml")
  (let ((upload (ensure-upload (db:ensure-id id))))
    (check-permitted :edit upload)
    (r-clip:process T
                    :upload upload
                    :id (dm:id upload)
                    :title (dm:field upload "title")
                    :visibility (->visibility (dm:field upload "visibility"))
                    :arrangement (->arrangement (dm:field upload "arrangement"))
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (dm:field upload "description")
                    :licenses (list-licenses)
                    :license (dm:field upload "license"))))

(define-page upload "studio/^upload" (:clip "upload.ctml")
  (let ((gallery (ensure-gallery (auth:current "anonymous"))))
    (check-permitted :create)
    (r-clip:process T
                    :author (user:username (auth:current "anonymous"))
                    :licenses (list-licenses)
                    :license (dm:field gallery "license")
                    :arrangement :top-to-bottom)))

(define-page view-license ("studio/^license/([^/]+)/" -1) (:uri-groups (id) :clip "license-view.ctml")
  (let ((license (ensure-license id)))
    (check-permitted :license)
    (r-clip:process T
                    :id (dm:id license)
                    :name (dm:field license "name")
                    :description (dm:field license "description")
                    :body (dm:field license "body"))))

(define-page new-license "studio/^license/new$" (:clip "license-edit.ctml")
  (check-permitted :license)
  (r-clip:process T))

(define-page edit-license "studio/^license/([^/]+)/edit" (:uri-groups (id) :clip "license-edit.ctml")
  (let ((license (ensure-license id)))
    (check-permitted :license)
    (r-clip:process T
                    :id (dm:id license)
                    :name (dm:field license "name")
                    :description (dm:field license "description")
                    :body (dm:field license "body"))))
