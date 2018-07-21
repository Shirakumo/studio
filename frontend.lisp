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

(defun prev-link (user page offset &optional tag)
  (when (or (< 1 page) (< 0 offset))
    (gallery-link user :tag tag
                       :page (if (< 0 offset) page (1- page))
                       :offset (max 0 (- offset (config :max-per-page))))))

(defun next-link (user uploads page offset &optional tag)
  (if (< (length uploads) (config :max-per-page))
      (gallery-link user :tag tag :page (1+ page))
      (gallery-link user :tag tag :page page :offset (+ offset (config :max-per-page)))))

(define-page front "studio/^([0-9]+)?$" (:uri-groups (page) :clip "front.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (galleries (galleries :skip (* (config :max-per-page) (1- page))
                               :amount (config :max-per-page))))
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
    (r-clip:process T
                    :author (user:username (auth:current))
                    :description (when gallery (dm:field gallery "description"))
                    :exists gallery)))

(define-page gallery "studio/^gallery/([^/]+)(?:/([0-9+]+))?(?:\\+([0-9]+))?" (:uri-groups (user page offset) :clip "gallery.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads user :date (list (get-universal-time) page))))
    (r-clip:process T
                    :author user
                    :uploads uploads
                    :prev (prev-link user page offset)
                    :next (next-link user uploads page offset))))

(define-page tag-gallery "studio/^gallery/([^/]+)/tag/(.+?)(?:/([0-9+]+))?(?:\\+([0-9]+))?$" (:uri-groups (user tag page offset) :clip "gallery.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads user :tag tag :date (list (get-universal-time) page))))
    (r-clip:process T
                    :author user
                    :tag tag
                    :uploads uploads
                    :prev (prev-link user page offset tag)
                    :next (next-link user uploads page offset tag))))

(define-page view-image "studio/^view/(.+)" (:uri-groups (id) :clip "view.ctml")
  (let ((upload (ensure-upload (db:ensure-id id))))
    (r-clip:process T
                    :upload upload
                    :id (dm:id upload)
                    :title  (dm:field upload "title")
                    :visibility (->visibility (dm:field upload "visibility"))
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (dm:field upload "description"))))

(define-page edit-image "studio/^edit/(.+)" (:uri-groups (id) :clip "upload.ctml")
  (let ((upload (ensure-upload (db:ensure-id id))))
    ;; FIXME: Check permissions
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
  ;; FIXME: Check permissions
  (r-clip:process T
                  :author (user:username (auth:current "anonymous"))))
