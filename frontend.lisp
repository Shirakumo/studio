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

(defun next-link (uploads page offset &key tag user)
  (if (< (length uploads) (config :max-per-page))
      (gallery-link :tag tag :user user :page (1+ page))
      (gallery-link :tag tag :user user :page page :offset (+ offset (config :max-per-page)))))

(define-page gallery "studio/^([0-9]+)?(?:\\+([0-9]+))?$" (:uri-groups (page offset) :clip "front.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads :date (list (get-universal-time) page))))
    (r-clip:process T
                    :uploads uploads
                    :next (next-link uploads page offset))))

(define-page tag-gallery "studio/^tag/(.+)(?:/([0-9+]+))?(?:\\+([0-9]+))?" (:uri-groups (tag page offset) :clip "front.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads :tag tag :date (list (get-universal-time) page))))
    (r-clip:process T
                    :tag tag
                    :uploads uploads
                    :next (next-link uploads page offset :tag tag))))

(define-page user-gallery "studio/^user/(.+)(?:/([0-9+]+))?(?:\\+([0-9]+))?" (:uri-groups (user page offset) :clip "user.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads :user user :date (list (get-universal-time) page))))
    (dolist (upload uploads)
      (setf (dm:field upload "file") (gethash "id"
                                              (first (db:select 'files (db:query (:= 'upload (dm:id upload)))
                                                                :amount 1 :fields '("_id"))))))
    (r-clip:process T
                    :author user
                    :uploads uploads
                    :next (next-link uploads page offset :user user))))

(define-page user-tag-gallery "studio/^user/(.+)/tag/(.+)(?:/([0-9+]+))?(?:\\+([0-9]+))?" (:uri-groups (user tag page offset) :clip "user.ctml")
  (let* ((page (maybe-parse-integer page 1))
         (offset (maybe-parse-integer offset 0))
         (uploads (uploads :user user :tag tag :date (list (get-universal-time) page))))
    (r-clip:process T
                    :author user
                    :tag tag
                    :uploads uploads
                    :next (next-link uploads page offset :user user :tag tag))))

(define-page view-image "studio/^view/(.+)" (:uri-groups (id) :clip "view.ctml")
  (let ((upload (ensure-upload (db:ensure-id id))))
    (r-clip:process T
                    :title (dm:field upload "title")
                    :author (user:username (dm:field upload "author"))
                    :files (upload-files upload)
                    :tags (upload-tags upload)
                    :time (dm:field upload "time")
                    :description (dm:field upload "description"))))

(define-page edit-image "studio/^edit/(.+)" (:uri-groups (id) :clip "upload.ctml")
  ;; FIXME: Check permissions
  (r-clip:process T
                  :upload (ensure-upload (db:ensure-id id))))

(define-page upload "studio/upload" (:clip "upload.ctml")
  ;; FIXME: Check permissions
  (r-clip:process T))
