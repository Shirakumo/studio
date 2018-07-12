#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-trigger db:connected ()
  (db:create 'uploads '((time (:integer 5))
                        (month (:integer 1))
                        (author :integer)
                        (title (:varchar 64))
                        (files (:integer 1))
                        (description :text))
             :indices '(author))
  (db:create 'tags '((upload :id)
                     (tag (:varchar 32)))
             :indices '(upload tag)))

(defun ensure-upload (upload-ish)
  (etypecase upload-ish
    (dm:model upload-ish)
    (db:id (dm:get-one 'uploads (db:query (:= '_id upload-ish))))))

(defun upload-tags (upload-ish)
  (let ((id (dm:id (ensure-upload upload-ish)))
        (tags ()))
    (db:iterate 'tags (db:query (:= 'upload id))
                (lambda (data) (push (gethash "tag" data) tags))
                :fields '(tag))
    (nreverse tags)))

(defun tag-uploads (tag &key (start 0) (end 20))
  (let ((uploads ()))
    (db:iterate 'tags (db:query (:= 'tag tag))
                (lambda (data)
                  (let ((id (gethash "upload" data)))
                    (push (dm:get-one 'uploads (db:query (:= '_id id))) uploads)))
                :fields '(upload) :skip start :amount (- end start))
    (nreverse uploads)))
