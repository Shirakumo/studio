#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-trigger radiance:startup ()
  (defaulted-config :allowed-content-types '("image/png" "image/jpeg" "image/gif" "image/svg+xml")))

(define-trigger db:connected ()
  (db:create 'uploads '((time (:integer 5))
                        (author :integer)
                        (title (:varchar 64))
                        (description :text))
             :indices '(author))
  (db:create 'files '((upload :id)
                      (type (:varchar 32)))
             :indices '(upload))
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

(defun upload-files (upload-ish)
  (let ((id (dm:id (ensure-upload upload-ish))))
    (dm:get 'files (db:query (:= 'upload id)))))

(defun tag-uploads (tag &key (start 0) (end 20))
  (let ((uploads ()))
    (db:iterate 'tags (db:query (:= 'tag tag))
                (lambda (data)
                  (let ((id (gethash "upload" data)))
                    (push (dm:get-one 'uploads (db:query (:= '_id id))) uploads)))
                :fields '(upload) :skip start :amount (- end start))
    (nreverse uploads)))

(defun file-pathname (file)
  (merge-pathnames
   (make-pathname :name (princ-to-string (dm:id file))
                  :type (mimes:mime-file-type (dm:field file "type"))
                  :directory `(:relative "uploads" ,(dm:field file "upload")))
   (mconfig-pathname #.*package*)))

(defun upload-pathname (upload)
  (merge-pathnames
   (make-pathname :directory `(:relative "uploads" ,(dm:id upload)))
   (mconfig-pathname #.*package*)))

(defun %handle-new-files (upload files)
  (let ((id (dm:id upload)))
    (loop for (file mime) in files
          for hull = (dm:hull 'files)
          do (setf (dm:field hull "upload") id)
             (setf (dm:field hull "type") (mimes:mime-lookup (second file)))
             (dm:insert hull)
             (copy-file file (file-pathname hull)))))

(defun %dispose-files (pathnames)
  (dolist (file pathnames)
    (handler-case (delete-file file)
      (error (e)
        (v:debug :radiance.studio e)
        (v:warn :radiance.studio "Failed to delete file ~a." file)))))

(defun make-upload (title files &key description (author (auth:current)) (time (get-universal-time)) tags)
  (db:with-transaction ()
    (let ((upload (dm:hull 'uploads)))
      (setf (dm:field upload "title") title)
      (setf (dm:field upload "description") (or description ""))
      (setf (dm:field upload "author") (user:id author))
      (setf (dm:field upload "time") time)
      (dm:insert upload)
      (let ((id (dm:field upload "_id")))
        ;; FIXME: Clean up files in case of erroneous unwind.
        (ensure-directories-exist (upload-pathname upload))
        (%handle-new-files upload files)
        (dolist (tag tags)
          (db:insert 'tags `(("upload" . ,id) ("tag" . ,tag)))))
      upload)))

(defun update-upload (upload &key title description author time (keep-files NIL change-files) new-files (tags NIL tags-p))
  (let ((to-delete ()))
    (db:with-transaction ()
      (let* ((upload (ensure-upload upload))
             (id (dm:id upload)))
        (when title
          (setf (dm:field upload "title") title))
        (when description
          (setf (dm:field upload "description") description))
        (when author
          (setf (dm:field upload "author") (user:id author)))
        (when time
          (setf (dm:field upload "time") time))
        (dm:save upload)
        (when change-files
          (loop for file in (upload-files upload)
                unless (find (dm:id file) keep-files :test 'equal :key #'db:ensure-id)
                do (push (file-pathname file) to-delete)
                   (dm:delete file)))
        ;; FIXME: Clean up files in case of erroneous unwind.
        (when new-files
          (%handle-new-files upload new-files))
        (when tags-p
          (db:delete 'tags (db:query (:= 'upload id)))
          (dolist (tag tags)
            (db:insert 'tags `(("upload" . ,id) ("tag" . ,tag)))))))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)))

(defun delete-upload (upload)
  (let ((to-delete ()))
    (db:with-transaction ()
      (let* ((upload (ensure-upload upload))
             (id (dm:id upload)))
        (dolist (file (upload-files upload))
          (push (file-pathname file) to-delete))
        (delete-directory (upload-pathname upload))
        (db:delete 'files (db:query (:= 'upload files)))
        (db:delete 'tags (db:qery (:= 'upload id)))
        (dm:delete upload)))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)))
