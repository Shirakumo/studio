#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-trigger radiance:startup ()
  (defaulted-config '("image/png" "image/jpeg" "image/gif" "image/svg+xml") :allowed-content-types)
  (defaulted-config (* 4 10) :per-page :uploads)
  (defaulted-config 10 :per-page :galleries)
  (defaulted-config 4 :frontpage-uploads)
  (defaulted-config 400 :thumbnail-size)
  (defaulted-config (list (perm studio gallery create)
                          (perm studio gallery edit own)
                          (perm studio gallery delete own)
                          (perm studio upload import)
                          (perm studio upload create)
                          (perm studio upload edit own)
                          (perm studio upload delete own))
                    :permissions :default)
  (apply #'user:add-default-permissions (config :permissions :default)))

(define-trigger db:connected ()
  (db:create 'galleries '((author :integer)
                          (last-update (:integer 5))
                          (cover :id)
                          (description :text))
             :indices '(author))
  (db:create 'uploads '((time (:integer 5))
                        (author :integer)
                        (title (:varchar 64))
                        (visibility (:integer 1))
                        (description :text))
             :indices '(author time))
  (db:create 'files '((upload :id)
                      (order (:integer 1))
                      (type (:varchar 32)))
             :indices '(upload))
  (db:create 'tags '((upload :id)
                     (tag (:varchar 32)))
             :indices '(upload tag)))

(defun ensure-gallery (gallery-ish &optional (errorp T))
  (etypecase gallery-ish
    (dm:data-model gallery-ish)
    (user:user (ensure-gallery (user:id gallery-ish) errorp))
    (string (ensure-gallery (user:id gallery-ish) errorp))
    (integer (or (dm:get-one 'galleries (db:query (:= 'author gallery-ish)))
                 (when errorp (error "No gallery with Author ~s" gallery-ish))))))

(defun ensure-upload (upload-ish &optional (errorp T))
  (etypecase upload-ish
    (dm:data-model upload-ish)
    (db:id (or (dm:get-one 'uploads (db:query (:= '_id upload-ish)))
               (when errorp (error "No upload with ID ~s" upload-ish))))
    (string (ensure-upload (db:ensure-id upload-ish) errorp))))

(defun ensure-file (file-ish &optional (errorp T))
  (etypecase file-ish
    (dm:data-model file-ish)
    (db:id (or (dm:get-one 'files (db:query (:= '_id file-ish)))
               (when errorp (error "No file with ID ~s" file-ish))))
    (string (ensure-file (db:ensure-id file-ish) errorp))))

(defun gallery-uploads (gallery-ish)
  (let* ((gallery (ensure-gallery gallery-ish))
         (cover (when (dm:field gallery "cover")
                  (dm:get-one 'uploads (db:query (:= '_id (dm:field gallery "cover"))))))
         (query (if cover
                    (db:query (:and (:= 'author (dm:field gallery "author"))
                                    (:!= '_id (dm:field gallery "cover"))
                                    (:= 'visibility (visibility->int :public))))
                    (db:query (:and (:= 'author (dm:field gallery "author"))
                                    (:= 'visibility (visibility->int :public))))))
         (count (config :frontpage-uploads)))
    (if cover
        (list* cover (dm:get 'uploads query :amount (1- count) :sort '((time :desc))))
        (dm:get 'uploads query :amount count :sort '((time :desc))))))

(defun upload-tags (upload-ish)
  (let ((id (dm:id (ensure-upload upload-ish)))
        (tags ()))
    (db:iterate 'tags (db:query (:= 'upload id))
                (lambda (data) (push (gethash "tag" data) tags))
                :fields '(tag))
    (nreverse tags)))

(defun upload-files (upload-ish)
  (let ((id (dm:id (ensure-upload upload-ish))))
    (dm:get 'files (db:query (:= 'upload id))
            :sort '(("order" :asc)))))

(defun file-link (file &key thumb)
  (let ((file (ensure-file file)))
    (uri-to-url (radiance:make-uri :domains '("studio")
                                   :path (format NIL "api/studio/file"))
                :representation :external
                :query (list* (cons "id" (princ-to-string (dm:id file)))
                              (when thumb '(("thumb" . "true")))))))

(defun upload-link (upload)
  (let ((upload (ensure-upload upload)))
    (uri-to-url (radiance:make-uri :domains '("studio")
                                   :path (format NIL "view/~a" (dm:id upload)))
                :representation :external)))

(defun gallery-link (user &key tag date offset)
  (let ((offset (when (and offset (< 0 offset)) offset)))
    (uri-to-url (radiance:make-uri :domains '("studio")
                                   :path (format NIL "gallery/~a~@[tag/~a~]~@[/~d~@[+~d~]~]"
                                                 (user:username user)
                                                 tag
                                                 (typecase date
                                                   (null NIL)
                                                   (integer (format-date date))
                                                   (string date))
                                                 offset))
                :representation :external)))

(defun galleries (&key (skip 0) (amount (config :per-page :galleries)))
  (dm:get 'galleries (db:query :all) :skip skip :amount amount :sort '((last-update :desc))))

(defun page-marks (uploads date offset author)
  (let* ((oldest (if uploads (dm:field (car (last uploads)) "time") (first date)))
         (latest (if uploads (dm:field (first uploads) "time") (second date)))
         (older (first (db:select 'uploads (db:query (:and (:< 'time oldest)
                                                           (:= 'author author)))
                                  :amount 1 :fields '("time") :sort '((time :desc)))))
         (newer (first (db:select 'uploads (db:query (:and (:< latest 'time)
                                                           (:= 'author author)))
                                  :amount 1 :fields '("time") :sort '((time :asc))))))
    (values (when older
              (list (format-date (gethash "time" older))
                    (if (< (first date) (gethash "time" older))
                        (+ offset (config :per-page :uploads)) 0)))
            (cond (newer
                   (list (format-date (gethash "time" newer))
                         (if (< (second date) (gethash "time" newer))
                             0 (- offset (config :per-page :uploads)))))
                  ((< 0 offset)
                   (list (format-date (first date))
                         0))))))

(defun uploads (user &key tag date (skip 0) (amount (config :per-page :uploads)) (author-p (user:= user (auth:current "anonymous"))))
  (let ((min-date (first date))
        (max-date (second date)))
    (cond (tag
           (let ((uploads ()) (count 0) (uid (user:id user)))
             ;; KLUDGE: THIS IS SLOW AND STUPID
             (loop for i from 0 by amount
                   for found = NIL
                   do (db:iterate 'tags (db:query (:and (:= 'tag tag)))
                                  (lambda (data)
                                    (setf found T)
                                    (let* ((id (gethash "upload" data))
                                           (upload (dm:get-one 'uploads (if date
                                                                            (db:query (:and (:= '_id id)
                                                                                            (:= 'author uid)
                                                                                            (:<= min-date 'time)
                                                                                            (:<  'time max-date)))
                                                                            (db:query (:and (:= '_id id)
                                                                                            (:= 'author uid)))))))
                                      (when upload
                                        (cond ((< count skip))
                                              ((< count (+ amount skip))
                                               (push upload uploads)))
                                        (incf count))))
                                  :fields '(upload) :skip i :amount amount)
                      (unless found
                        (return-from uploads
                          (sort uploads #'> :key (lambda (a) (dm:field a "time"))))))))
          (T
           (dm:get 'uploads (cond ((and date author-p)
                                   (db:query (:and (:= 'author (user:id user))
                                                   (:<= min-date 'time)
                                                   (:<  'time max-date))))
                                  (date
                                   (db:query (:and (:= 'author (user:id user))
                                                   (:= 'visibility (visibility->int :public))
                                                   (:<= min-date 'time)
                                                   (:<  'time max-date))))
                                  (author-p
                                   (db:query (:= 'author (user:id user))))
                                  (T
                                   (db:query (:and (:= 'author (user:id user))
                                                   (:= 'visibility (visibility->int :public))))))
                   :skip skip :amount amount :sort '((time :desc)))))))

(defun file-pathname (file &key thumb)
  (merge-pathnames
   (make-pathname :name (format NIL "~a~:[~;-thumb~]" (dm:id file) thumb)
                  :type (mimes:mime-file-type (dm:field file "type"))
                  :directory `(:relative "uploads" ,(princ-to-string (dm:field file "upload"))))
   (mconfig-pathname #.*package*)))

(defun upload-pathname (upload)
  (let ((config (mconfig-pathname #.*package*)))
    (make-pathname :name NIL :type NIL
                   :directory `(:absolute ,@(rest (pathname-directory config))
                                          "uploads" ,(princ-to-string (dm:id upload)))
                   :defaults config)))

(defun %handle-new-files (upload files)
  (let ((id (dm:id upload)))
    (loop for i from 0
          for (path file mime order) in files
          for hull = (dm:hull 'files)
          do (setf (dm:field hull "upload") id)
             (setf (dm:field hull "order") (or order i))
             (setf (dm:field hull "type") mime)
             (dm:insert hull)
             (let ((file (file-pathname hull)))
               (uiop:copy-file path file)
               (trivial-thumbnail:create file (file-pathname hull :thumb T)
                                         :width (config :thumbnail-size)
                                         :crop :cover)))))

(defun %dispose-files (pathnames)
  (dolist (file pathnames)
    (handler-case (if (uiop:directory-pathname-p file)
                      (uiop:delete-empty-directory file)
                      (delete-file file))
      (error (e)
        (l:debug :radiance.studio e)
        (l:warn :radiance.studio "Failed to delete file ~a" file)))))

(defun make-gallery (author &key description cover)
  (db:with-transaction ()
    (let ((gallery (dm:hull 'galleries)))
      (setf (dm:field gallery "author") (user:id author))
      (setf (dm:field gallery "description") (or description ""))
      (setf (dm:field gallery "last-update") 0)
      (setf (dm:field gallery "cover") cover)
      (dm:insert gallery))))

(defun update-gallery (gallery &key author description (cover NIL cover-p) last-update)
  (db:with-transaction ()
    (let ((gallery (ensure-gallery gallery)))
      (when author
        (setf (dm:field gallery "author") (user:id author)))
      (when description
        (setf (dm:field gallery "description") description))
      (when cover-p
        (setf (dm:field gallery "cover") cover))
      (when last-update
        (setf (dm:field gallery "last-update") (user:id last-update)))
      (dm:save gallery))))

(defun delete-gallery (gallery)
  (db:with-transaction ()
    (let* ((gallery (ensure-gallery gallery))
           (job (import-job (dm:field gallery "author"))))
      (when job (stop-import job))
      (db:iterate 'uploads (db:query (:= 'author (dm:field gallery "author")))
                  (lambda (row)
                    (delete-upload (make-instance 'dm:data-model :collection 'uploads :field-table row :inserted T))))
      (dm:delete gallery))))

(defun make-upload (title files &key description (author (auth:current)) (time (get-universal-time)) tags (visibility :public))
  (db:with-transaction ()
    (let ((upload (dm:hull 'uploads)))
      (setf (dm:field upload "title") title)
      (setf (dm:field upload "description") (or description ""))
      (setf (dm:field upload "author") (user:id author))
      (setf (dm:field upload "time") time)
      (setf (dm:field upload "visibility") (visibility->int visibility))
      (dm:insert upload)
      (let ((id (dm:field upload "_id")))
        ;; FIXME: Clean up files in case of erroneous unwind.
        (ensure-directories-exist (upload-pathname upload))
        (%handle-new-files upload files)
        (dolist (tag tags)
          (db:insert 'tags `(("upload" . ,id) ("tag" . ,tag)))))
      (update-gallery (user:id author) :last-update (get-universal-time))
      upload)))

(defun update-upload (upload &key title description author time (files NIL files-p) (tags NIL tags-p) visibility)
  (let ((to-delete ()))
    (db:with-transaction ()
      (setf upload (ensure-upload upload))
      (let ((id (dm:id upload)))
        (when title
          (setf (dm:field upload "title") title))
        (when description
          (setf (dm:field upload "description") description))
        (when author
          (setf (dm:field upload "author") (user:id author)))
        (when time
          (setf (dm:field upload "time") time))
        (when visibility
          (setf (dm:field upload "visibility") (visibility->int visibility)))
        (dm:save upload)
        (when tags-p
          (db:remove 'tags (db:query (:= 'upload id)))
          (dolist (tag tags)
            (db:insert 'tags `(("upload" . ,id) ("tag" . ,tag)))))
        (when files-p
          (let ((to-upload ()) (to-keep ()))
            ;; Determine order numbers and update existing files.
            (loop for i from 0
                  for file in files
                  do (typecase file
                       (list
                        (push (append file (list i)) to-upload))
                       (T
                        (let ((file (ensure-file file)))
                          (push (dm:id file) to-keep)
                          (when (/= i (dm:field file "order"))
                            (setf (dm:field file "order") i)
                            (dm:save file))))))
            ;; Delete missing files from DB and FS.
            (loop for file in (upload-files upload)
                  do (unless (find (dm:id file) to-keep)
                       (push (file-pathname file) to-delete)
                       (push (file-pathname file :thumb T) to-delete)
                       (dm:delete file)))
            ;; FIXME: Clean up files in case of failed commit.
            (%handle-new-files upload to-upload)))))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)
    upload))

(defun delete-upload (upload)
  (let ((to-delete ()))
    (db:with-transaction ()
      (let* ((upload (ensure-upload upload))
             (id (dm:id upload)))
        (dolist (file (upload-files upload))
          (push (file-pathname file) to-delete)
          (push (file-pathname file :thumb T) to-delete))
        (push (upload-pathname upload) to-delete)
        (db:remove 'files (db:query (:= 'upload id)))
        (db:remove 'tags (db:query (:= 'upload id)))
        (dm:delete upload)))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)))

(defun has-gallery-p (&optional (user (auth:current)))
  (when user
    (< 0 (db:count 'galleries (db:query (:= 'author (user:id user)))))))

(defun permitted-p (perm &optional object (user (auth:current)))
  (when user
    (ecase perm
      (:import (user:check user (perm studio upload import)))
      (:create (user:check user (perm studio upload create)))
      (:view (or (= (user:id user) (dm:field object "author"))
                 (/= (dm:field object "visibility") (visibility->int :private))))
      (:edit   (or (and (= (user:id user) (dm:field object "author"))
                        (user:check user (perm studio upload edit own)))
                   (user:check user (perm studio upload edit))))
      (:delete (or (and (= (user:id user) (dm:field object "author"))
                        (user:check user (perm studio upload delete own)))
                   (user:check user (perm studio upload delete))))
      (:create-gallery (user:check user (perm studio gallery create)))
      (:edit-gallery (or (and (if object (= (user:id user) (dm:field object "author")) T)
                              (user:check user (perm studio gallery edit own)))
                         (user:check user (perm studio gallery edit))))
      (:delete-gallery (or (and (if object (= (user:id user) (dm:field object "author")) T)
                                (user:check user (perm studio gallery delete own)))
                           (user:check user (perm studio gallery delete)))))))

(defun check-permitted (perm &optional object (user (auth:current)))
  (unless (permitted-p perm object user)
    (error 'request-denied :message (format NIL "You are not allowed to ~(~a~)~@[ this ~a~]"
                                            perm (when object (collection->name (dm:collection object)))))))
