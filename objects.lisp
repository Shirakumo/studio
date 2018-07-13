#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-trigger radiance:startup ()
  (defaulted-config '("image/png" "image/jpeg" "image/gif" "image/svg+xml") :allowed-content-types)
  (defaulted-config (* 4 10) :max-per-page))

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
    (dm:data-model upload-ish)
    (db:id (or (dm:get-one 'uploads (db:query (:= '_id upload-ish)))
               (error "No upload with ID ~s" upload-ish)))))

(defun ensure-file (file-ish)
  (etypecase file-ish
    (dm:data-model file-ish)
    (db:id (or (dm:get-one 'files (db:query (:= '_id file-ish)))
               (error "No file with ID ~s" file-ish)))))

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

(defun file-link (file)
  (let ((file (ensure-file file)))
    (uri-to-url (radiance:make-uri :domains '("studio")
                                   :path (format NIL "api/studio/file"))
                :representation :external
                :query `(("id" . ,(princ-to-string (dm:id file)))))))

(defun upload-link (upload)
  (let ((upload (ensure-upload upload)))
    (uri-to-url (radiance:make-uri :domains '("studio")
                                   :path (format NIL "view/~a" (dm:id upload)))
                :representation :external)))

(defun gallery-link (&key user tag page offset)
  (uri-to-url (radiance:make-uri :domains '("studio")
                                 :path (format NIL "~@[user/~a~]~@[tag/~a~]~@[/~d~@[+~d~]~]"
                                               (when user (user:username user)) tag page offset))
              :representation :external))

(defun date-range (date &optional (page 1))
  (multiple-value-bind (ss mm hh d m y day dl-p tz) (decode-universal-time date 0)
    (declare (ignore ss mm hh d day dl-p))
    (values (multiple-value-bind (y+ m) (floor (- m page) 12)
              (encode-universal-time 0 0 0 1 (1+ m) (+ y y+) tz))
            (multiple-value-bind (y+ m) (floor (- (1+ m) page) 12)
              (encode-universal-time 0 0 0 1 (1+ m) (+ y y+) tz)))))

(defun uploads (&key tag user date (skip 0) (amount (config :max-per-page)))
  (multiple-value-bind (min-date max-date) (when date (apply #'date-range date))
    (cond (tag
           (let ((uploads ()) (count 0))
             ;; KLUDGE: THIS IS SLOW AND STUPID
             (db:iterate 'tags (cond (user
                                      (db:query (:and (:= 'tag tag)
                                                      (:= 'author user))))
                                     (T
                                      (db:query :all)))
                         (lambda (data)
                           (let* ((id (gethash "upload" data))
                                  (upload (dm:get-one 'uploads (if date
                                                                   (db:query (:and (:= '_id id)
                                                                                   (:<= min-date 'time)
                                                                                   (:<  'time max-date)))
                                                                   (db:query (:= '_id id)))
                                                      :sort '((time :asc)))))
                             (when upload
                               (cond ((< count skip))
                                     ((< count (+ amount skip))
                                      (push upload uploads))
                                     (T
                                      (return-from uploads (nreverse uploads))))
                               (incf count))))
                         :fields '(upload) :skip skip :amount amount)))
          (T
           (dm:get 'uploads (cond ((and user date)
                                   (db:query (:and (:= 'author (user:id user))
                                                   (:<= min-date 'time)
                                                   (:<  'time max-date))))
                                  (user
                                   (db:query (:= 'author (user:id user))))
                                  (date
                                   (db:query (:and (:<= min-date 'time)
                                                   (:<  'time max-date))))
                                  (T
                                   (db:query :all)))
                   :skip skip :amount amount :sort '((time :asc)))))))

(defun file-pathname (file)
  (merge-pathnames
   (make-pathname :name (princ-to-string (dm:id file))
                  :type (mimes:mime-file-type (dm:field file "type"))
                  :directory `(:relative "uploads" ,(princ-to-string (dm:field file "upload"))))
   (mconfig-pathname #.*package*)))

(defun upload-pathname (upload)
  (merge-pathnames
   (make-pathname :directory `(:relative "uploads" ,(princ-to-string (dm:id upload))))
   (mconfig-pathname #.*package*)))

(defun %handle-new-files (upload files)
  (let ((id (dm:id upload)))
    (loop for (path file mime) in files
          for hull = (dm:hull 'files)
          do (setf (dm:field hull "upload") id)
             (setf (dm:field hull "type") mime)
             (dm:insert hull)
             (uiop:copy-file path (file-pathname hull)))))

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
      (setf (dm:field upload "author") (user:id (or author "anonymous")))
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
  (let ((to-delete ()) upload)
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
          (db:remove 'tags (db:query (:= 'upload id)))
          (dolist (tag tags)
            (db:insert 'tags `(("upload" . ,id) ("tag" . ,tag)))))))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)
    upload))

(defun delete-upload (upload)
  (let ((to-delete ()))
    (db:with-transaction ()
      (let* ((upload (ensure-upload upload))
             (id (dm:id upload)))
        (dolist (file (upload-files upload))
          (push (file-pathname file) to-delete))
        (delete-directory (upload-pathname upload))
        (db:remove 'files (db:query (:= 'upload files)))
        (db:remove 'tags (db:query (:= 'upload id)))
        (dm:delete upload)))
    ;; Do this late so we only delete files on successful TX commit.
    (%dispose-files to-delete)))
