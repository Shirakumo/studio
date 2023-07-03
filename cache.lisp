(in-package #:org.shirakumo.radiance.studio)

(defparameter *header* (plump:first-child (plump:parse (@template "header.ctml"))))

(defun cache-page (id)
  (environment-module-pathname
   #.*package* :cache
   (make-pathname :name (princ-to-string (car (last id)))
                  :type "html"
                  :directory `(:relative ,@(mapcar #'princ-to-string (butlast id))))))

(defmacro with-cache ((author &rest id) &body body)
  `(generate-cache ,author (list ,@id)
                   (lambda () ,@body)))

(defun generate-cache (author id generator)
  (with-simple-restart (abort (format NIL "Abort generating the cache for ~s" id))
    (let* ((*admin-user-p* T)
           (result (funcall generator))
           (file (cache-page id)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output
                                :if-exists :supersede)
        (when author (princ (user:id author) out))
        (write-char #\Linefeed out)
        (plump:serialize result out)))))

(defun fetch-cache (id)
  (with-open-file (in (cache-page id) :direction :input
                                      :if-does-not-exist NIL)
    (when in
      (let ((author (read-line in)))
        (values (plump:parse in)
                (when (string/= "" author)
                  (parse-integer author)))))))

(defun render-cache (id &optional (user (auth:current "anonymous")))
  (multiple-value-bind (dom uid) (fetch-cache id)
    (when dom
      (when (has-gallery-p user)
        ;; Replace the header.
        (plump:replace-child
         (aref (clss:select "header:first-only" dom) 0)
         (r-clip:process (plump:clone-node *header* T))))
      (cond ((= uid (user:id user)))
            ((eql :gallery (first id))
             ;; Remove non-public images
             (loop for hidden across (clss:select ".image.hidden,.image.private" dom)
                   do (plump:remove-child hidden)))
            ((eql :view (first id))
             ;; Remove editing buttons
             (loop for hidden across (clss:select "a.editbutton" dom)
                   do (plump:remove-child hidden))))
      (plump:serialize dom NIL))))

(defun delete-cache (model)
  (ecase (dm:collection model)
    (uploads
     (uiop:delete-file-if-exists
      (cache-page (list :view (dm:id model)))))
    (galleries
     (uiop:delete-directory-tree
      (uiop:pathname-directory-pathname
       (cache-page (list :gallery (user:username (dm:field model "author")) NIL)))))))

(defun cache-front ()
  (loop for page from 1 to 100
        for galleries = (galleries :skip (* (config :per-page :galleries) (1- page)))
        while galleries
        do (with-cache (NIL :front page)
             (r-clip:process
              (@template "front.ctml")
              :galleries galleries
              :prev (when (< 1 page)
                      (uri-to-url (radiance:make-uri :domains '("studio")
                                                     :path (princ-to-string (1- page)))
                                  :representation :external))
              :next (uri-to-url (radiance:make-uri :domains '("studio")
                                                   :path (princ-to-string (1+ page)))
                                :representation :external)))))

(defun cache-gallery (user date)
  (let ((user (user:get user)))
    (loop with gallery = (ensure-gallery user)
          with cover = (when (dm:field gallery "cover")
                         (ensure-upload (dm:field gallery "cover") NIL))
          for offset from 0 by (config :per-page :uploads)
          for uploads = (uploads user :date date :skip offset :author-p T)
          while uploads
          do (multiple-value-bind (older newer) (page-marks uploads date offset user)
               (with-cache (user :gallery (user:username user) (format-date date) offset)
                 (r-clip:process
                  (@template "gallery.ctml")
                  :description (dm:field gallery "description")
                  :cover cover
                  :author (user:username user)
                  :uploads uploads
                  :next (when older (gallery-link user :date (first older) :offset (second older)))
                  :prev (when newer (gallery-link user :date (first newer) :offset (second newer)))))))))

(defun cache-tag-gallery (user tag date)
  (let ((user (user:get user)))
    (loop with gallery = (ensure-gallery user)
          with cover = (when (dm:field gallery "cover")
                         (ensure-upload (dm:field gallery "cover") NIL))
          for offset from 0 by (config :per-page :uploads)
          for uploads = (uploads user :date date :skip offset :tag tag :author-p T)
          while uploads
          do (multiple-value-bind (older newer) (page-marks uploads date offset user tag)
               (with-cache (user :gallery (user:username user) (format-date date) tag offset)
                 (r-clip:process
                  (@template "gallery.ctml")
                  :description (dm:field gallery "description")
                  :cover cover
                  :author (user:username user)
                  :uploads uploads
                  :next (when older (gallery-link user :date (first older) :offset (second older)))
                  :prev (when newer (gallery-link user :date (first newer) :offset (second newer)))))))))

(defun cache-view (upload)
  (let ((upload (ensure-upload upload))
        (gallery (ensure-gallery (dm:field upload "author")))
        (user (user:get (dm:field upload "author"))))
    (with-cache (user :view (dm:id upload))
      (r-clip:process
       (@template "view.ctml")
       :upload upload
       :id (dm:id upload)
       :title  (dm:field upload "title")
       :visibility (->visibility (dm:field upload "visibility"))
       :author (user:username (dm:field upload "author"))
       :files (upload-files upload)
       :tags (upload-tags upload)
       :time (dm:field upload "time")
       :description (dm:field upload "description")
       :cover-p (equal (dm:id upload) (dm:field gallery "cover"))))))
