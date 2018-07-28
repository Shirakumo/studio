#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(defmacro with-tumblr-oauth ((&key client token secret callback) &body body)
  `(let ((humbler:*client* ,(or client
                                `(make-instance 'humbler:client
                                                :key (config :tumblr :api-key)
                                                :secret (config :tumblr :api-secret)
                                                :callback ,callback
                                                :token ,token
                                                :token-secret ,secret)))
         (humbler:*user* NIL))
     ,@body))

(defclass tumblr-import-job (import-job)
  ((ids :initarg :ids :accessor ids)
   (blog :initarg :blog :accessor blog)
   (tag :initarg :tag :accessor tag)
   (client :initarg :client :accessor client))
  (:default-initargs
   :ids (error "IDS required")
   :blog (error "BLOG required.")
   :tag NIL))

(defmethod run-import ((job tumblr-import-job))
  (with-tumblr-oauth (:client (client job))
    (let ((counter 0)
          (already-seen (make-hash-table :test 'equal)))
      (flet ((process-post (post)
               (with-simple-restart (ignore-post "Ignore ~a" post)
                 (unless (gethash (humbler:id post) already-seen)
                   (setf (gethash (humbler:id post) already-seen) T)
                   (let ((upload (import-tumblr-post post (user job))))
                     (when upload
                       (incf counter)
                       (when (= 0 (mod counter 100))
                         (l:info :studio.import.tumblr.all "Imported ~d post~:p from ~a~@[/~] for ~a."
                                 counter (blog job) (tag job) (user job)))
                       (push upload (results job))))))))
        (if (eql T (ids job))
            (humbler:pageinate (lambda (&rest args)
                                 (loop for data in (apply #'humbler:blog/posts (blog job) args)
                                       for post = (humbler:make-post data)
                                       do (process-post post))
                                 (list T))
                               0 T :type :photo :tag (tag job))
            (loop for id in (ids job)
                  for post = (humbler:post id (blog job))
                  do (process-post post)))))))

(defun url-temp-file (url)
  (handler-case
      (multiple-value-bind (in status headers) (dexador:get url :want-stream T)
        (declare (ignore status))
        (unwind-protect
             (let ((file (merge-pathnames
                          (make-pathname :name (format NIL "~d-~d~@[-~a~]" (get-universal-time)
                                                       (random 10000) (gethash "Etag" headers))
                                         :type (trivial-mimes:mime-file-type
                                                (gethash "Content-Type" headers))
                                         :directory '(:relative "studio" "import"))
                          (uiop:temporary-directory))))
               (ensure-directories-exist file)
               (with-open-file (out file :direction :output :element-type '(unsigned-byte 8))
                 (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
                       for read = (read-sequence buffer in)
                       while (/= 0 read)
                       do (write-sequence buffer out)))
               file)
          (close in)))
    (dexador:http-request-failed (e)
      (l:warn :studio.import.tumblr "Failed to download ~s for import: ~a" url e))))

(defun convert-post-photos (post)
  (loop for photo in (humbler:photos post)
        for url = (humbler:url (first (humbler:sizes photo)))
        for file = (url-temp-file url)
        when file
        collect (list file
                      (subseq url (1+ (position #\/ url :from-end T)))
                      (trivial-mimes:mime file))))

(defun tumblr-post-title (post)
  (local-time:format-timestring
   NIL (humbler:date post)
   :format '((:year 4) "." (:month 2) "." (:day 2))))

(defun import-tumblr-post (post author)
  (typecase post
    (humbler:photo-post
     (let ((files (convert-post-photos post)))
       (when files
         (unwind-protect
              (make-upload (tumblr-post-title post)
                           files
                           :author author
                           :description (plump:text (plump:parse (humbler:caption post)))
                           :tags (humbler:tags post)
                           :time (local-time:timestamp-to-universal (humbler:date post)))
           (dolist (file files)
             (uiop:delete-file-if-exists (first file)))))))
    (T
     (l:warn :studio.import.tumblr "Ignoring ~a: not a photo post." post))))

(define-page import-tumblr "studio/^import/tumblr$" (:clip "import-tumblr.ctml")
  (check-permitted :import)
  (with-tumblr-oauth (:token (post/get "token") :secret (post/get "secret"))
    (let ((blog (post/get "blog")))
      (r-clip:process
       T :author (user:username (auth:current))
         :job (import-job (auth:current))
         :blog blog
         :blogs (humbler:blogs (humbler:myself))
         :posts (when blog
                  (humbler:posts blog :type :photo
                                      :tag (post/get "tag")
                                      :offset (maybe-parse-integer (post/get "offset") 0)))
         :token (post/get "token")
         :secret (post/get "secret")))))

(define-api studio/import/tumblr/authorize () ()
  (check-permitted :import)
  (with-tumblr-oauth (:callback (uri-to-url #@"studio/api/studio/import/tumblr/authenticate"
                                            :representation :external))
    (let ((url (north:initiate-authentication humbler:*client*)))
      (setf (session:field 'import/tumblr-secret) (north:token-secret humbler:*client*))
      (redirect url))))

(define-api studio/import/tumblr/authenticate (oauth_verifier oauth_token) ()
  (check-permitted :import)
  (with-tumblr-oauth (:secret (session:field 'import/tumblr-secret))
    (north:complete-authentication humbler:*client* oauth_verifier oauth_token)
    (redirect (uri-to-url #@"studio/import/tumblr"
                          :representation :external
                          :query `(("token" . ,(north:token humbler:*client*))
                                   ("secret" . ,(north:token-secret humbler:*client*)))))))

(defun %tumblr-import-api (ids blog token secret tag)
  (check-permitted :import)
  (with-tumblr-oauth (:token token :secret secret)
    (start-import (make-instance 'tumblr-import-job :ids (etypecase ids
                                                           (cons (mapcar #'parse-integer ids))
                                                           ((eql T) T))
                                                    :blog blog
                                                    :tag tag
                                                    :client humbler:*client*
                                                    :user (auth:current))))
  (if (string= (post/get "browser") "true")
      (redirect (uri-to-url #@"studio/import/tumblr"
                            :representation :external
                            :query `(("token" . ,token)
                                     ("secret" . ,secret)
                                     ("blog" . ,blog)
                                     ("message" . "Import started."))))
      (api-output T :message "Import started.")))

(define-api studio/import/tumblr (id[] blog token secret) ()
  (%tumblr-import-api id[] blog token secret NIL))

(define-api studio/import/tumblr/all (blog token secret &optional tag) ()
  (%tumblr-import-api T blog token secret tag))
