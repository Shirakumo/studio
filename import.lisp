#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(defmacro with-tumblr-oauth ((&key token secret) &body body)
  `(let ((south:*oauth-api-key* (config :tumblr :api-key))
         (south:*oauth-api-secret* (config :tumblr :api-secret))
         (south:*oauth-access-token* ,token)
         (south:*oauth-access-secret* ,secret)
         (humbler:*user* NIL))
     ,@body))

(defun url-temp-file (url)
  (multiple-value-bind (in status headers) (drakma:http-request url :want-stream T)
    (unwind-protect
         (case status
           (200 (let ((file (merge-pathnames
                             (make-pathname :name (format NIL "~d-~d~@[-~a~]" (get-universal-time)
                                                          (random 10000) (cdr (assoc :etag headers)))
                                            :type (trivial-mimes:mime-file-type
                                                   (cdr (assoc :content-type headers)))
                                            :directory '(:relative "studio" "import"))
                             (uiop:temporary-directory))))
                  (ensure-directories-exist file)
                  (with-open-file (out file :direction :output :element-type '(unsigned-byte 8))
                    (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
                          for read = (read-sequence buffer in)
                          while (/= 0 read)
                          do (write-sequence buffer out)))
                  file))
           (T
            (v:warn :studio.import.tumblr "Failed to download ~s for import." url)))
      (close in))))

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

(defun import-tumblr (id blog token secret)
  (with-tumblr-oauth (:token token :secret secret)
    (let ((post (etypecase id
                  (humbler:post id)
                  (integer (humbler:post id blog)))))
      (typecase post
        (humbler:photo-post
         (let ((files (convert-post-photos post)))
           (when files
             (unwind-protect
                  (make-upload (tumblr-post-title post)
                               files
                               :description (plump:text (plump:parse (humbler:caption post)))
                               :tags (humbler:tags post)
                               :time (local-time:timestamp-to-universal (humbler:date post)))
               (dolist (file files)
                 (uiop:delete-file-if-exists (first file)))))))
        (T
         (v:warn :studio.import.tumblr "Ignoring ~a/~d: not a photo post." blog id))))))

(define-page import-overview "studio/^import$" (:clip "import.ctml")
  (check-permitted :import)
  (r-clip:process
   T :author (user:username (auth:current))))

(define-page import-tumblr "studio/^import/tumblr$" (:clip "import-tumblr.ctml")
  (check-permitted :import)
  (with-tumblr-oauth (:token (post/get "token") :secret (post/get "secret"))
    (let ((blog (post/get "blog")))
      (r-clip:process
       T :author (user:username (auth:current))
       :blog blog
       :blogs (humbler:blogs (humbler:myself))
       :posts (when blog
                (humbler:posts blog :type :photo :tag (post/get "tag")))
       :token south:*oauth-access-token*
       :secret south:*oauth-access-secret*))))

(define-api studio/import/tumblr/authorize () ()
  (check-permitted :import)
  (with-tumblr-oauth ()
    (let ((url (south:oauth/authenticate
                (uri-to-url #@"studio/api/studio/import/tumblr/authenticate"
                            :representation :external))))
      (setf (session:field 'import/tumblr-secret) south:*oauth-access-secret*)
      (redirect url))))

(define-api studio/import/tumblr/authenticate (oauth_verifier oauth_token) ()
  (check-permitted :import)
  (with-tumblr-oauth ()
    (setf south:*oauth-access-secret* (session:field 'import/tumblr-secret))
    (south:complete-authentication oauth_verifier oauth_token)
    (redirect (uri-to-url #@"studio/import/tumblr"
                          :representation :external
                          :query `(("token" . ,south:*oauth-access-token*)
                                   ("secret" . ,south:*oauth-access-secret*))))))

(define-api studio/import/tumblr (id[] blog token secret) ()
  (check-permitted :import)
  (let ((uploads (loop for id in id[]
                       for upload = (import-tumblr (parse-integer id) blog token secret)
                       when upload collect upload)))
    (if (string= (post/get "browser") "true")
        (redirect (uri-to-url #@"studio/import/tumblr"
                              :representation :external
                              :query `(("token" . ,south:*oauth-access-token*)
                                       ("secret" . ,south:*oauth-access-secret*)
                                       ("blog" . ,blog)
                                       ("message" . ,(format NIL "~d upload~:p imported."
                                                             (length uploads))))))
        (api-output (mktable :uploads (mapcar #'upload->table uploads))))))

(define-api studio/import/tumblr/all (blog token secret &optional tag) ()
  (check-permitted :import)
  (let* ((counter 0)
         (username (user:username (auth:current)))
         (uploads (humbler:pageinate (lambda (&rest args)
                                       (v:info :studio.import.tumblr.all "Imported ~d post~:p from ~a~@[/~] for ~a."
                                               counter blog tag username)
                                       (loop for data in (apply #'humbler:blog/posts blog args)
                                             for post = (humbler:make-post data)
                                             do (incf counter)
                                             collect (import-tumblr post blog token secret)))
                                     0 T :type :photo :tag tag)))
    (if (string= (post/get "browser") "true")
        (redirect (uri-to-url #@"studio/import/tumblr"
                              :representation :external
                              :query `(("token" . ,south:*oauth-access-token*)
                                       ("secret" . ,south:*oauth-access-secret*)
                                       ("blog" . ,blog)
                                       ("message" . ,(format NIL "~d upload~:p imported." counter)))))
        (api-output (mktable :uploads (mapcar #'upload->table uploads))))))
