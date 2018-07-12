#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.studio)

(define-page gallery "studio/([0-9]+)" (:uri-groups (page) :clip "gallery.ctml")
  )

(define-page tag-gallery "studio/tag/(.+)(?:/([0-9]+))?" (:uri-groups (tag page) :clip "gallery.ctml")
  )

(define-page user-gallery "studio/user/(.+)(?:/([0-9]+))?" (:uri-groups (user page) :clip "gallery.ctml")
  )

(define-page user-tag-gallery "studio/user/(.+)/tag/(.+)(?:/([0-9]+))?" (:uri-groups (user tag page) :clip "gallery.ctml")
  )

(define-page view-image "studio/view/(.+)" (:uri-groups (id) :clip "view.ctml")
  )

(define-page edit-image "studio/edit/(.+)" (:uri-groups (id) :clip "upload.ctml")
  )

(define-page upload "studio/upload" (:clip "upload.ctml")
  )
