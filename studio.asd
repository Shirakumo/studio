#|
 This file is a part of Studio
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:studio
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.0.0"
  :description "An image gallery hosting service"
  :homepage "https://Shirakumo.github.io/studio/"
  :bug-tracker "https://github.com/Shirakumo/studio/issues"
  :source-control (:git "https://github.com/Shirakumo/studio.git")
  :components ((:file "module")
               (:file "toolkit")
               (:file "objects")
               (:file "frontend")
               (:file "api")
               (:file "import")
               (:file "import-tumblr"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               :north-drakma
               :dexador
               :humbler
               :trivial-thumbnail
               :r-data-model
               :r-clip
               :i-json
               :cl-ppcre
               :uiop
               :3bmd))
