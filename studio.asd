(asdf:defsystem #:studio
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.2.0"
  :description "An image gallery hosting service"
  :homepage "https://shirakumo.org/docs/studio/"
  :bug-tracker "https://shirakumo.org/project/studio/issues"
  :source-control (:git "https://shirakumo.org/project/studio.git")
  :components ((:file "module")
               (:file "toolkit")
               (:file "objects")
               (:file "frontend")
               (:file "api")
               (:file "import")
               (:file "import-tumblr"))
  :depends-on ((:interface :relational-database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               :north-drakma
               :dexador
               :humbler
               :trivial-mimes
               :trivial-thumbnail
               :r-data-model
               :r-clip
               :i-json
               :cl-ppcre
               :uiop
               :3bmd))
