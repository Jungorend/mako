(defsystem "mako"
  :version "0.0.1"
  :author "Jungy"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-astar
               #:cl-fast-ecs
               #:cl-tiled
               
               #:cl-liballegro
               #:cl-liballegro-nuklear
               
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "common")
                 (:file "wall")
                 (:file "map")
                 (:file "sprites")
                 (:file "enemies")
                 (:file "player")
                 (:file "main"))))
  :description "A simple game."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"mako"
  :entry-point "mako:main")
