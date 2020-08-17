(defsystem cram-pepper-demo
  :author "derrick"
  :license "BSD"

  :depends-on (roslisp-utilities ; for ros-init-function

               cl-transforms
               cl-transforms-stamped
               cl-tf
               cl-tf2
               cram-tf

               cram-language
               cram-executive
               cram-designators
               cram-prolog
               cram-projection
               cram-occasions-events
               cram-utilities ; for EQUALIZE-LISTS-OF-LISTS-LENGTHS

               cram-common-failures
               cram-mobile-pick-place-plans
               cram-robot-interfaces ; for *robot-urdf*
               cram-object-knowledge

               cram-physics-utils ; for reading "package://" paths
               cl-bullet ; for handling BOUNDING-BOX datastructures
               cram-bullet-reasoning
               cram-bullet-reasoning-belief-state
               cram-bullet-reasoning-utilities
               cram-btr-visibility-costmap

               cram-robot-pose-gaussian-costmap
               cram-occupancy-grid-costmap
               cram-location-costmap
               cram-manipulation-interfaces ; for standard rotations

               cram-urdf-projection      ; for with-simulated-robot
               cram-pepper-description
               cram-process-modules
               cram-fetch-deliver-plans

               )

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "setup" :depends-on ("package"))
     (:file "projection-demo" :depends-on ("package"))
     (:file "cram-plans" :depends-on ("package"))
     (:file "shelf-prolog" :depends-on ("package"))
     ))))
