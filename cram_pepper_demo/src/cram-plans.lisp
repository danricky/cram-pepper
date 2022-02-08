(in-package :pepper-demo)

;; Global variables
(defparameter *look-center* nil)
(defparameter *look-right* nil)
(defparameter *look-left* nil)

(defparameter *shelf-one-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector 0.0 -0.33 0.05) (cl-tf:make-quaternion 0 0 0 1)))

(defparameter *shelf-two-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector 0.0 -0.25 0.1) (cl-tf:make-quaternion 0 0 0 1)))

(defparameter *right-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector 0.3 0.0 0.05) (cl-tf:make-quaternion 0 0 0 1)))
 
(defparameter *left-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector -0.3 0.0 0.05) (cl-tf:make-quaternion 0 0 0 1)))

(defparameter *robot-pose-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector -0.3 0.0 0.0) (cl-tf:make-quaternion 0 0 0 1)))

(defparameter *human-pose-offset* 
	(cl-tf:make-transform (cl-tf:make-3d-vector -0.8 0.0 1.0) (cl-tf:make-quaternion 0 0 1 1)))
;; Global variables end

(defun spawn-products-one ()
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal1 ((4.1 0 1.03) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal2 ((2 -1.4 1.05) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal3 ((4.1 0.15 1.73) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet1 ((4.1 -0.55 1.03) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet2 ((4.1 0.15 1.4) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet3 ((4.1 -0.3 1.73) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk1 ((4.1 -0.55 1.4) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk2 ((1.5 -1.4 1.37) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl1 ((1.5 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl2 ((1.8 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar1 ((1.5 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar2 ((1.8 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))
    (spawn-human)
	)

;; This function spawns the product onto the shelfs
(defun spawn-products-two ()
	 (btr-utils:kill-all-objects)
	 (prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal3 ((4.1 0.15 2.05) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal2 ((2 -1.4 1.05) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet1 ((4.1 -0.55 1.03) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet2 ((4.1 0.15 1.4) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet3 ((4.1 -0.3 1.73) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk1 ((4.1 -0.55 1.4) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl1 ((1.5 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl2 ((1.8 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar1 ((1.5 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar2 ((1.8 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))

	(spawn-human)
	)

;; This function spawns the product onto the shelfs
(defun spawn-products-three ()
	 (btr-utils:kill-all-objects)

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet1 ((4.1 -0.55 1.03) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet2 ((4.1 0.15 1.4) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet3 ((4.1 -0.3 1.73) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk1 ((4.1 -0.55 1.4) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl1 ((1.5 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl2 ((1.8 -1.4 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar1 ((1.5 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar2 ((1.8 -1.4 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))

	(spawn-human)
	)

;; This function spawns the product onto the shelfs
(defun spawn-products-four ()
	 (btr-utils:kill-all-objects)
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal1 ((4.05 0.6 1.03) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal2 ((2 1.7 1.05) (0 0 0 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-cereal3 ((4.05 0.5 1.73) (0 0 1 1))
			:mass 0 :color (1 0 0) :mesh :breakfast-cereal))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet1 ((4.05 -0.1 1.03) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet2 ((4.05 0.2 1.4) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-sachet3 ((4.05 -0.1 1.73) (0 0 1 1))
			:mass 0 :color (0 0.3 0) :mesh :somat))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk1 ((4.05 -0.1 1.4) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-milk2 ((1.5 1.7 1.37) (0 0 1 1))
			:mass 0 :color (1 0 1) :mesh :milk))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl1 ((1.5 1.7 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bowl2 ((1.8 1.7 0.52) (0 0 1 1))
			:mass 0 :color (0 0.9 0.9) :mesh :bowl))))

	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar1 ((1.5 1.7 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-bar2 ((1.8 1.7 0.77) (0 0 1 1))
			:mass 0 :color (0.5 0 0.1) :mesh :denkmit))))

	(spawn-human)
	)

;; Put the arms are a parking state
(defun park-arms()
	(cram-executive:perform
		(desig:an action
			(type positioning-arm)
			(left-configuration park)
			(right-configuration park))))

;; This function spawns the human behind the robot's original position
(defun spawn-human()
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object ?world :mesh :my-human ((-1 0 1) (0 0 1 1))
			:mass 0.2 :color (1 1 0) :mesh :body :scale 0.3)))))

;; This function repositions the robot to face the human
(defun reposition-robot()
	(prolog:prolog '(and (btr:bullet-world ?world)
		(assert (btr:object-pose ?world :JULIETTEY20MP
			((0 0 0) (0 0 1 0)))))))

;; Repositions the human avatar 
(defun reposition-human()
	(prolog:prolog '(and (btr:bullet-world ?world)
                              (assert (btr:object-pose ?world :my-human ((-1 0 1) (0 0 1 1)))))))

;; This function returns a list of shelf poses
;; It takes the product name as an argument 
(defun get-shelf-pose(?product-name)
	(let* ((list-of-shelves 
		(cut:force-ll (prolog `(and(is-on-shelf ,?product-name ?shelf))))) 
		(shelf-location-list nil)
		(shelf-name nil)
		(shelf-btr-name nil)
		(shelf-pose-trans nil)
		(shelf-str nil)
		)

	(dolist (element list-of-shelves)
		(setf shelf-name (cdar element)) ;;; takes the first element (?SHELF . SHELF-ONE-LEVEL-4) then the list without the fisrt SHELF-ONE-LEVEL-4

		(setf shelf-btr-name (cdaar (cut:force-ll (prolog `(and(is-located-at ,shelf-name ?location)))))) ;;; :|DM-SHELVES.shelf_1_level_4_link|

		(setf shelf-pose-trans 
			(cl-transforms:pose->transform
			(btr:pose 
				(find shelf-btr-name		
					(btr:rigid-bodies (btr:get-environment-object)) :key #'btr::name :test #'equalp))))
		(setf shelf-str (subseq (write-to-string shelf-btr-name) 13 20))

		(cond ((string-equal shelf-str "shelf_1")
			(setf shelf-location-list (cons	
					(cl-transforms:transform->pose
						(cl-transforms:transform* shelf-pose-trans *shelf-one-offset*))

				shelf-location-list)))

		((string-equal shelf-str "shelf_2")	
			(setf shelf-location-list (cons
					(cl-transforms:transform->pose
						(cl-transforms:transform* shelf-pose-trans *shelf-two-offset*))

				shelf-location-list))
			)))

	shelf-location-list))

;; This function finds a product based on the name
;; It tries to look for the product from a different direction on a shelf 
;; It also handles failures accordingly
(defun find-product (?product-name ?product-pose)
 
	(generate-look-directions ?product-pose)

	(let* ((?object-type (get-product-type ?product-name))
		(possible-look-directions `(,*look-center*
			,*look-right*
			,*look-left*))
		(?looking-direction (first possible-look-directions)))

	(setf possible-look-directions (cdr possible-look-directions))

	(move-to-location (calculate-robot-navigation-goal-towards-target ?looking-direction))

	(look-at-product ?looking-direction)

	(cpl:with-failure-handling
		((cram-common-failures:perception-object-not-found (e)
           ;; Try different look directions until there is none left.
           (when possible-look-directions
           	(roslisp:ros-warn (perception-failure) "~a~%Turning head." e)

           	(format t "Changing viewing direction!")
           	(setf ?looking-direction (first possible-look-directions))

           	(setf possible-look-directions (cdr possible-look-directions))

           	(move-to-location (calculate-robot-navigation-goal-towards-target ?looking-direction))
           	(look-at-product ?looking-direction)

           	(cpl:retry))
           (return)))

		(cram-executive:perform
			(desig:an action
				(type detecting)
				(object (desig:an object
					(type ?object-type)))))
		)))

;; This function generates the a possible location from which
;; the robot can see the product.
(defun generate-robot-pose(?shelf-pose)
    (let* ((?product-pose ?shelf-pose)
        (?product-pose-stamped (cl-transforms-stamped:pose->pose-stamped "map" 0 ?product-pose))
        (?product-location-designator (desig:a location (pose ?product-pose-stamped)))
        (to-see-designator (desig:a location (visible-for :JULIETTEY20MP)
            (location ?product-location-designator))))
    (desig:reference to-see-designator)))

;; This function generates and returns a new robot position based 
;; on its current position. It multiplies the current position of
;; the robot by an offset.
(defun get-robot-new-pose()
	(let* ((robot-cur-pose (cram-tf:robot-current-pose))
		(robot-cur-trans (cl-transforms:pose->transform robot-cur-pose)))
 
	(cl-transforms-stamped:pose->pose-stamped "map" 0 
		(cl-transforms:transform->pose
			(cl-transforms:transform* robot-cur-trans  *robot-pose-offset*)))))

;; This function generates and returns a new human position based 
;; on the robots position. It multiplies the current position of the robot
;; by an offset
(defun get-human-new-pose()
	(let* ((robot-cur-pose (cram-tf:robot-current-pose))
		(robot-cur-trans (cl-transforms:pose->transform robot-cur-pose)))
	(cl-transforms-stamped:pose->pose-stamped "map" 0 
		(cl-transforms:transform->pose
			(cl-transforms:transform* robot-cur-trans  *human-pose-offset*)))))

(defun calculate-robot-navigation-goal-towards-target (?location-pose)
	(calculate-pose-towards-target
		?location-pose
		(cram-tf:robot-current-pose)))

;; Given a `look-pose-stamped' and a `robot-pose-stamped' (both in fixed frame),
;; calculate the new robot-pose-stamped, which is rotated with an angle to point towards
;; the `look-pose-stamped'
(defun calculate-pose-towards-target (look-pose-stamped robot-pose-stamped)
 
	(let* ((world->robot-transform
		(cram-tf:pose-stamped->transform-stamped robot-pose-stamped "robot"))
	(robot->world-transform
		(cl-transforms:transform-inv world->robot-transform))
	(world->look-pose-origin
		(cl-transforms:origin look-pose-stamped))
	(look-pose-in-robot-frame
		(cl-transforms:transform-point
			robot->world-transform
			world->look-pose-origin))
	(rotation-angle
		(atan
			(cl-transforms:y look-pose-in-robot-frame)
			(cl-transforms:x look-pose-in-robot-frame))))
	(cram-tf:rotate-pose robot-pose-stamped :z rotation-angle)))

;; This function generates the left possible looking direction
;; It takes the product pose as an argument
(defun get-left-pose(?product-pose)
	(let* ((mapTshelf-pose ?product-pose)
		(mapTshelf-trans (cl-transforms:pose->transform mapTshelf-pose)))
	(cl-transforms-stamped:pose->pose-stamped "map" 0 
		(cl-transforms:transform->pose
			(cl-transforms:transform* mapTshelf-trans *left-offset*)))))

;; This function generates the right possible looking direction
;; It takes the product pose as an argument
(defun get-right-pose(?product-pose)
	(let* ((mapTshelf-pose ?product-pose)
		(mapTshelf-trans (cl-transforms:pose->transform mapTshelf-pose)))
	(cl-transforms-stamped:pose->pose-stamped "map" 0 
		(cl-transforms:transform->pose
			(cl-transforms:transform* mapTshelf-trans *right-offset*)))))

;; This function generates possible looking directions for the robot
(defun generate-look-directions (?product-pose)
	(setf *look-center* (cl-transforms-stamped:pose->pose-stamped "map" 0  ?product-pose))
	(setf *look-right* (get-right-pose ?product-pose))
	(setf *look-left* (get-left-pose ?product-pose)))

;; This function queries our reasoning base and returns the type of 
;; product based on the name
(defun get-product-type(?product-name)
	(cdaar (cut:force-ll (prolog `(and(is-of-type ,?product-name ?type))))))

;; This function helps the robot to look at the given
;; location.
(defun look-at-product (?product-direction)
	(cpl:with-retry-counters ((error-counter 2))
		(cpl:with-failure-handling
 
		  ((cram-common-failures:ptu-goal-not-reached (e)
 
	       	(roslisp:ros-warn (perception-failure) "~a~%Looking at product went wrong...repositioning" e)
	       	(cpl:do-retry error-counter
 
	    	  (move-to-location (get-robot-new-pose))
	    	  (cpl:retry))

	       (cpl:fail 'common-fail:looking-high-level-failure)))
 
			(cram-executive:perform (desig:a action 
				(type looking)
				(target (desig:a location
					(pose ?product-direction))))))))

;; This function moves the robot to a given pose
(defun move-to-location(?pose)
	(let ((?navigation-goal ?pose))
		(cram-executive:perform (desig:an action
			(type going)
			(target (desig:a location 
				(pose ?navigation-goal)))))))

;; This function moves the robot to a new position
(defun move-human-to-location()
	(let* ((?new-human-location (get-human-new-pose)))
		(setf (btr:pose (btr:object btr:*current-bullet-world* :my-human)) ?new-human-location)))

;; Point forward using the right arm
(defun point-front-right()
	(cram-executive:perform
		(desig:an action
			(type positioning-arm)
			(left-configuration park)
			(right-configuration point-ahead))))