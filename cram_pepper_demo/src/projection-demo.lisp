(in-package :pepper-demo)

;; Menu functions start
(defun menu-one()
 (princ "Hello my name is Pepper. How may I help you?
		Please use the number corresponding to your request.

		------Select a number-----
		1. Help me find a product.
		2. I just wanted to say hi."))

(defun menu-two()
	(princ "What do you want?
		Please use the number corresponding to your request.

		------Select a number-----
		1. Cereal.
		2. Milk.
		3. I think I am fine."))

;; Interaction execution
(defun interaction()
	(cram-urdf-projection:with-simulated-robot (park-arms))
	(reposition-human)
	(reposition-robot)
	(menu-one)
	
	(let* ((?productname nil)
			(?productposelist nil))
		(setf response (read t))

		;; first conditional statement start
		(cond ((= response 1)
				(menu-two)

			;; second conditional statement start
			(setf proresponse (read t))
			(cond ((= proresponse 1)
				(setf ?productname 'cereal)
				(setf ?productposelist (get-shelf-pose ?productname))
				(format t "Alright. Please follow me!"))
			((= proresponse 2)
				(setf ?productname 'milk)
				(setf ?productposelist (get-shelf-pose ?productname))
				(format t "Alright. Please follow me!"))
			)
			;; second conditional statement end

			(demo-one ?productname ?productposelist)

			)
			((= response 2 )
				(format t "That's nice of you. Thanks!"))
			);;first conditional statement end

	) ;; let* close brackets
 ) ;; interaction close brackets

;; Demo one
(defun demo-one(?product-name ?shelf-pose-lists)
	(cram-urdf-projection:with-simulated-robot

		;; function details
        (let*  ((shelf-pose-lists-copy ?shelf-pose-lists)
				(product-name-copy ?product-name)
				(product-pose nil)
				(found nil))

        ;; expression
		(dolist (?shelf-pose shelf-pose-lists-copy)	

			(setf product-pose (generate-robot-pose ?shelf-pose))

			(cpl:with-retry-counters ((error-counter 2))
				(cpl:with-failure-handling

					((cram-common-failures:navigation-pose-unreachable (e)

						(roslisp:ros-warn (navigation-failure) "~a~%
							Could not move to location...retrying" e)

						(cpl:do-retry error-counter

							(setf product-pose (generate-robot-pose ?shelf-pose))

							(cpl:retry))
						(return)))

					(move-to-location product-pose)))
			;; Finding product
			(setf found (find-product product-name-copy ?shelf-pose))
			(if (not (null found))
				(progn
					(point-front-right)
					(move-human-to-location)
					(format t "There is your product!")
				(return))
				(progn
					(move-human-to-location)			
					(format t "Sorry I could not find your product! However, 
						I believe it is supposed to be in this area of the shelf.")))

			)) ;; endof let* and dolist
	))