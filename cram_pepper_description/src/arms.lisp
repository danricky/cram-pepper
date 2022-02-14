(in-package :pepper-descr)

(defparameter *standard-to-pepper-gripper-transform*
  (cl-transforms-stamped:make-identity-transform))

(defparameter *left-parking-joint-states*
  '(("LShoulderPitch" 1.08)
    ("LShoulderRoll" 0.51)
    ("LElbowYaw" -0.15)
    ("LElbowRoll" -0.79)
    ("LWristYaw" 0.00)))

(defparameter *right-parking-joint-states*
  '(("RShoulderPitch" 1.08)
    ("RShoulderRoll" -0.53)
    ("RElbowYaw" 0.0)
    ("RElbowRoll" 0.79)
    ("RWristYaw" 0.0)))

(defparameter *left-pointing-joint-states*
  '(("LShoulderPitch" -0.46)
    ("LShoulderRoll" 0.99)
    ("LElbowYaw" -1.18)
    ("LElbowRoll" -0.01)
    ("LWristYaw" 0.00)))

(defparameter *right-pointing-joint-states*
  '(("RShoulderPitch" -0.41)
    ("RShoulderRoll" -1.00)
    ("RElbowYaw" 0.50)
    ("RElbowRoll" 0.01)
    ("RWristYaw" 0.91)))

(defparameter *straight-pointing-joint-states*
  '(("RShoulderPitch" -0.13)
    ("RShoulderRoll" -0.37)
    ("RElbowYaw" 0.46)
    ("RElbowRoll" 0.69)
    ("RWristYaw" 1.82)))


(def-fact-group pepper-arm-facts (end-effector-link
                                  robot-tool-frame
                                  arm-joints arm-links
                                  gripper-joint gripper-link
                                  gripper-meter-to-joint-multiplier
                                  standard-to-particular-gripper-transform
                                  robot-joint-states)

  (<- (end-effector-link :JULIETTEY20MP :left "LThumb1_link"))
  (<- (end-effector-link :JULIETTEY20MP :right "RThumb1_link"))
 
  (<- (robot-tool-frame :JULIETTEY20MP :left "LThumb1"))
  (<- (robot-tool-frame :JULIETTEY20MP :right "RThumb1"))
 
  (<- (arm-joints :JULIETTEY20MP :left ("LShoulderPitch"
                                        "LShoulderRoll"
                                        "LElbowYaw"
                                        "LElbowRoll"
                                        "LWristYaw")))
  (<- (arm-joints :JULIETTEY20MP :right ("RShoulderPitch"
                                        "RShoulderRoll"
                                        "RElbowYaw"
                                        "RElbowRoll"
                                        "RWristYaw")))

  (<- (arm-links :JULIETTEY20MP :left ("LShoulder"
                                      "LBicep"
                                      "LElbow"
                                      "LForeArm"
                                      "l_wrist")))
  (<- (arm-links :JULIETTEY20MP :right ("RShoulder"
                                        "RBicep"
                                        "RElbow"
                                        "RForeArm"
                                        "r_wrist")))

  (<- (gripper-joint :JULIETTEY20MP :left "left_gripper_joint"))
  (<- (gripper-joint :JULIETTEY20MP :right "right_gripper_joint"))

  (<- (gripper-link :JULIETTEY20MP :left ?link)
    (bound ?link)
    (lisp-fun search "left_gripper" ?link ?pos)
    (lisp-pred identity ?pos))
  (<- (gripper-link :JULIETTEY20MP :right ?link)
    (bound ?link)
    (lisp-fun search "right_gripper" ?link ?pos)
    (lisp-pred identity ?pos))

  (<- (gripper-meter-to-joint-multiplier :JULIETTEY20MP 1.0))

  (<- (standard-to-particular-gripper-transform :JULIETTEY20MP ?transform)
    (symbol-value *standard-to-pepper-gripper-transform* ?transform))

  (<- (robot-joint-states :JULIETTEY20MP :arm :left :point ?joint-states)
    (symbol-value *left-pointing-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :arm :right :point ?joint-states)
    (symbol-value *right-pointing-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :arm :right :point-ahead ?joint-states)
    (symbol-value *straight-pointing-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :arm :left :park ?joint-states)
    (symbol-value *left-parking-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :arm :right :park ?joint-states)
    (symbol-value *right-parking-joint-states* ?joint-states)))