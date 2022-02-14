(in-package :pepper-descr)

(defparameter *neck-good-looking-down-state*
  '(("HeadYaw" 0.00)
    ("HeadPitch" 0.62)))

(defparameter *neck-good-looking-left-state*
  '(("HeadYaw" 0.86)
    ("HeadPitch" 0.00)))

(defparameter *neck-parking-joint-states*
  '(("HeadYaw" 0.00)
    ("HeadPitch" -0.00)))


(def-fact-group pepper-neck-facts (robot-neck-links
                                  robot-neck-joints
                                  robot-neck-base-link
                                  robot-joint-states
                                  camera-in-neck-ee-pose)

  (<- (robot-neck-links :JULIETTEY20MP
                        "Neck"
                        "Head"))

  (<- (robot-neck-joints :JULIETTEY20MP
                         "HeadYaw"
                         "HeadPitch"))

  (<- (robot-joint-states :JULIETTEY20MP :neck ?there-is-only-one-neck :away ?joint-states)
    (symbol-value *neck-parking-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :neck ?there-is-only-one-neck :forward ?joint-states)
    (symbol-value *neck-parking-joint-states* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :neck ?there-is-only-one-neck :down ?joint-states)
    (symbol-value *neck-good-looking-down-state* ?joint-states))

  (<- (robot-joint-states :JULIETTEY20MP :neck ?there-is-only-one-neck :down-left ?joint-states)
    (symbol-value *neck-good-looking-left-state* ?joint-states)))