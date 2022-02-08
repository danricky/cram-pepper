(in-package :pepper-descr)

(def-fact-group pepper-metadata (robot
                               robot-odom-frame
                               robot-base-frame robot-torso-link-joint
                               arm
                               camera-frame
                               camera-minimal-height
                               camera-maximal-height)
  (<- (robot :JULIETTEY20MP))

  (<- (robot-odom-frame :JULIETTEY20MP "odom"))

  (<- (robot-base-frame :JULIETTEY20MP "base_link"))
  (<- (robot-torso-link-joint :JULIETTEY20MP "torso" "base_link_fixedjoint"))

  (<- (arm :JULIETTEY20MP :left))
  (<- (arm :JULIETTEY20MP :right))

  (<- (camera-frame :JULIETTEY20MP "CameraDepth_optical_frame"))

  (<- (camera-minimal-height :JULIETTEY20MP 1.00))
  (<- (camera-maximal-height :JULIETTEY20MP 1.10))
  )
  

; Gaussian distribution configuration
(def-fact-group location-costmap-metadata ( costmap:costmap-size
                                            costmap:costmap-origin
                                            costmap:costmap-resolution
                                            costmap:orientation-samples
                                            costmap:orientation-sample-step
                                            costmap:costmap-padding
                                            costmap:costmap-manipulation-padding
                                            costmap:costmap-in-reach-distance
                                            costmap:costmap-reach-minimal-distance
                                            costmap:visibility-costmap-size)
    (<- (costmap:costmap-size 12 12))
    (<- (costmap:costmap-origin -6 -6))
    (<- (costmap:costmap-resolution 0.04))
 
    (<- (costmap:costmap-padding 0.3))
    (<- (costmap:costmap-manipulation-padding 0.4))
    (<- (costmap:costmap-in-reach-distance 0.7))
    (<- (costmap:costmap-reach-minimal-distance 0.2))
    (<- (costmap:visibility-costmap-size :JULIETTEY20MP 2))
    (<- (costmap:orientation-samples 2))
    (<- (costmap:orientation-sample-step 0.1)))