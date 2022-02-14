cmake_minimum_required(VERSION 2.8.3)
project(pepper_description)

find_package(catkin REQUIRED COMPONENTS
  urdf
  controller_manager
  joint_state_controller
  robot_state_publisher
)

catkin_package()
