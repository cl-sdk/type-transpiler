(defpackage #:domaindsl.artifact
  (:use #:cl)
  (:export
   #:generate-artifact
   #:artifact
   #:make-artifact
   #:artifact-name
   #:artifact-file
   #:artifact-content
   #:compile-artifact))

(in-package #:domaindsl.artifact)

(defstruct artifact
  name
  file
  content)

(defgeneric generate-artifact (target object))

(defgeneric compile-artifact (target object))
