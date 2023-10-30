(defpackage #:domaindsl.artifact
  (:use #:cl)
  (:export
   #:generate-artifact
   #:artifact
   #:make-artifact
   #:artifact-name
   #:artifact-file
   #:artifact-content
   #:compile-artifact
   #:artifact-extension))

(in-package #:domaindsl.artifact)

(defstruct artifact
  name
  file
  content)

(defgeneric artifact-extension (target))

(defgeneric generate-artifact (target object))

(defgeneric compile-artifact (target object))
