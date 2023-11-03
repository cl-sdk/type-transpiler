(defpackage #:domaindsl.render
  (:use :cl)
  (:export
   #:render-object))

(in-package #:domaindsl.render)

(defgeneric render-object (target o))
