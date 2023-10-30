(defpackage #:domaindsl.tests
  (:use #:cl)
  (:import-from #:5am
                #:def-test
                #:in-suite
                #:def-suite))

(in-package #:domaindsl.tests)

(defclass class-for-argument () ())
