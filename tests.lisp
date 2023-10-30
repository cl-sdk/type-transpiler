(defpackage #:domaindsl.tests
  (:use #:cl)
  (:import-from #:5am
                #:def-test
                #:in-suite
                #:def-suite))

(in-package #:domaindsl.tests)

(defclass class-for-argument () ())

(def-suite domaindsl.types-suite)

(in-suite domaindsl.types-suite)

(defclass class-for-argument () ())

(progn
  (domaindsl.types:datatype
      single
    ((single)))
  (def-test declare-a-type-with-a-single-constructor ()
    (let* ((ty (gethash 'single (domaindsl.types:all-types)))
           (ctor (first (domaindsl.types:object-constructors ty))))
      (5am:is (equal
               'single
               (domaindsl.types:object-name ty)))
      (5am:is (equal
               'single
               (domaindsl.types:object-name ctor))))
    (domaindsl.types:clean-all-types)))

(progn
  (domaindsl.types:datatype
      single
    ((single (:class class-for-argument))))
  (def-test declare-a-type-with-a-single-constructor-with-arguments ()
    (let* ((ty (gethash 'single (domaindsl.types:all-types)))
           (ctor (first (domaindsl.types:object-constructors ty)))
           (arg (first (domaindsl.types:object-arguments ctor))))
      (5am:is (equal
               'single
               (domaindsl.types:object-name)))
      (5am:is (equal
               'single
               (domaindsl.types:object-name ctor)))
      (5am:is (equal
               'class-for-argument
               (domaindsl.types:object-name arg)))
      (5am:is (equal
               'class
               (domaindsl.types:object-kind arg)))
      (5am:is (equal
               (find-class 'class-for-argument)
               (domaindsl.types:object-reference arg))))
    (domaindsl.types:clean-all-types)))

;; -------

(def-suite domaindsl.swift-suite)

(in-suite domaindsl.swift-suite)

(defclass class-for-argument () ())

(def-test lisp-symbol->swift-class-name ()
  (5am:is (equal
           "Test"
           (domaindsl.types:object-to-class-name-string :swift 'test))))

(def-test lisp-symbol->swift-enum-tag ()
  (5am:is (equal
           "test"
           (domaindsl.types:object-to-constructor-name-string :swift 'test))))

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test
                                             :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :swift ty))
         (art (car arts)))
    (5am:is (= 1 (length arts)))
    (5am:is (equal
             'test
             (domaindsl.artifact:artifact-name art)))
    (5am:is (equal
             "Test"
             (domaindsl.artifact:artifact-file art)))
    (5am:is (equal
             ty
             (domaindsl.artifact:artifact-content art)))))

;; -------

(def-suite domaindsl.kotlin-suite)

(in-suite domaindsl.kotlin-suite)

(defclass class-for-argument () ())

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test
                                             :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :kotlin ty))
         (art (car arts)))
    (5am:is (= 1 (length arts)))
    (5am:is (equal
             'test
             (domaindsl.artifact:artifact-name art)))
    (5am:is (equal
             "Test.kt"
             (domaindsl.artifact:artifact-file art)))
    (5am:is (equal
             ty
             (domaindsl.artifact:artifact-content art)))))
