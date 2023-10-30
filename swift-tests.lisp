(in-package #:domaindsl.tests)

(def-suite domaindsl.swift-suite)

(in-suite domaindsl.swift-suite)

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
