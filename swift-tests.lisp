(in-package #:domaindsl.tests)

(def-suite domaindsl.swift-suite)

(in-suite domaindsl.swift-suite)

(def-test data-type->swift-class-name ()
  (let ((ty (domaindsl.types:make-data-type :name 'test
                                            :ctors nil)))
    (5am:is (equal
             "Test"
             (domaindsl.types:object-to-class-name-string :swift ty)))))

(def-test type-constructor->swift-class-name ()
  (let ((ty (domaindsl.types:make-type-constructor :name 'test-test
                                                   :args nil
                                                   :of-class nil)))
    (5am:is (equal
             "testTest"
             (domaindsl.types:object-to-class-name-string :swift ty)))))

(def-test constructor-argument->swift-class-name ()
  (let ((ty (domaindsl.types:make-constructor-argument
             :name 'test :type 'test :kind nil
             :reference nil :array nil)))
    (5am:is (equal
             "Test"
             (domaindsl.types:object-to-class-name-string :swift ty)))))

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test
                                             :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :swift ty))
         (art (car arts)))
    (5am:is (= 1 (length arts)))
    (5am:is (equal
             "Test"
             (domaindsl.artifact:artifact-name art)))
    (5am:is (equal
             "Test"
             (domaindsl.artifact:artifact-file art)))
    (5am:is (equal
             ty
             (domaindsl.artifact:artifact-content art)))))
