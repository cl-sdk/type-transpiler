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

(progn
  (domaindsl.types:datatype
      single
    ((single)))
  (def-test declare-a-type-with-a-single-constructor ()
    (let* ((ty (first (domaindsl.types:all-types)))
           (ctor (first (domaindsl.types:dt-type-ctors ty))))
      (5am:is (equal
               'single
               (domaindsl.types:dt-type-name ty)))
      (5am:is (equal
               'single
               (domaindsl.types:dt-ctor-name ctor))))
    (domaindsl.types:clean-all-types)))

(progn
  (domaindsl.types:datatype
      single
    ((single (class class-for-argument))))
  (def-test declare-a-type-with-a-single-constructor-with-arguments ()
    (let* ((ty (first (domaindsl.types:all-types)))
           (ctor (first (domaindsl.types:dt-type-ctors ty)))
           (arg (first (domaindsl.types:dt-ctor-args ctor))))
      (5am:is (equal
               'single
               (domaindsl.types:dt-type-name ty)))
      (5am:is (equal
               'single
               (domaindsl.types:dt-ctor-name ctor)))
      (5am:is (equal
               'class-for-argument
               (domaindsl.types:ctor-arg-name arg)))
      (5am:is (equal
               'class
               (domaindsl.types:ctor-arg-kind arg)))
      (5am:is (equal
               (find-class 'class-for-argument)
               (domaindsl.types:ctor-arg-reference arg))))
    (domaindsl.types:clean-all-types)))

;; -------

(def-suite domaindsl.swift-suite)

(in-suite domaindsl.swift-suite)

(def-test lisp-symbol->swift-class-name ()
  (5am:is (equal
           "Test"
           (domaindsl.swift:to-class-name 'test))))

(def-test lisp-symbol->swift-enum-tag ()
  (5am:is (equal
           "test"
           (domaindsl.swift:to-enum-tag 'test))))

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-dt-type :name 'test
                                          :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :swift ty))
         (art (car arts)))
    (5am:is (= 1 (length arts)))
    (5am:is (equal
             "Test"
             (domaindsl.artifact:artifact-name art)))
    (5am:is (equal
             "Test.swift"
             (domaindsl.artifact:artifact-file art)))
    (5am:is (equal
             ty
             (domaindsl.artifact:artifact-content art)))))

;; -------

(def-suite domaindsl.kotlin-suite)

(in-suite domaindsl.kotlin-suite)

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-dt-type :name 'test
                                          :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :kotlin ty))
         (art (car arts)))
    (5am:is (= 1 (length arts)))
    (5am:is (equal
             "Test"
             (domaindsl.artifact:artifact-name art)))
    (5am:is (equal
             "Test.kt"
             (domaindsl.artifact:artifact-file art)))
    (5am:is (equal
             ty
             (domaindsl.artifact:artifact-content art)))))
