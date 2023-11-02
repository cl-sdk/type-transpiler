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

(def-test render-a-type-without-constructors ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test
                                             :ctors nil))
         (art (first (domaindsl.artifact:generate-artifact :swift ty))))
    (5am:is (string-equal "enum Test {
}"
                          (domaindsl.artifact:compile-artifact :swift art)))))

(def-test render-a-type-with-single-constructor-and-no-arguments ()
  (let* ((ctor (domaindsl.types:make-type-constructor :name 'test-ctor
                                                      :args nil
                                                      :of-class nil))
         (ty (domaindsl.types:make-data-type :name 'test
                                             :ctors (list ctor)))
         (art (first (domaindsl.artifact:generate-artifact :swift ty))))
    (5am:is (string-equal "enum Test {
  case testCtor
}"
                          (domaindsl.artifact:compile-artifact :swift art)))))

(def-test render-a-type-with-single-constructor-and-1-argument ()
  (let* ((arg (domaindsl.types:make-constructor-argument :name 'arg
                                                         :type 'class-for-argument
                                                         :reference (find-class 'class-for-argument)
                                                         :kind :class
                                                         :array nil))
         (ctor (domaindsl.types:make-type-constructor :name 'test-ctor
                                                      :args (list arg)
                                                      :of-class nil))
         (ty (domaindsl.types:make-data-type :name 'test
                                             :ctors (list ctor)))
         (art (first (domaindsl.artifact:generate-artifact :swift ty))))
    (5am:is (string-equal "enum Test {
  case testCtor(ClassForArgument)
}"
                          (domaindsl.artifact:compile-artifact :swift art)))))
