(in-package #:domaindsl.tests)

(def-suite domaindsl.types-suite)

(in-suite domaindsl.types-suite)

(def-test data-type-to-swift-class-name ()
  (let ((ty (domaindsl.types:make-data-type :name 'test :ctors nil)))
    (5am:is (string-equal
             "Test"
             (domaindsl.types:object-to-class-name-string :swift ty)))))

  (domaindsl.types:datatype
      single
    ((single)))

(domaindsl.types:datatype
    single-2
  ((single (:class class-for-argument))))

(def-test declare-a-type-with-a-single-constructor-no-arguments ()
  (let* ((ty (gethash 'single (domaindsl.types:all-types)))
         (ctor (first (domaindsl.types:object-constructors ty))))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ty)))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ctor)))))

(def-test declare-a-type-with-a-single-constructor-with-arguments ()
  (let* ((ty (gethash 'single-2 (domaindsl.types:all-types)))
         (ctor (first (domaindsl.types:object-constructors ty)))
         (arg (first (domaindsl.types:object-arguments ctor))))
    (5am:is (equal
             'single-2
             (domaindsl.types:object-name ty)))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ctor)))
    (5am:is (equal
             'class-for-argument
             (domaindsl.types:object-name arg)))
    (5am:is (equal
             :class
             (domaindsl.types:object-argument-kind arg)))
    (5am:is (equal
             (find-class 'class-for-argument)
             (domaindsl.types:object-reference arg)))))
