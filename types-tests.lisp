(in-package #:domaindsl.tests)

(def-suite domaindsl.types-suite)

(in-suite domaindsl.types-suite)

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
               (domaindsl.types:object-name ty)))
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
