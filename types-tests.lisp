(in-package #:domaindsl.tests)

(def-suite domaindsl.types-suite)

(in-suite domaindsl.types-suite)

(def-test declare-a-type-with-a-single-constructor-no-arguments ()
  (let* ((ctors (list (domaindsl.types:make-type-constructor :name 'single
                                                             :args nil
                                                             :of-class 'single)))
         (ty (domaindsl.types:make-data-type :name 'single :ctors ctors))
         (ctor (first (domaindsl.types:object-constructors ty))))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ty)))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ctor)))))

(def-test declare-a-type-with-a-single-constructor-with-arguments ()
  (let* ((ctor-arg (domaindsl.types:make-constructor-argument :name 'class-for-argument
                                                              :type 'class-for-argument
                                                              :kind :class
                                                              :reference (find-class 'class-for-argument)
                                                              :array nil))
         (ctor-args (list ctor-arg))
         (ctors (list (domaindsl.types:make-type-constructor :name 'single
                                                             :args ctor-args
                                                             :of-class 'test)))
         (ty (domaindsl.types:make-data-type :name 'single :ctors ctors))
         (ctor (first (domaindsl.types:object-constructors ty))))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ty)))
    (5am:is (equal
             'single
             (domaindsl.types:object-name ctor)))
    (5am:is (equal
             'class-for-argument
             (domaindsl.types:object-name ctor-arg)))
    (5am:is (equal
             :class
             (domaindsl.types:object-argument-kind ctor-arg)))
    (5am:is (equal
             (find-class 'class-for-argument)
             (domaindsl.types:object-reference ctor-arg)))))
