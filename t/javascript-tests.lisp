(in-package #:domaindsl.tests)

(def-suite domaindsl.javascript-suite)

(in-suite domaindsl.javascript-suite)

(def-test generate-artifact-for-datatypes ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :javascript ty)))
    (destructuring-bind (art)
        arts
      (5am:is (equal
               'test
               (domaindsl.artifact:artifact-name art)))
      (5am:is (equal
               "Test.js"
               (domaindsl.artifact:artifact-file art)))
      (5am:is (equal
               ty
               (domaindsl.artifact:artifact-content art))))))

(def-test render-a-type-without-constructors ()
  (let* ((ty (domaindsl.types:make-data-type :name 'test
                                             :ctors nil))
         (arts (domaindsl.artifact:generate-artifact :javascript ty)))
    (destructuring-bind (base-class)
        (mapcar (lambda (art)
                  (domaindsl.artifact:compile-artifact :javascript art))
                arts)
      (5am:is (string-equal "export function Test() {}" base-class)))))

(def-test render-a-type-with-single-constructor-and-no-arguments ()
  (let* ((ctor (domaindsl.types:make-type-constructor :name 'test-ctor
                                                      :args nil
                                                      :of-class 'test))
         (ty (domaindsl.types:make-data-type :name 'test
                                             :ctors (list ctor)))
         (arts (domaindsl.artifact:generate-artifact :javascript ty))
         (compiled (mapcar (lambda (art)
                             (domaindsl.artifact:compile-artifact :javascript art))
                           arts)))
    (destructuring-bind (base-class constructor)
        compiled
      (5am:is (string-equal "export function Test() {}" base-class))
      (5am:is (string-equal "export const TestCtor = new (function TestCtor() {})" constructor)))))

(def-test render-a-type-with-single-constructor-and-1-argument ()
  (let* ((arg (domaindsl.types:make-constructor-argument :name 'class-for-argument
                                                         :type 'class-for-argument
                                                         :reference (find-class 'class-for-argument)
                                                         :kind :class
                                                         :array nil))
         (ctor (domaindsl.types:make-type-constructor :name 'test-ctor
                                                      :args (list arg)
                                                      :of-class 'test))
         (ty (domaindsl.types:make-data-type :name 'test
                                             :ctors (list ctor)))
         (arts (domaindsl.artifact:generate-artifact :javascript ty))
         (compiled (mapcar (lambda (art)
                             (domaindsl.artifact:compile-artifact :javascript art))
                           arts)))
    (destructuring-bind (base-class constructor)
        compiled
      (5am:is (string-equal "export function Test() {}" base-class))
      (5am:is (string-equal "export function TestCtor(classForArgument) {
  this.classForArgument = classForArgument;
}" constructor)))))

(def-test render-a-type-with-single-constructor-and-1-named-argument ()
  (let* ((arg (domaindsl.types:make-constructor-argument :name 'arg
                                                         :type 'class-for-argument
                                                         :reference (find-class 'class-for-argument)
                                                         :kind :class
                                                         :array nil))
         (ctor (domaindsl.types:make-type-constructor :name 'test-ctor
                                                      :args (list arg)
                                                      :of-class 'test))
         (ty (domaindsl.types:make-data-type :name 'test
                                             :ctors (list ctor)))
         (arts (domaindsl.artifact:generate-artifact :javascript ty))
         (compiled (mapcar (lambda (art)
                             (domaindsl.artifact:compile-artifact :javascript art))
                           arts)))
    (destructuring-bind (base-class constructor)
        compiled
      (5am:is (string-equal "export function Test() {}" base-class))
      (5am:is (string-equal "export function TestCtor(arg) {
  this.arg = arg;
}" constructor)))))
