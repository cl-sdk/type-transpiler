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
