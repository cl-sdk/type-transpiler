(in-package #:domaindsl.tests)

(def-suite domaindsl.kotlin-suite)

(in-suite domaindsl.kotlin-suite)

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
