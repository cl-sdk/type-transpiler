(asdf:defsystem #:domaindsl.tests
  :depends-on (#:fiveam
               #:str
               #:domaindsl.types
               #:domaindsl.artifact
               #:domaindsl.swift
               #:domaindsl.kotlin)
  :components ((:file "t/tests")
               (:file "t/types-tests")
               (:file "t/swift-tests")
               (:file "t/kotlin-tests")))
