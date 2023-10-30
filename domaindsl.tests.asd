(asdf:defsystem #:domaindsl.tests
  :depends-on (#:fiveam
               #:str
               #:domaindsl.types
               #:domaindsl.artifact
               #:domaindsl.swift
               #:domaindsl.kotlin)
  :components ((:file "tests")
               (:file "types-tests")
               (:file "swift-tests")
               (:file "kotlin-tests")))
