(asdf:defsystem #:domaindsl.artifact
  :depends-on (#:str
               #:domaindsl.types)
  :components ((:file "src/artifact")))
