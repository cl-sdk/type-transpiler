(asdf:defsystem #:domaindsl.kotlin
  :depends-on (#:str
               #:domaindsl.types
               #:domaindsl.artifact)
  :components ((:file "kotlin-render")))
