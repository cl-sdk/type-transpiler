(asdf:defsystem #:domaindsl.kotlin
  :depends-on (#:str
               #:domaindsl.types
               #:domaindsl.artifact)
  :components ((:file "src/kotlin-render")))
