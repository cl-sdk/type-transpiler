(asdf:defsystem #:domaindsl.kotlin
  :depends-on (#:str
               #:domaindsl.types
               #:domaindsl.artifact
               #:domaindsl.render)
  :components ((:file "src/kotlin-render")))
