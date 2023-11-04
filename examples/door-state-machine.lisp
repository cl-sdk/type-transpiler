(ql:quickload '(:alexandria
                :domaindsl.javascript
                :domaindsl.kotlin
                :domaindsl.swift))

(domaindsl.types:datatype
 door-state
 ((opened-door)
  (closed-door)))

(domaindsl.types:datatype
    door-event
  ((open-door-event)
   (close-door-event)))

(defvar *targets* (list :kotlin :javascript :swift))

(mapcar (lambda (target)
          (mapcar (lambda (declaration)
                    (destructuring-bind (name . type)
                        declaration
                      (let ((artifacts (domaindsl.artifact:generate-artifact target type)))
                        (mapcar (lambda (artifact)
                                  (list 'artifact
                                        target
                                        name
                                        (domaindsl.artifact:compile-artifact
                                         target
                                         artifact)))
                                artifacts)
                        )))
                  (alexandria:hash-table-alist (domaindsl.types:all-types))))
        *targets*)
