(defpackage #:domaindsl.types
  (:use #:cl)
  (:export
   #:all-types
   #:clean-all-types
   #:class-reference
   #:data-type
   #:type-constructor
   #:constructor-argument
   #:find-reference
   #:missing-constructor-argument
   #:create-constructor-argument
   #:build-constructor-argument
   #:object-name
   #:object-reference
   #:object-constructors
   #:object-arguments
   #:object-of-class
   #:object-type
   #:object-array
   #:object-to-class-name-string
   #:object-to-constructor-name-string
   #:object-to-constructor-variable-name-string
   #:object-argument-is-array
   #:object-argument-type
   #:datatype
   #:make-data-type
   #:make-class-reference
   #:make-type-constructor
   #:make-constructor-argument
   #:object-argument-kind))

(in-package #:domaindsl.types)

(defparameter *all* (make-hash-table))

(defun all-types ()
  *all*)

(defun clean-all-types ()
  (setf *all* (make-hash-table)))

(defstruct class-reference
  name
  reference)

(defstruct data-type
  name
  ctors)

(defstruct type-constructor
  name
  args
  of-class)

(defstruct constructor-argument
  name
  type
  kind
  reference
  array)

(defun add-to-table (name value &optional (replacable t))
  (if (or replacable
          (null (gethash name *all*)))
      (setf (gethash name *all*) value)
      (format t "warn: ~a already on the table~%" name)))

(defun find-reference (kind type-name)
  (case kind
    (:class (find-class type-name))
    (:datatype (gethash type-name *all*))))

(defun missing-constructor-argument (kind type-name opts)
    (error (format nil "missing constructor argument's type reference type ~a ~a (opts: ~a)" kind type-name opts)))

(defun create-constructor-argument (kind type-name klass opts)
  (make-constructor-argument :name (or (cadr (member :name opts)) type-name)
                             :type type-name
                             :array (cadr (member :array opts))
                             :kind kind
                             :reference klass))

(defun build-constructor-argument (arg)
  (etypecase arg
    ((or list cons)
     (destructuring-bind (kind type-name &optional opts)
         arg
       (let ((klass (find-reference kind type-name)))
         (if (null klass)
             (missing-constructor-reference kind type-name opts)
             (prog1 (create-constructor-argument kind type-name klass opts)
               (add-to-table type-name
                             (make-class-reference :name type-name :reference klass)
                             nil))))))
    (t (error (format nil "don't know how to build constructor argument for: ~a" arg)))))

(defmacro datatype (name &body ctors)
  (let ((ctors (loop for ctor in (car ctors)
                     collect (destructuring-bind (cname . args)
                                 ctor
                               (make-type-constructor :name cname
                                                      :args (mapcar #'build-constructor-argument args)
                                                      :of-class name)))))
    (add-to-table name (make-data-type :name name :ctors ctors)))
  nil)

(defgeneric object-name (o))

(defmethod object-name ((o class-reference))
  (class-reference-name o))
(defmethod object-name ((o data-type))
  (data-type-name o))
(defmethod object-name ((o type-constructor))
  (type-constructor-name o))
(defmethod object-name ((o constructor-argument))
  (constructor-argument-name o))

(defgeneric object-reference (o))

(defmethod object-reference ((o class-reference))
  (class-reference-reference o))
(defmethod object-reference ((o constructor-argument))
  (constructor-argument-reference o))

(defgeneric object-constructors (o))

(defmethod object-constructors ((o data-type))
  (data-type-ctors o))

(defgeneric object-arguments (o))

(defmethod object-arguments ((o type-constructor))
  (type-constructor-args o))

(defgeneric object-of-class (o))

(defmethod object-of-class ((o type-constructor))
  (type-constructor-of-class o))

(defgeneric object-argument-type (o))

(defmethod object-argument-type ((o constructor-argument))
  (constructor-argument-type o))

(defgeneric object-argument-kind (o))

(defmethod object-argument-kind ((o constructor-argument))
  (constructor-argument-kind o))

(defgeneric object-argument-is-array (o))

(defmethod object-argument-is-array ((o constructor-argument))
  (constructor-argument-array o))

(defgeneric object-to-class-name-string (target o))
(defgeneric object-to-constructor-name-string (target o))
(defgeneric object-to-constructor-variable-name-string (target o))
