(defpackage #:domaindsl.types
  (:use #:cl)
  (:export
   #:datatype
   #:all-types
   #:dt-type
   #:make-dt-type
   #:dt-type-name
   #:dt-type-ctors
   #:dt-ctor
   #:make-dt-ctor
   #:dt-ctor-name
   #:dt-ctor-args
   #:dt-ctor-of-class
   #:ctor-arg
   #:make-ctor-arg
   #:ctor-arg-name
   #:ctor-arg-type
   #:ctor-arg-kind
   #:ctor-arg-reference
   #:ctor-arg-array
   #:dt-class
   #:make-dt-class
   #:dt-class-name
   #:dt-class-reference
   #:clean-all-types
   #:dt-name))

(in-package #:domaindsl.types)

(defparameter *all* (make-hash-table))

(defun all-types ()
  *all*)

(defun clean-all-types ()
  (setf *all* (make-hash-table)))

(defstruct dt-class
  name
  reference)

(defstruct dt-type
  name
  ctors)

(defstruct dt-ctor
  name
  args
  of-class)

(defstruct ctor-arg
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

(defun build-ctor-arg (arg)
  (etypecase arg
    ((or list cons)
     (destructuring-bind (kind type-name &optional opts)
         arg
       (let ((klass (find-reference kind type-name)))
         (if (null klass)
             (error (format nil "missing ctor arg reference type ~a ~a (opts: ~a)" kind type-name opts))
             (prog1 (make-ctor-arg :name (or (cadr (member :name opts)) type-name)
                                   :type type-name
                                   :array (cadr (member :array opts))
                                   :kind kind
                                   :reference klass)
               (add-to-table type-name
                             (make-dt-class :name type-name
                                            :reference klass)
                             nil))))))))

(defmacro datatype (name &body ctors)
  (let ((ctors (loop for ctor in (car ctors)
                     collect (destructuring-bind (cname . args)
                                 ctor
                               (make-dt-ctor :name cname
                                             :args (mapcar #'build-ctor-arg args)
                                             :of-class name)))))
    (add-to-table name (make-dt-type :name name :ctors ctors)))
  nil)

(defgeneric dt-name (o))

(defmethod dt-name ((o dt-type))
  (dt-type-name o))

(defmethod dt-name ((o dt-class))
  (dt-class-name o))
