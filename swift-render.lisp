(defpackage #:domaindsl.swift
  (:use #:cl)
  (:export
   #:swift-render
   #:render-type
   #:render-class
   #:render))

(in-package #:domaindsl.swift)

(defun to-class-name (n &optional as-array)
  (let ((name (str:pascal-case (symbol-name n))))
    (if as-array
        (str:concat "[" name "]")
        name)))

(defun to-enum-tag (n)
  (str:camel-case (symbol-name n)))

(defun to-ctor-variable-name (n)
  (to-class-name (domaindsl.types:ctor-arg-type n)
                 (domaindsl.types:ctor-arg-array n)))

(defun render-enum-case-args (args)
  (flet ((render-args (args)
           (destructuring-bind (first . rest)
               args
             (reduce (lambda (acc x)
                       (str:concat acc ", " (to-ctor-variable-name x)))
                     rest
                     :initial-value (to-ctor-variable-name first)))))
    (if (null args)
        ""
        (str:concat "(" (render-args args) ")"))))

(defun render-ctor (ct)
  (str:concat "  case "
              (to-enum-tag (domaindsl.types:dt-ctor-name ct))
              (render-enum-case-args (domaindsl.types:dt-ctor-args ct))))

(defun render-type (ty ctors)
  (str:concat
      "enum "
      (to-class-name (domaindsl.types:dt-type-name ty)) " {" ctors "}"))

(defun render-class (ty)
  (str:concat
   "class "
   (to-class-name (domaindsl.types:dt-class-name ty))
   " {}"))

(defun render (ty)
  (etypecase ty
    (domaindsl.types:dt-type (render-type
                             ty
                             (reduce (lambda (acc ct)
                                       (str:concat acc (render-ctor ct) (string #\NEWLINE)))
                                     (domaindsl.types:dt-type-ctors ty)
                                     :initial-value (string #\NEWLINE))))
    (domaindsl.types:dt-class (render-class ty))))

(defun file-for-artifact (name o)
  (domaindsl.artifact:make-artifact :name name
                                   :file name
                                   :content o))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :swift)) (o domaindsl.types:dt-type))
  (let ((class-name (to-class-name
                     (domaindsl.types:dt-type-name o))))
    (list (file-for-artifact class-name o))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :swift)) (o domaindsl.types:dt-class))
  (let ((class-name (to-class-name
                     (domaindsl.types:dt-class-name o))))
    (list (file-for-artifact class-name o))))

(defmethod domaindsl.artifact:compile-artifact
    ((target (eql :swift)) o)
  (render (domaindsl.artifact:artifact-content o)))
