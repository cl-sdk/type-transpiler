(defpackage #:domaindsl.swift
  (:use #:cl)
  (:import-from #:domaindsl.types
                #:object-to-constructor-variable-name-string)
  (:export
   #:swift-render
   #:render-type
   #:render-class
   #:render
   #:to-class-name
   #:to-enum-tag
   #:to-ctor-variable-name))

(in-package #:domaindsl.swift)


;; struct ClassName {
;;   public val FieldName;
;; }

;; class ClassName {
;;    public val FieldName;
;; }

;; enum ClassName {
;;   case ConstructorName(ConstructorArgument,...)
;; }


(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :swift))
     (obj domaindsl.types:constructor-argument))
  (let* ((type-symbol (domaindsl.types:object-argument-type obj))
         (name (str:pascal-case (symbol-name type-symbol)))
         (is-array (domaindsl.types:object-argument-is-array obj)))
    (if is-array (str:concat "[" name "]") name)))

(defmethod domaindsl.types:object-to-constructor-name-string
    ((target (eql :swift))
     (obj domaindsl.types:constructor-argument))
  (str:camel-case (symbol-name (domaindsl.types:object-argument-type obj))))

(defmethod domaindsl.types:object-to-constructor-variable-name-string
    ((target (eql :swift))
     (obj domaindsl.types:constructor-argument))
  (domaindsl.types:object-to-class-name-string target obj))

(defun render-enum-case-args (args)
  (flet ((render-args (args)
           (destructuring-bind (first . rest)
               args
             (reduce (lambda (acc x)
                       (str:concat acc ", " (object-to-constructor-variable-name-string x)))
                     rest
                     :initial-value (object-to-constructor-variable-name-string first)))))
    (if (null args)
        ""
        (str:concat "(" (render-args args) ")"))))

(defun render-ctor (ct)
  (str:concat "  case "
              (to-enum-tag (domaindsl.types:object-name ct))
              (render-enum-case-args (domaindsl.types:object-arguments ct))))

(defun render-type (ty ctors)
  (str:concat
      "enum "
      (to-class-name (domaindsl.types:object-name ty)) " {" ctors "}"))

(defun render-class (ty)
  (str:concat
   "class "
   (to-class-name (domaindsl.types:object-name ty))
   " {}"))

(defun render (ty)
  (etypecase ty
    (domaindsl.types:data-type (render-type
                                ty
                                (reduce (lambda (acc ct)
                                          (str:concat acc (render-ctor ct) (string #\NEWLINE)))
                                        (domaindsl.types:object-constructors ty)
                                        :initial-value (string #\NEWLINE))))
    (domaindsl.types:class-reference (render-class ty))))

(defun file-for-artifact (name o)
  (domaindsl.artifact:make-artifact :name name
                                   :file name
                                   :content o))

(defmethod domaindsl.artifact:artifact-extension ((target (eql :swift)))
  ".swift")

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :swift)) (o domaindsl.types:data-type))
  (let ((class-name (to-class-name
                     (domaindsl.types:object-name o))))
    (list (file-for-artifact class-name o))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :swift)) (o domaindsl.types:class-reference))
  (let ((class-name (to-class-name
                     (domaindsl.types:object-name o))))
    (list (file-for-artifact class-name o))))

(defmethod domaindsl.artifact:compile-artifact
    ((target (eql :swift)) o)
  (render (domaindsl.artifact:artifact-content o)))
