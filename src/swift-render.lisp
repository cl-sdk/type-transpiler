(defpackage #:domaindsl.swift
  (:use #:cl)
  (:import-from #:domaindsl.types
                #:class-reference
                #:object-argument-is-array
                #:object-argument-type
                #:object-name
                #:data-type
                #:object-to-constructor-name-string
                #:object-arguments
                #:constructor-argument
                #:type-constructor
                #:object-constructors
                #:object-to-class-name-string
                #:object-to-constructor-variable-name-string)
  (:import-from #:domaindsl.render
                #:render-object)
  (:import-from #:domaindsl.artifact
                #:artifact-content
                #:compile-artifact
                #:generate-artifact
                #:artifact-extension)
  (:export
   #:swift-render
   #:render-type
   #:render-class
   #:render
   #:to-class-name
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

(defun file-for-artifact (name o)
  (domaindsl.artifact:make-artifact :name name
                                   :file name
                                   :content o))

(defmethod object-to-class-name-string ((target (eql :swift)) (obj data-type))
  (str:pascal-case (symbol-name (object-name obj))))

(defmethod object-to-class-name-string ((target (eql :swift)) (obj type-constructor))
  (str:camel-case (symbol-name (object-name obj))))

(defmethod object-to-class-name-string ((target (eql :swift)) (obj constructor-argument))
  (let* ((type-symbol (object-argument-type obj))
         (name (str:pascal-case (symbol-name type-symbol)))
         (is-array (object-argument-is-array obj)))
    (if is-array (str:concat "[" name "]") name)))

(defmethod object-to-constructor-name-string ((target (eql :swift)) (obj constructor-argument))
  (str:camel-case (symbol-name (object-argument-type obj))))

(defmethod object-to-constructor-variable-name-string ((target (eql :swift)) (obj constructor-argument))
  (object-to-class-name-string target obj))

(defmethod render-object ((target (eql :swift)) (o constructor-argument))
  (object-to-constructor-variable-name-string :swift x))

(defmethod render-object ((target (eql :swift)) (o type-constructor))
  (str:concat "  case "
              (object-to-class-name-string target o)
              (if (null args)
                  ""
                  (str:concat "("
                              (destructuring-bind (first . rest)
                                  args
                                (reduce (lambda (acc x)
                                          (str:concat acc ", " (render-object target x)))
                                        rest
                                        :initial-value (render-object target x)))
                              ")"))

              " {}"))

(defmethod render-object ((target (eql :swift)) (o data-type))
  (str:concat
   "enum "
   (object-to-class-name-string :swift o)
   " {"
   (reduce (lambda (acc ct)
             (str:concat acc (render-object target ct) (string #\NEWLINE)))
           (object-constructors o)
           :initial-value (string #\NEWLINE))
   "}"))

(defmethod render-object ((target (eql :swift)) (o class-reference))
  (str:concat "class " (object-to-class-name-string target o) " {}"))

(defmethod artifact-extension ((target (eql :swift)))
  ".swift")

(defmethod generate-artifact ((target (eql :swift)) (o data-type))
  (let ((class-name (object-to-class-name-string target o)))
    (list (file-for-artifact class-name o))))

(defmethod generate-artifact ((target (eql :swift)) (o class-reference))
  (let ((class-name (object-to-class-name-string target o)))
    (list (file-for-artifact class-name o))))

(defmethod compile-artifact ((target (eql :swift)) o)
  (render-object target (artifact-content o)))
