(defpackage #:domaindsl.kotlin
  (:use #:cl)
  (:import-from #:domaindsl.types
                #:object-argument-is-array
                #:object-argument-type
                #:object-to-constructor-name-string
                #:constructor-argument
                #:object-arguments
                #:object-of-class
                #:object-constructors
                #:object-name
                #:type-constructor
                #:class-reference
                #:data-type
                #:object-to-class-name-string
                #:object-to-constructor-variable-name-string)
  (:import-from #:domaindsl.render
                #:render-object)
  (:import-from #:domaindsl.artifact
                #:artifact-content
                #:make-artifact
                #:compile-artifact
                #:generate-artifact
                #:artifact-extension))

(in-package #:domaindsl.kotlin)

;; class ClassName {
;;    public val FieldName;
;; }

;; data-type
;;
;; open class BaseClassName;
;;
;; - constructor w/o arguments
;;
;; class ClassName : BaseClassName()
;;
;; - constructor with arguments
;;
;; data class ClassName(ConstructorArgument,...) : BaseClassName()

;;; ------

(defun symbol-to-pascal-case (s)
  (str:pascal-case (symbol-name s)))

(defmethod object-to-class-name-string ((target (eql :kotlin)) (obj data-type))
  (symbol-to-pascal-case (object-name obj)))

(defmethod object-to-class-name-string ((target (eql :kotlin)) (obj type-constructor))
  (symbol-to-pascal-case (object-name obj)))

(defmethod object-to-class-name-string ((target (eql :kotlin)) (obj constructor-argument))
  (let* ((type-symbol (object-argument-type obj))
         (name (symbol-to-pascal-case type-symbol))
         (is-array (object-argument-is-array obj)))
    (if is-array (str:concat "Array<" name ">") name)))

(defmethod object-to-constructor-name-string ((target (eql :kotlin)) (obj constructor-argument))
  (str:camel-case (symbol-name (object-argument-type obj))))

(defmethod object-to-constructor-variable-name-string ((target (eql :kotlin)) (obj constructor-argument))
  (str:camel-case (symbol-name (object-argument-type obj))))

(defmethod render-object ((target (eql :kotlin)) (o constructor-argument))
  (str:concat "val "
              (object-to-constructor-variable-name-string target o)
              ": "
              (object-to-class-name-string target o)))

(defmethod render-object ((target (eql :kotlin)) (o type-constructor))
  (let* ((args (object-arguments o))
         (has-args (< 0 (length args)))
         (class-tag (if has-args "data class " "class ")))
    (str:concat
     class-tag
     (object-to-class-name-string target o)
     (let ((constructor-arguments (domaindsl.types:object-arguments o)))
       (if (null constructor-arguments)
           ""
           (str:concat
            "("
            (destructuring-bind (first . rest)
                constructor-arguments
              (reduce (lambda (acc x)
                        (str:concat acc ", " (render-object target x)))
                      rest
                      :initial-value (render-object target first)))
            ")")))
     ": "
     (symbol-to-pascal-case (object-of-class o))
     "()")))

(defmethod render-object ((target (eql :kotlin)) (o data-type))
  (str:concat
   "open class "
   (object-to-class-name-string target o)))

(defmethod render-object ((target (eql :kotlin)) (o class-reference))
  (str:concat "class "
              (object-to-class-name-string target o)
              "{}"))

(defmethod artifact-extension ((target (eql :kotlin)))
  ".kt")

(defmethod generate-artifact ((target (eql :kotlin)) (o data-type))
  (flet ((artifact (name obj)
           (make-artifact
            :name name
            :file (str:concat
                   (object-to-class-name-string target obj)
                   (artifact-extension target))
            :content obj)))
    (cons (artifact (object-name o) o)
          (mapcar (lambda (c) (artifact (object-name c) c)) (object-constructors o)))))

(defmethod generate-artifact ((target (eql :kotlin)) (o class-reference))
  (list (make-artifact
         :name (object-name o)
         :file (object-to-class-name-string target o)
         :content o)))

(defmethod compile-artifact ((target (eql :kotlin)) o)
  (render-object target (artifact-content o)))
