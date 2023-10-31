(defpackage #:domaindsl.kotlin
  (:use #:cl)
  (:import-from #:domaindsl.types
                #:object-to-class-name-string
                #:object-to-constructor-variable-name-string)
  (:export
   #:kotlin-render))

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

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :kotlin))
     (obj domaindsl.types:data-type))
  (str:pascal-case (symbol-name (domaindsl.types:object-name obj))))

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :kotlin))
     (obj domaindsl.types:type-constructor))
  (str:pascal-case (symbol-name (domaindsl.types:object-name obj))))

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :kotlin))
     (obj domaindsl.types:constructor-argument))
  (let* ((type-symbol (domaindsl.types:object-argument-type obj))
         (name (str:pascal-case (symbol-name type-symbol)))
         (is-array (domaindsl.types:object-argument-is-array obj)))
    (if as-array (str:concat "Array<" name ">") name)))

(defmethod domaindsl.types:object-to-constructor-name-string
    ((target (eql :kotlin))
     (obj domaindsl.types:constructor-argument))
  (str:camel-case (symbol-name (domaindsl.types:object-argument-type obj))))

(defmethod domaindsl.types:object-to-constructor-variable-name-string
    ((target (eql :kotlin))
     (obj domaindsl.types:constructor-argument))
  (str:camel-case (symbol-name (domaindsl.types:object-argument-type obj))))

(defun render-constructor-argument (arg)
  (str:concat "val "
              (object-to-constructor-variable-name-string arg)
              ": "
              (object-to-class-name-string arg)))

(defun render-constructor-arguments (ct)
  (let ((constructor-arguments (domaindsl.types:object-arguments ct)))
    (if (null constructor-arguments)
        ""
        (destructuring-bind (first . rest)
            constructor-arguments
          (reduce (lambda (acc x)
                    (str:concat acc ", " (render-constructor-argument x)))
                  rest
                  :initial-value (render-constructor-argument first))))))

(defun render-base-class (ty)
  (str:concat
   "open class "
   (object-to-class-name-string ty)))

(defun render-data-class (ty ct)
  (let* ((args (domaindsl.types:object-arguments ct))
         (has-args (< 0 (length args)))
         (class-tag (if has-args "data class " "class ")))
    (str:concat
     class-tag
     (object-to-class-name-string ty)
     "("
     (render-constructor-arguments ct)
     ")"
     ": "
     (object-to-class-name-string ty)
     "()")))

(defun render-type (ty)
  (render-base-class ty))

(defun render-class (ty)
  (str:concat "class "
              (to-class-name (domaindsl.types:object-name ty))
              "{}"))

(defun render-constructor (c)
  (let* ((args (domaindsl.types:object-arguments c))
         (has-args (< 0 (length args)))
         (class-tag (if has-args "data class " "class ")))
    (str:concat
     class-tag
     (to-class-name (domaindsl.types:object-name c))
     (kotlin-render-ctor-args c)
     ": "
     (to-class-name (domaindsl.types:object-of-class c))
     "()")))

(defun render (ty)
  (etypecase ty
    (domaindsl.types:data-type (render-type ty))
    (domaindsl.types:class-reference (render-class ty))
    (domaindsl.types:type-constructor (render-constructor ty))))

(defmethod domaindsl.artifact:artifact-extension ((target (eql :kotlin)))
  ".kt")

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :kotlin))
     (o domaindsl.types:data-type))
  (flet ((artifact (name obj)
           (domaindsl.artifact:make-artifact
            :name name
            :file (str:concat
                   (object-to-class-name-string target obj)
                   (domaindsl.artifact:artifact-extension target))
            :content obj)))
    (cons (artifact (domaindsl.types:object-name o) o)
          (mapcar (lambda (c)
                    (artifact (domaindsl.types:object-name c) c))
                  (domaindsl.types:object-constructors o)))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :kotlin))
     (o domaindsl.types:class-reference))
  (list (domaindsl.artifact:make-artifact
         :name (domaindsl.types:object-name o)
         :file (to-file-name (object-to-class-name-string target o))
         :content o)))

(defmethod domaindsl.artifact:compile-artifact
    ((target (eql :kotlin)) o)
  (render (domaindsl.artifact:artifact-content o)))
