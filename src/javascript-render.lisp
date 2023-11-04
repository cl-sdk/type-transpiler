(defpackage #:domaindsl.javascript
  (:use #:cl)
  (:import-from #:domaindsl.types
                #:class-reference
                #:object-constructors
                #:object-arguments
                #:object-to-constructor-variable-name-string
                #:object-to-constructor-name-string
                #:object-argument-is-array
                #:object-argument-type
                #:constructor-argument
                #:type-constructor
                #:object-name
                #:data-type
                #:object-to-class-name-string)
  (:import-from #:domaindsl.render
                #:render-object)
  (:import-from #:domaindsl.artifact
                #:artifact-content
                #:compile-artifact
                #:make-artifact
                #:generate-artifact
                #:artifact-extension))

(in-package #:domaindsl.javascript)

(defun symbol-to-pascal-case (s)
  (str:pascal-case (symbol-name s)))

(defmethod object-to-class-name-string ((target (eql :javascript)) (obj data-type))
  (symbol-to-pascal-case (object-name obj)))

(defmethod object-to-class-name-string ((target (eql :javascript)) (obj type-constructor))
  (symbol-to-pascal-case (object-name obj)))

(defmethod object-to-class-name-string ((target (eql :javascript)) (obj constructor-argument))
  (let* ((type-symbol (object-argument-type obj))
         (name (symbol-to-pascal-case type-symbol))
         (is-array (object-argument-is-array obj)))
    (if is-array (str:concat name) name)))

(defmethod object-to-class-name-string ((target (eql :javascript)) (obj class-reference))
  (symbol-to-pascal-case (object-name obj)))

(defmethod object-to-constructor-name-string ((target (eql :javascript)) (obj constructor-argument))
  (str:camel-case (symbol-name (object-name obj))))

(defmethod object-to-constructor-variable-name-string ((target (eql :javascript)) (obj constructor-argument))
  (str:camel-case (symbol-name (object-name obj))))

(defun render-function-arguments (target ty)
  (etypecase ty
    (data-type "()")
    (t (let ((args (object-arguments ty)))
         (if (null args)
             ""
             (str:concat
              "("
              (destructuring-bind (first . rest)
                  args
                (reduce (lambda (acc x)
                          (str:concat acc ", " (render-object target x)))
                        rest
                        :initial-value (render-object target first)))
              ")"))))))

(defun render-function-assignment (target ty)
  (let ((name (object-to-constructor-variable-name-string target ty)))
    (str:concat "  this." name " = " name ";")))

(defun render-function-assignments (target ty)
  (etypecase ty
    (data-type "")
    (t (let ((args (object-arguments ty)))
         (if (null args)
             ""
             (destructuring-bind (first . rest)
                 args
               (reduce (lambda (acc x)
                         (str:concat acc (string #\NEWLINE)
                                     (render-function-assignment target x)))
                       rest
                       :initial-value (render-function-assignment target first))))))))

(defun render-constructor-helper (target ty)
  (str:concat 
   "  if (!(this instanceof "
   (object-to-class-name-string target ty)
   ")) { return new "
   (object-to-class-name-string target ty)
   (render-function-arguments target ty)
   "; }"))

(defun render-function (target ty)
  (let ((is-data-type (equal 'domaindsl.types:data-type (type-of ty))))
    (str:concat
     "export function "
     (object-to-class-name-string target ty)
     (render-function-arguments target ty)
     " {"
     (if is-data-type
         ""
         (str:concat (string #\NEWLINE)
                     (render-constructor-helper target ty)
                     (string #\NEWLINE)))
     (if is-data-type
         ""
         (str:concat (string #\NEWLINE)
                     (render-function-assignments target ty)
                     (string #\NEWLINE)))
     "}")))

(defun render-evaluated-function (target ty)
  (let ((name (domaindsl.types:object-to-class-name-string target ty)))
   (str:concat
    "export const "
    name
    " = new (function "
    (domaindsl.types:object-to-class-name-string target ty)
    "() {})")))

(defmethod render-object ((target (eql :javascript)) (o constructor-argument))
  (object-to-constructor-variable-name-string target o))

(defmethod render-object ((target (eql :javascript)) (o type-constructor))
  (if (null (object-arguments o))
      (render-evaluated-function target o)
      (render-function target o)))

(defmethod render-object ((target (eql :javascript)) (o data-type))
  (render-function target o))

(defmethod render-object ((target (eql :javascript)) (o class-reference))
  (render-function target o))

(defmethod artifact-extension ((target (eql :javascript)))
  ".js")

(defmethod generate-artifact ((target (eql :javascript)) (o data-type))
  (flet ((artifact (name obj)
           (make-artifact
            :name name
            :file (str:concat
                   (object-to-class-name-string target obj)
                   (artifact-extension target))
            :content obj)))
    (cons (artifact (object-name o) o)
          (mapcar (lambda (c)
                    (artifact (object-name c) c))
                  (object-constructors o)))))

(defmethod generate-artifact ((target (eql :javascript)) (o class-reference))
  (list (make-artifact
         :name (object-name o)
         :file (object-to-class-name-string target o)
         :content o)))

(defmethod compile-artifact ((target (eql :javascript)) o)
  (render-object target (artifact-content o)))
