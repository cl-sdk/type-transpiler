(defpackage #:domaindsl.javascript
  (:use #:cl))

(defun symbol-to-pascal-case (s)
  (str:pascal-case (symbol-name s)))

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :javascript))
     (obj domaindsl.types:data-type))
  (symbol-to-pascal-case (domaindsl.types:object-name obj)))

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :javascript))
     (obj domaindsl.types:type-constructor))
  (symbol-to-pascal-case (domaindsl.types:object-name obj)))

(defmethod domaindsl.types:object-to-class-name-string
    ((target (eql :javascript))
     (obj domaindsl.types:constructor-argument))
  (let* ((type-symbol (domaindsl.types:object-argument-type obj))
         (name (symbol-to-pascal-case type-symbol))
         (is-array (domaindsl.types:object-argument-is-array obj)))
    (if is-array (str:concat name) name)))

(defmethod domaindsl.types:object-to-constructor-name-string
    ((target (eql :javascript))
     (obj domaindsl.types:constructor-argument))
  (str:camel-case (symbol-name (domaindsl.types:object-name obj))))

(defmethod domaindsl.types:object-to-constructor-variable-name-string
    ((target (eql :javascript))
     (obj domaindsl.types:constructor-argument))
  (str:camel-case (symbol-name (domaindsl.types:object-name obj))))

(defun render-function-arguments (target ty)
  (etypecase ty
    (domaindsl.types:data-type "()")
    (t (let ((args (domaindsl.types:object-arguments ty)))
         (if (null args)
             ""
             (str:concat
              "("
              (destructuring-bind (first . rest)
                  args
                (reduce (lambda (acc x)
                          (str:concat acc ", " (domaindsl.types:object-to-constructor-variable-name-string target x)))
                        rest
                        :initial-value (domaindsl.types:object-to-constructor-variable-name-string target first)))
              ")"))))))

(defun render-function (target ty)
  (str:concat
   "export function "
   (domaindsl.types:object-to-class-name-string target ty)
   (render-function-arguments target ty)
   " {}"))

(defun render-evaluated-function (target ty)
  (let ((name (domaindsl.types:object-to-class-name-string target ty)))
   (str:concat
    "export const "
    name
    " = new (function "
    (domaindsl.types:object-to-class-name-string target ty)
    "() {})")))

(defun render-type (target ty)
  (render-function target ty))

(defun render-constructor (target ty)
  (if (null (domaindsl.types:object-arguments ty))
      (render-evaluated-function target ty)
      (render-function target ty)))

(defun render (target ty)
  (etypecase ty
    (domaindsl.types:data-type (render-type target ty))
    (domaindsl.types:class-reference (render-class target ty))
    (domaindsl.types:type-constructor (render-constructor target ty))))

(defmethod domaindsl.artifact:artifact-extension ((target (eql :javascript)))
  ".js")

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :javascript))
     (o domaindsl.types:data-type))
  (flet ((artifact (name obj)
           (domaindsl.artifact:make-artifact
            :name name
            :file (str:concat
                   (domaindsl.types:object-to-class-name-string target obj)
                   (domaindsl.artifact:artifact-extension target))
            :content obj)))
    (cons (artifact (domaindsl.types:object-name o) o)
          (mapcar (lambda (c)
                    (artifact (domaindsl.types:object-name c) c))
                  (domaindsl.types:object-constructors o)))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :javascript))
     (o domaindsl.types:class-reference))
  (list (domaindsl.artifact:make-artifact
         :name (domaindsl.types:object-name o)
         :file (to-file-name (domaindsl.types:object-to-class-name-string target o))
         :content o)))

(defmethod domaindsl.artifact:compile-artifact
    ((target (eql :javascript)) o)
  (render target (domaindsl.artifact:artifact-content o)))
