(defpackage #:domaindsl.kotlin
  (:use #:cl)
  (:export
   #:kotlin-render))

(in-package #:domaindsl.kotlin)

(defun to-file-name (s)
  (str:concat s ".kt"))

(defun to-class-name (n &optional as-array)
  (let ((name (str:pascal-case (symbol-name n))))
    (if as-array
        (str:concat "Array<" name ">")
        name)))

(defun to-variable-name (n)
  (str:camel-case (symbol-name n)))

(defun dt-var (x)
  (str:concat "val "
              (to-variable-name (domaindsl.types:ctor-arg-name x))
              ": "
              (to-class-name (domaindsl.types:ctor-arg-type x)
                             (domaindsl.types:ctor-arg-array x))))

(defun kotlin-render-ctor-args (ct)
  (if (null (domaindsl.types:dt-ctor-args ct))
      ""
      (str:concat "("
                  (destructuring-bind (first . rest)
                      (domaindsl.types:dt-ctor-args ct)
                    (reduce (lambda (acc x)
                              (str:concat acc ", " (dt-var x)))
                            rest
                            :initial-value (dt-var first)))
                  ")")))

(defun render-base-class (ty)
  (str:concat
   "open class "
   (to-class-name (domaindsl.types:dt-type-name ty))))

(defun render-data-class (ty ct)
  (let* ((args (domaindsl.types:dt-ctor-args ct))
         (has-args (< 0 (length args)))
         (class-tag (if has-args
                        "data class "
                        "class ")))
    (str:concat
     class-tag
     (to-class-name (domaindsl.types:dt-ctor-name ct))
     (kotlin-render-ctor-args ct)
     ": "
     (to-class-name (domaindsl.types:dt-type-name ty))
     "()")))

(defun render-type (ty)
  (render-base-class ty))

(defun render-class (ty)
  (str:concat "class "
              (to-class-name (domaindsl.types:dt-class-name ty))
              "{}"))

(defun render-ctor (c)
  (let* ((args (domaindsl.types:dt-ctor-args c))
         (has-args (< 0 (length args)))
         (class-tag (if has-args
                        "data class "
                        "class ")))
    (str:concat
     class-tag
     (to-class-name (domaindsl.types:dt-ctor-name c))
     (kotlin-render-ctor-args c)
     ": "
     (to-class-name (domaindsl.types:dt-ctor-of-class c))
     "()")))

(defun render (ty)
  (etypecase ty
    (domaindsl.types:dt-type (render-type ty))
    (domaindsl.types:dt-class (render-class ty))
    (domaindsl.types:dt-ctor (render-ctor ty))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :kotlin)) (o domaindsl.types:dt-type))
  (flet ((artifact (name obj)
           (domaindsl.artifact:make-artifact :name name
                                            :file (to-file-name (to-class-name name))
                                            :content obj)))
    (cons (artifact (domaindsl.types:dt-type-name o) o)
          (mapcar (lambda (c)
                    (artifact (domaindsl.types:dt-ctor-name c) c))
                  (domaindsl.types:dt-type-ctors o)))))

(defmethod domaindsl.artifact:generate-artifact
    ((target (eql :kotlin)) (o domaindsl.types:dt-class))
  (list (domaindsl.artifact:make-artifact :name (domaindsl.types:dt-class-name o)
                                         :file (to-file-name (to-class-name
                                                              (domaindsl.types:dt-class-name o)))
                                         :content o)))

(defmethod domaindsl.artifact:compile-artifact
    ((target (eql :kotlin)) o)
  (render (domaindsl.artifact:artifact-content o)))
