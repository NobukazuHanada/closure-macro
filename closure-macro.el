(defmacro defclosure-object (name args &rest body)
  (let* ((defun-objects (get-defun-object body))
         (letrec-expr (defun-object-to-letrec-expr defun-objects))
         (not-funcall-expr (cons letrec-expr (remove-defun-forms body)))
         (expr (add-funcall not-funcall-expr defun-objects)))
    `(defun ,name ,args
       ,@expr)))

(defstruct defun-object
  name
  args
  body)


(defun get-defun-object (body)
  (remove-if #'null (mapcar (lambda (expr)
                              (if (defun-form-p expr)
                                  (defun-form-to-object expr)))
                            body)))

(defun defun-form-to-object (form)
  (let ((name (cadr form))
        (args (caddr form))
        (body (cdddr form)))
    (make-defun-object
     :name name
     :args args
     :body body)))

(defun defun-object-to-letrec-expr (forms)
  (if (null forms)
      nil
      `(letrec
           ,(make-letrec-bindings forms))))

(defun make-letrec-bindings (forms)
  (mapcar (lambda (form)
            `(,(defun-object-name form) (lambda ,(defun-object-args form)
                                          ,@(defun-object-body form))))
          forms))

(defun defun-form-p (form)
  (and (listp form) (eq (car form) 'defun)))

(defun remove-defun-forms (body)
  (remove-if #'defun-form-p body))

(defun add-funcall (exprs defun-objects)
  (let ((func-names (mapcar #'defun-object-name defun-objects)))
    (mapcar (lambda (expr)
              (if (and (listp expr) (find (car expr) func-names))
                  (cons 'funcall expr)
                expr))
            exprs)))

(provide 'closure-macro)
