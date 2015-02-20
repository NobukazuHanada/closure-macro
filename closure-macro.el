;; -*- coding: utf-8; lexical-binding: t -*-


(defmacro defclosure-object (name args export-funcs &rest body)
  (let* ((defun-objects (get-defun-object body))
         (letrec-expr (defun-object-to-letrec-expr defun-objects))
         (mpc-closure (create-message-passing-closure export-funcs))
         (remove-defun (remove-defun-forms body))
         (not-funcall-expr (append letrec-expr remove-defun `(,mpc-closure)))
         (expr (add-funcall not-funcall-expr defun-objects)))
    `(progn (defun ,name ,args ,expr)
            ,@(export-functions export-funcs))))

(defun export-functions (funcs)
  (create-msp-closure-call-funcs funcs))

(defmacro export-function (func)
  (create-msp-closure-call-func func))

(defun create-message-passing-closure (export-funcs)
  `(function (lambda (message &rest args)
               (cond ,@(mapcar (lambda (name)
                                 `((eq message ',name)
                                   (apply ,name args)))
                               export-funcs)))))

(defun create-msp-closure-call-funcs (export-funcs)
  (mapcar #'create-msp-closure-call-func export-funcs))

(defun create-msp-closure-call-func (export-func)
  `(defun ,export-func (obj &rest args)
     (apply obj (cons ',export-func args))))


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
    (replace-symbol-expr func-names exprs)))

(defun replace-symbol-exprs (defun-names exprs)
  (mapcar (lambda (expr)
            (replace-symbol-expr defun-names expr))
          exprs))


(defun replace-symbol-expr (defun-names expr)
  (pcase expr
    (`(lambda ,args . ,body)
     `(lambda ,args ,@(replace-symbol-exprs defun-names body)))    
    (`(let ,bindings . ,body)
     `(let ,(mapcar
             (lambda (bind)
               (let ((bind-var (car bind))
                     (bind-expr (cadr bind)))
                 `(,bind-var ,(replace-symbol-expr defun-names bind-expr))))
             bindings)
        ,@(replace-symbol-exprs defun-names body))) 
    (`(letrec ,bindings . ,body)
     `(letrec ,(mapcar
                (lambda (bind)
                  (let ((bind-var (car bind))
                        (bind-expr (cadr bind)))
                    `(,bind-var ,(replace-symbol-expr defun-names bind-expr))))
                    bindings)
        ,@(replace-symbol-exprs defun-names body)))
    (`(,func . ,args)
     (cond ((find func defun-names)
            `(funcall ,func
                      ,@(replace-symbol-exprs defun-names args)))
           ((listp func)
            `(,(replace-symbol-expr defun-names func)
              ,@(replace-symbol-exprs defun-names args)))
           (t
            `(,func
              ,@(replace-symbol-exprs defun-names args)))))
    (_ expr)))

(provide 'closure-macro)
