;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ert)
(require 'ac-hxc)

(ert-deftest test-get-defun-object ()
  (should (eq (get-defun-object '()) ()))
  (should (equal (get-defun-object '((defun func-name (arg1 arg2)
                                       (expr1)
                                       (expr2))))
                 `(,(make-defun-object
                     :name 'func-name
                     :args '(arg1 arg2)
                     :body '((expr1)
                             (expr2))))))
  (should (equal (get-defun-object '((defun func-name1 (arg1 arg2)
                                      (expr1)
                                      (expr2))
                                    (func-name1)
                                    (defun func-name2 ()
                                      ())))
                 `(,(make-defun-object
                     :name 'func-name1
                     :args '(arg1 arg2)
                     :body '((expr1)
                             (expr2)))
                   ,(make-defun-object
                     :name 'func-name2
                     :args '()
                     :body '(()))))))

(ert-deftest test-defun-object-to-letrec-expr ()
  (should (equal (defun-object-to-letrec-expr ()) ()))
  (should (equal (defun-object-to-letrec-expr `(,(make-defun-object
                                                  :name 'func-name
                                                  :args '(arg1 arg2)
                                                  :body '((expr1)
                                                          (expr2)))))
                 '(letrec ((func-name (lambda (arg1 arg2)
                                        (expr1)
                                        (expr2)))))))
  (should (equal (defun-object-to-letrec-expr
                   `(,(make-defun-object
                       :name 'func-name1
                       :args '(arg1 arg2)
                       :body '((expr1)
                               (expr2)))
                     ,(make-defun-object
                       :name 'func-name2
                       :args '()
                       :body '(()))))
                 '(letrec ((func-name1 (lambda (arg1 arg2)
                                         (expr1)
                                         (expr2)))
                           (func-name2 (lambda ()
                                         ())))))))

(ert-deftest test-defun-form-p ()
  (should (not (defun-form-p ())))
  (should (defun-form-p '(defun sample (args) body))))

(ert-deftest test-remove-defun-forms ()
  (should (eq (remove-defun-forms '()) ()))
  (should (eq (remove-defun-forms '((defun name args body)))
              ()))
  (should (equal (remove-defun-forms '(expr1
                                       expr2
                                       (expr3)
                                       (defun sample args sample)))
                 '(expr1 expr2 (expr3)))))

(ert-deftest test-add-funcall ()
  (should (eq (add-funcall '() '()) '()))
  (should (equal (add-funcall '((func arg1 arg2)) '())
                 '((func arg1 arg2))))
  (should (let ((func-obj (make-defun-object
                           :name 'func)))
            (equal (add-funcall '((func arg1 arg2)) `(,func-obj))
                   '((funcall func arg1 arg2)))))
  (let ((func1 (make-defun-object
                    :name 'func1))
        (func2 (make-defun-object
                    :name 'func2))
        (func3 (make-defun-object
                    :name 'func3)))
    (should (equal (add-funcall
                    '((letrec ((func1 (lambda () (func2 10))))
                        (func3)))
                    `(,func1 ,func2 ,func3))
                   '((letrec ((func1 (lambda () (funcall func2 10))))
                       (funcall func3)))))))

(ert-deftest test-replace-symbol-expr ()
  (should (eq (replace-symbol-expr '() '())
              '()))
  (should (equal (replace-symbol-expr '() '(+ 1 2))
                 '(+ 1 2)))
  (should (equal (replace-symbol-expr '(func) '(func arg1 arg2))
                 '(funcall func arg1 arg2)))
  (should (equal (replace-symbol-expr '(func1 func2)
                                      '(func1 (func2 arg1 arg2) arg3))
                 '(funcall func1 (funcall func2 arg1 arg2) arg3)))
  (should (equal (replace-symbol-expr
                  '(func)
                  '(let ((func (lambda () (+ 1 2))))
                                         (func)))
                 '(let ((func (lambda () (+ 1 2))))
                    (funcall func))))
  (should (equal (replace-symbol-expr
                  '(func)
                  '(letrec ((func (lambda () (+ 1 2))))
                     (func)))
                 '(letrec ((func (lambda () (+ 1 2))))
                    (funcall func))))
  (should (equal (replace-symbol-expr
                  '(func)
                  '(progn (func)
                          (func)
                          end))
                 '(progn
                    (funcall func)
                    (funcall func)
                    end))))

