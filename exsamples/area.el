;; -*- coding: utf-8; lexical-binding: t -*-

(defclosure-module rect (w h)
  (area)
  (defun area () (* w h)))

(defclosure-module circle (r)
  (area)
  (defun area () (* r r pi)))

(setq r (rect 100 200))
(setq c (circle 100))
(print (area r))
(print (area c))
