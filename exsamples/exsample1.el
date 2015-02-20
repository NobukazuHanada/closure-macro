;; -*- coding: utf-8; lexical-binding: t -*-

(defclosure-module rect (x y width height)
  (area)
  (defun area () (* width height)))
