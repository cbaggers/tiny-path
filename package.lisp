;;;; package.lisp

(uiop:define-package #:tiny-path.internals
    (:use #:cl #:uiop)
  (:export :make-path
           :path+
           :path-pop
           :path-push
           :render-path))

(uiop:define-package #:tiny-path
    (:use #:tiny-path.internals)
  (:export :make-path
           :path+
           :path-pop
           :path-push
           :render-path))
