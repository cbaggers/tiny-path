;;;; package.lisp

(uiop:define-package #:tiny-path.internals
    (:use #:cl #:uiop)
  (:export :tiny-path
           :make-tpath
           :tpath+
           :tpath-pop
           :tpath-push
           :tpath-render
           :tpath-file-path-p
           :tpath-to-pathname
           :tpath-to-system))

(uiop:define-package #:tiny-path
    (:use #:cl #:tiny-path.internals)
  (:export :tiny-path
           :make-tpath
           :tpath+
           :tpath-pop
           :tpath-push
           :tpath-render
           :tpath-file-path-p
           :tpath-to-pathname
           :tpath-to-system))
