;;;; package.lisp

(uiop:define-package #:tiny-path.internals
    (:use #:cl #:uiop)
  (:export :tpath
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
  (:export :tpath
           :make-tpath
           :tpath+
           :tpath-pop
           :tpath-push
           :tpath-render
           :tpath-file-path-p
           :tpath-to-pathname
           :tpath-to-system))
