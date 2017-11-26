(in-package :tiny-path.internals)

;; Functiosn for moving between pathnames & tiny-paths

;;------------------------------------------------------------

(defun tiny-path-to-pathname (path)
  (let* ((str (render-path path))
         (pn (pathname str)))
    (if (file-path-p path)
        pn
        (ensure-directory-pathname pn))))

;;------------------------------------------------------------
