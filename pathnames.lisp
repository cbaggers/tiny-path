(in-package :tiny-path.internals)

;; Functions for moving between pathnames & tiny-paths

;;------------------------------------------------------------

(defun tpath-to-pathname (path)
  (let* ((str (tpath-render path))
         (pn (pathname str)))
    (if (tpath-file-path-p path)
        pn
        (ensure-directory-pathname pn))))

(defun tpath-from-pathname (path)
  (error "IMPLEMENT ME! ~a" path))

;;------------------------------------------------------------
