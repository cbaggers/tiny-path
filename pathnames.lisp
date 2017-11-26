(in-package :tiny-path.internals)



;;------------------------------------------------------------
;; Functions for moving between pathnames & tiny-paths

(defun tpath-to-pathname (path)
  (let* ((str (tpath-render path))
         (pn (pathname str)))
    (if (tpath-file-path-p path)
        pn
        (ensure-directory-pathname pn))))

(defun tpath-from-pathname (path)
  (error "IMPLEMENT ME! ~a" path)
  (tpath "foo"))

;;------------------------------------------------------------
;; Helper functiosn that just call down to uiop and wrap
;; results as tpaths

(defun tpath-dirs (path &optional (recursive t))
  (let ((pname (etypecase path
                 (tpath (tpath-to-pathname path))
                 (string (tpath-to-pathname (make-tpath path t)))
                 (pathname path))))
    (uiop:collect-sub*directories pname t recursive #'identity)))
