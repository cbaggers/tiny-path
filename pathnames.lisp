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
  (destructuring-bind (flag . parts) (pathname-directory path)
    (let ((nodes (if (eq flag :absolute)
                     (list (%make-tiny-path-root-dir))
                     (list))))
      (loop :for part :in parts :do
         (push (%make-tiny-path-dir :name part) nodes))
      (when (pathname-name path)
        (push (%make-tiny-path-file
               :name (format nil "~a~@[.~a~]" (pathname-name path)
                             (pathname-type path)))
              nodes))
      (make-path-from-nodes nodes))))

;;------------------------------------------------------------
;; Helper functions that just call down to uiop and wrap
;; results as tpaths

(defun tpath-dirs (path)
  (let* ((pname (etypecase path
                  (tpath (tpath-to-pathname path))
                  (string (tpath-to-pathname (make-tpath path)))
                  (pathname path)))
         (head (list nil))
         (tail head))
    (flet ((keep (x)
             (let ((new (cons (tpath-from-pathname x) nil)))
               (setf (cdr tail) new
                     tail new))))
      (uiop:collect-sub*directories pname t t #'keep)
      (cddr head))))

(defun tpath-files (path)
  (let ((pname (etypecase path
                 (tpath (tpath-to-pathname path))
                 (string (tpath-to-pathname (make-tpath path)))
                 (pathname path))))
    (mapcar #'tpath-from-pathname (uiop:directory-files pname))))
