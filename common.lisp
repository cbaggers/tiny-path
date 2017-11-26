(in-package :tiny-path.internals)

;; Functions that provided features common across OSs

;;------------------------------------------------------------

#+windows
(defun tpath-root (&optional name)
  (let ((name (or name "")))
    (assert (not (emptyp name)) ()
            "On windows the root dir is a named drive, please provide the name")
    (make-path-from-nodes
     (list (%make-tiny-path-root-dir :name name)))))

#-windows
(defun tpath-root (&optional name)
  (let ((name (or name "")))
    (assert (emptyp name) ()
            "No named root directories know for this OS")
    (make-path-from-nodes
     (list (%make-tiny-path-root-dir :name name)))))

;;------------------------------------------------------------

(defun tpath-to-system (name)
  (tpath-from-pathname
   (uiop:ensure-directory-pathname
     (asdf:system-relative-pathname name ""))))
