(in-package :tiny-path.internals)

;;------------------------------------------------------------

;; node - file or directory
;; file - a terminal node
;; dir - a non-terminal node

;;------------------------------------------------------------

(defvar *in-print-path* nil)
(defvar *in-render-path* nil)

;;------------------------------------------------------------

(defclass path ()
  ;; nodes are stored in reverse order
  ((nodes :initform nil :initarg :nodes)
   (absolute :initarg :absolute :initform nil :reader absolute-path-p)
   (len :initarg :len :reader path-length)))

(defclass node ()
  ((name :initarg :name :reader node-name)
   (terminal :reader terminal-node-p)))

(defclass file (node)
  ((terminal :initform t)))

(defclass dir (node)
  ((terminal :initform nil)
   (seperator :initform "/")))

(defclass root-dir (dir)
  ((terminal :initform nil)
   (absolute-prefix :initform "/")))

;;------------------------------------------------------------

(defmethod print-object ((dir dir) stream)
  (with-slots (name seperator) dir
    (if *in-print-path*
        (format stream "~a~a" name
                (if *in-render-path*
                    seperator
                    "/"))
        (format stream "#<DIR ~s>" name))))

(defmethod print-object ((dir root-dir) stream)
  (with-slots (name seperator absolute-prefix) dir
    (if *in-print-path*
        (format stream "~a~a~a" name
                (if *in-render-path*
                    absolute-prefix
                    "/")
                (if *in-render-path*
                    seperator
                    "/"))
        (format stream "#<DIR ~s>" name))))

(defmethod print-object ((file file) stream)
  (with-slots (name) file
    (if *in-print-path*
        (format stream name)
        (format stream "#<FILE ~s>" name))))

(defmethod print-object ((path path) stream)
  (with-slots (nodes) path
    (let ((*in-print-path* t))
      (format stream "#<PATH \"~{~s~}\">" (reverse nodes)))))

(defun render-path (path &optional stream)
  (with-slots (nodes) path
    (let ((*in-print-path* t)
          (*in-render-path* t))
      (format stream "~{~s~}" (reverse nodes)))))

;;------------------------------------------------------------

(defun make-path (str &optional dir)
  (let ((split (split-string str :separator '(#\/))))
    (assert split () "Empty relative paths are invalid")
    (when (and (emptyp (first split)) (> (length split) 1))
      (error "Cannot create an absolute path with #'make-path"))
    (let* ((sub-strs (reverse split))
           (sub-strs (if (emptyp (first sub-strs))
                         (if dir
                             (rest sub-strs)
                             (error "invalid trailing seperator"))
                         sub-strs))
           (nodes (mapcar (lambda (x) (make-instance 'dir :name x))
                          (rest sub-strs)))
           (nodes (cons (if dir
                            (make-instance 'dir :name (first sub-strs))
                            (make-instance 'file :name (first sub-strs)))
                        nodes)))
      (make-instance 'path :nodes nodes
                     :absolute (typep (car (last nodes)) 'root-dir)
                     :len (length nodes)))))

;;------------------------------------------------------------

(defun path+ (a b)
  (with-slots ((nodes-a nodes)
               (len-a len)) a
    (assert (not (terminal-node-p (first nodes-a))))
    (with-slots ((nodes-b nodes)
                 (len-b len)) b
      (make-instance 'path :nodes (append nodes-b nodes-a)
                     :absolute (absolute-path-p a)
                     :len (+ len-a len-b)))))

(defun path-pop (path)
  (with-slots (nodes len) path
    (values (make-instance 'path :nodes (rest nodes)
                           :absolute (absolute-path-p path)
                           :len (- len 1))
            (first nodes))))

(defun path-push (name path &optional dir)
  (path+ path (make-path name dir)))

;;------------------------------------------------------------
