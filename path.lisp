(in-package :tiny-path.internals)

;;------------------------------------------------------------

;; node - file or directory
;; file - a terminal node
;; dir - a non-terminal node

;;------------------------------------------------------------

(defvar *in-print-path* nil)
(defvar *in-render-path* nil)

;;------------------------------------------------------------

(deftype tiny-path-index ()
  '(unsigned-byte 16))

(defstruct (tiny-path
             (:constructor %make-tiny-path))
  (nodes nil :type list :read-only t)
  ;; these 2 slots are an optimization so we can avoid list walking
  (absolute nil :type boolean :read-only t)
  (length 0 :type tiny-path-index :read-only t))

;; (defclass path ()
;;   ;; nodes are stored in reverse order
;;   ((nodes :initform nil :initarg :nodes)
;;    (absolute :initarg :absolute :initform nil :reader absolute-path-p)
;;    (len :initarg :len :reader path-length)))

(defstruct tiny-path-node
  (name nil :type symbol :read-only t)
  (terminal nil :type boolean :read-only t))

;; (defclass node ()
;;   ((name :initarg :name :reader node-name)
;;    (terminal :reader terminal-node-p)))

(defstruct (tiny-path-file
             (:include tiny-path-node
                       (terminal t :type boolean :read-only t))))

;; (defclass file (node)
;;   ((terminal :initform t)))

(defstruct (tiny-path-dir
             (:include tiny-path-node
                       (terminal nil :type boolean :read-only t)))
  (seperator "/" :type string :read-only t))

;; (defclass dir (node)
;;   ((terminal :initform nil)
;;    (seperator :initform "/")))

(defstruct (tiny-path-root-dir
             (:include tiny-path-node
                       (terminal nil :type boolean :read-only t)))
  (absolute-prefix "/" :type string :read-only t))

;; (defclass root-dir (dir)
;;   ((terminal :initform nil)
;;    (absolute-prefix :initform "/")))

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

(defun make-path-from-nodes (nodes)
  ;; What people extending tiny path will call from their constructors
  (assert (not-some #'root-dir-p (butlast nodes)) ()
          "Only the last node in the node list may be a root-dir")
  (make-instance 'path :nodes nodes
                 :absolute (typep (car (last nodes)) 'root-dir)
                 :len (length nodes)))

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
      (make-path-from-nodes nodes))))

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

(defun path-pop (path &optional (n 1))
  (assert (>= n 1) () "Can't pop less that 1 node from path")
  (with-slots (nodes len) path
    (values (make-instance 'path :nodes (nthcdr n nodes)
                           :absolute (absolute-path-p path)
                           :len (- len n))
            (subseq nodes 0 n))))

(defun path-push (name path &optional dir)
  (path+ path (make-path name dir)))

;;------------------------------------------------------------
