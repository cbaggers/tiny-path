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

;;------------------------------------------------------------

(defstruct (tpath
             (:constructor %make-tiny-path))
  (nodes nil :type list :read-only t)
  ;; these 2 slots are an optimization so we can avoid list walking
  (absolute-p nil :type boolean :read-only t)
  (length 0 :type tiny-path-index :read-only t))


(defstruct tpath-node
  (name nil :type string :read-only t)
  (terminal-p nil :type boolean :read-only t))


(defstruct (tpath-file
             (:constructor %make-tiny-path-file)
             (:include tpath-node
                       (terminal-p t :type boolean :read-only t))))


(defstruct (tpath-dir
             (:constructor %make-tiny-path-dir)
             (:include tpath-node
                       (terminal-p nil :type boolean :read-only t)))
  (seperator "/" :type string :read-only t))

(defun %make-tpath-dir (name)
  (assert (not (emptyp name)) ()
          "Non root name cannot be empty")
  (%make-tiny-path-dir :name name))

(defstruct (tpath-root-dir
             (:constructor %make-tiny-path-root-dir)
             (:include tpath-dir
                       (name "" :type string :read-only t))))

;;------------------------------------------------------------

(defmethod print-object ((dir tpath-dir) stream)
  (if *in-print-path*
      (format stream "~a~a" (tpath-dir-name dir)
              (if *in-render-path*
                  (tpath-dir-seperator dir)
                  "/"))
      (format stream "#<DIR \"~a\">" (tpath-dir-name dir))))

(defmethod print-object ((file tpath-file) stream)
  (if *in-print-path*
      (format stream (tpath-file-name file))
      (format stream "#<FILE \"~a\">" (tpath-file-name file))))

(defmethod print-object ((path tpath) stream)
  (let ((*in-print-path* t)
        (*print-escape* nil))
    (format stream "#<PATH \"~{~a~}\">" (reverse (tpath-nodes path)))))

(defun tpath-render (path &optional stream)
  (let ((*in-print-path* t)
        (*in-render-path* t))
    (format stream "~{~a~}" (reverse (tpath-nodes path)))))

;;------------------------------------------------------------

(defun make-path-from-nodes (nodes)
  ;; What people extending tiny path will call from their constructors
  (assert (notany #'tpath-root-dir-p (butlast nodes)) ()
          "Only the last node in the node list may be a root-dir")
  (%make-tiny-path :nodes nodes
                   :absolute-p (tpath-root-dir-p (car (last nodes)))
                   :length (length nodes)))

(defun make-tpath (str &optional file)
  (let* ((dir (not file))
         (absolute (char= #\/ (char str 0)))
         (str (if absolute (subseq str 1) str))
         (split (split-string str :separator '(#\/))))
    (assert (or split absolute) () "Empty relative paths are invalid")
    (let ((nodes (if absolute
                     (list (%make-tiny-path-root-dir))
                     (list))))
      (loop :for (current next) :on split :do
         (if (string= current "")
             (unless (and dir (not next))
               (error "Non root directory name cannot be empty"))
             (push (if (or next dir)
                       (%make-tiny-path-dir :name current)
                       (%make-tiny-path-file :name current))
                   nodes)))
      (make-path-from-nodes nodes))))

(defun tpath (str &optional dir)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-tpath str dir))

;;------------------------------------------------------------

(declaim (inline ensure-tpath)
         (ftype (function (t &optional t) tpath) ensure-tpath))
(defun ensure-tpath (x &optional assume-file-if-unknown)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (etypecase x
    (tpath x)
    (string (make-tpath x assume-file-if-unknown))
    (pathname (tpath-from-pathname x))))

(defun tpath+ (a b)
  ;; when we ensure-tpath we have to assume dir if string given
  ;; otherwise will always fail
  (let* ((a (ensure-tpath a))
         (b (ensure-tpath b t))
         (nodes-a (tpath-nodes a))
         (nodes-b (tpath-nodes b))
         (len-a (tpath-length a))
         (len-b (tpath-length b)))
    (assert (not (tpath-node-terminal-p (first nodes-a))))
    (%make-tiny-path :nodes (append nodes-b nodes-a)
                     :absolute-p (tpath-absolute-p a)
                     :length (+ len-a len-b))))

(defun tpath-pop (path &optional (n 1))
  (assert (>= n 1) () "Can't pop less that 1 node from path")
  (let* ((path (ensure-tpath path))
         (nodes (tpath-nodes path))
         (len (tpath-length path))
         (remaining-nodes (nthcdr n nodes)))
    (values (when remaining-nodes
              (%make-tiny-path :nodes remaining-nodes
                               :absolute-p (tpath-absolute-p path)
                               :length (- len n)))
            (subseq nodes 0 n))))

(defun tpath-relative-p (path)
  (not (tpath-absolute-p path)))

(defun tpath-push (name path &optional dir)
  (path+ path (make-tpath name dir)))

(defun tpath-file-dir-p (path)
  (let ((path (ensure-tpath path)))
    (tpath-dir-p (first (tpath-nodes path)))))

(defun tpath-file-path-p (path)
  (let ((path (ensure-tpath path)))
    (tpath-file-p (first (tpath-nodes path)))))

;;------------------------------------------------------------
