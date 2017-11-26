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

(defstruct (tiny-path
             (:constructor %make-tiny-path))
  (nodes nil :type list :read-only t)
  ;; these 2 slots are an optimization so we can avoid list walking
  (absolute-p nil :type boolean :read-only t)
  (length 0 :type tiny-path-index :read-only t))


(defstruct tiny-path-node
  (name nil :type string :read-only t)
  (terminal-p nil :type boolean :read-only t))


(defstruct (tiny-path-file
             (:constructor %make-tiny-path-file)
             (:include tiny-path-node
                       (terminal-p t :type boolean :read-only t))))


(defstruct (tiny-path-dir
             (:constructor %make-tiny-path-dir)
             (:include tiny-path-node
                       (terminal-p nil :type boolean :read-only t)))
  (seperator "/" :type string :read-only t))


(defstruct (tiny-path-root-dir
             (:constructor %make-tiny-path-root-dir)
             (:include tiny-path-dir
                       (name "" :type string :read-only t))))

;;------------------------------------------------------------

(defmethod print-object ((dir tiny-path-dir) stream)
  (if *in-print-path*
      (format stream "~a~a" (tiny-path-dir-name dir)
              (if *in-render-path*
                  (tiny-path-dir-seperator dir)
                  "/"))
      (format stream "#<DIR \"~a\">" (tiny-path-dir-name dir))))

(defmethod print-object ((file tiny-path-file) stream)
  (if *in-print-path*
      (format stream (tiny-path-file-name file))
      (format stream "#<FILE \"~a\">" (tiny-path-file-name file))))

(defmethod print-object ((path tiny-path) stream)
  (let ((*in-print-path* t)
        (*print-escape* nil))
    (format stream "#<PATH \"~{~a~}\">" (reverse (tiny-path-nodes path)))))

(defun render-path (path &optional stream)
  (let ((*in-print-path* t)
        (*in-render-path* t))
    (format stream "~{~a~}" (reverse (tiny-path-nodes path)))))

;;------------------------------------------------------------

(defun make-path-from-nodes (nodes)
  ;; What people extending tiny path will call from their constructors
  (assert (notany #'tiny-path-root-dir-p (butlast nodes)) ()
          "Only the last node in the node list may be a root-dir")
  (%make-tiny-path :nodes nodes
                   :absolute-p (typep (car (last nodes)) 'tiny-path-root-dir)
                   :length (length nodes)))

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
           (nodes (mapcar (lambda (x) (%make-tiny-path-dir :name x))
                          (rest sub-strs)))
           (nodes (cons (if dir
                            (%make-tiny-path-dir :name (first sub-strs))
                            (%make-tiny-path-file :name (first sub-strs)))
                        nodes)))
      (make-path-from-nodes nodes))))

;;------------------------------------------------------------

(defun path+ (a b)
  (let ((nodes-a (tiny-path-nodes a))
        (nodes-b (tiny-path-nodes b))
        (len-a (tiny-path-length a))
        (len-b (tiny-path-length b)))
    (assert (not (tiny-path-node-terminal-p (first nodes-a))))
    (%make-tiny-path :nodes (append nodes-b nodes-a)
                     :absolute-p (tiny-path-absolute-p a)
                     :length (+ len-a len-b))))

(defun path-pop (path &optional (n 1))
  (assert (>= n 1) () "Can't pop less that 1 node from path")
  (let* ((nodes (tiny-path-nodes path))
         (len (tiny-path-length path))
         (remaining-nodes (nthcdr n nodes)))
    (values (when remaining-nodes
              (%make-tiny-path :nodes remaining-nodes
                               :absolute-p (tiny-path-absolute-p path)
                               :length (- len n)))
            (subseq nodes 0 n))))

(defun path-push (name path &optional dir)
  (path+ path (make-path name dir)))

(defun file-path-p (path)
  (typep (first (tiny-path-nodes path)) 'tiny-path-file))

;;------------------------------------------------------------
