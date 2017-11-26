;;;; tiny-path.asd

(asdf:defsystem #:tiny-path
  :description "A tiny path lib not based on, by compatible with, pathnames"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:uiop)
  :components ((:file "package")
               (:file "path")))

