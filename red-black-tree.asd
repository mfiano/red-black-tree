(asdf:defsystem #:red-black-tree
  :description "An implementation of the red-black search tree data structure."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/red-black-tree"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on
  (#:mfiano-utils)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "red-black-tree")))
