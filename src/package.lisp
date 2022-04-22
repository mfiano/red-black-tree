(in-package #:cl-user)

(defpackage #:red-black-tree
  (:local-nicknames
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:find
   #:max
   #:min)
  (:export
   #:delete-node
   #:find
   #:insert
   #:make-tree
   #:max
   #:min
   #:next
   #:node
   #:previous
   #:tree
   #:valid-p
   #:walk))
