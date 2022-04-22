(in-package #:cl-user)

(defpackage #:red-black-tree
  (:local-nicknames
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:max
   #:min)
  (:export
   #:delete
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
