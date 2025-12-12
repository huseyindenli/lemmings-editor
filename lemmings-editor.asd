;;; lemmings-editor.asd

(asdf:defsystem #:lemmings-editor
  :description "Simple 2D Lemmings-like level editor using Ltk"
  :author "oleg harput"
  :license "MIT"
  :depends-on (:ltk)
  :serial t
  :components
  ((:file "package")
   (:file "editor")))
