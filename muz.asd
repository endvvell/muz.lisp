(asdf:defsystem #:muz
  :serial t
  :author "endvvell"
  :description "Anki-like program for learning music theory."
  :depends-on ((:version #:cl-ppcre "2.1.1")
               (:version #:str "0.19.1")
               #:alexandria
               #:uiop
               #:serapeum
               #:clingon)
  :components ((:file "colors")
               (:file "muz"
                      :depends-on ("colors")))
  :build-operation "program-op"
  :build-pathname "bin/muz"
  :entry-point "muz:main")

;; silences compiled binary's start-up messages:
(push :deploy-console *features*)
