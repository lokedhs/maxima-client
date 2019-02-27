(defsystem maxima-client
  :name "maxima-client"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Maxima CLIM client"
  :depends-on (:alexandria
               :mcclim
               :log4cl
               :trivial-gray-streams
               :trivial-arguments
               :lambda-fiddle
               :cl-ppcre
               :maxima)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     #+nil(:file "clipboard")
                                     (:file "workbench")
                                     (:file "util")
                                     (:file "renderer-util")
                                     (:file "markup-new")
                                     (:file "renderer")
                                     (:file "disp")
                                     (:file "popup")
                                     (:file "output")
                                     (:file "queries")
                                     (:file "info")
                                     (:file "info-new")
                                     (:file "reflect")
                                     (:file "maxima-syntax")
                                     (:file "character-picker")
                                     (:file "notes")
                                     (:file "cmdline")
                                     (:file "maxima-plot")
                                     (:file "plot2d")
                                     (:file "algebra-cmd")
                                     (:file "matrix-edit")
                                     (:file "matrix-cmd"))))
  :perform (load-op :after (o c)
                    (load (asdf:system-relative-pathname (asdf:find-system :maxima) "../lisp-utils/defsystem.lisp"))))
