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
                                     (:file "clipboard")
                                     (:file "util")
                                     (:file "renderer-util")
                                     (:file "wrap")
                                     (:file "markup")
                                     (:file "renderer")
                                     (:file "popup")
                                     (:file "output")
                                     (:file "queries")
                                     (:file "info")
                                     (:file "reflect")
                                     (:file "maxima-syntax")
                                     (:file "character-picker")
                                     (:file "cmdline")
                                     (:file "maxima-plot")
                                     (:file "plot2d")
                                     (:file "algebra-cmd")
                                     (:file "matrix-cmd")))))
