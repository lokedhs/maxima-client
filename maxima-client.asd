(defsystem maxima-client
  :name "maxima-client"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Maxima CLIM client"
  :depends-on (:alexandria
               :mcclim
               :log4cl
               :maxima)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "util")
                                     (:file "renderer")
                                     (:file "cmdline")))))
