(defsystem infoparser
  :name "infoparser"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Parser for info documents"
  :depends-on (:alexandria
               :cl-ppcre
               :collectors
               :string-case
               :split-sequence
               :log4cl
               :maxima-client)
  :components ((:module infoparser
                        :serial t
                        :components ((:file "package")
                                     (:file "infoparser")))))
