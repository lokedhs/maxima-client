(defpackage :infoparser
  (:use :cl :maxima-client.common)
  (:export #:parse-file
           #:resolve-example-code
           #:generate-doc-directory))
