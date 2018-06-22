(defpackage :maxima-client
  (:use :cl)
  (:documentation "CLIM Maxima client")
  (:export #:maxima-client))

(defpackage :maxima-client.markup
  (:use :cl)
  (:export #:display-markup
           #:text-commands))
