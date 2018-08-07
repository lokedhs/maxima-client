(defpackage :maxima-client.common
  (:use :cl)
  (:export #:present-to-stream
           #:call-in-event-handler
           #:with-call-in-event-handler
           #:dimension-bind
           #:clamp
           #:find-presentation-at-pos
           #:gesture-modifier-p
           #:with-maxima-package
           #:maxima-native-error
           #:maxima-native-error/message
           #:maxima-expr-parse-error/src
           #:maxima-expr-parse-error/message
           #:maxima-expr-parse-error/pos
           #:with-maxima-error-handler
           #:wrap-function
           #:string-to-maxima-expr
           #:string-to-native-expr
           #:maxima-expr-as-string
           #:maxima-coerce-float
           #:maxima-native-expr-as-float
           #:eval-maxima-expression
           #:maxima-list-to-list
           #:format-sym-name
           #:maxima-expr-parse-error
           #:maxima-native-expr
           #:maxima-native-expr/expr
           #:maxima-native-expr/src
           #:maxima-io
           #:maxima-output
           #:maxima-input
           #:maxima-io/clim-stream
           #:maxima-io/buffer
           #:maxima-io/pos
           #:maxima-stream-text
           #:*invert-readtable*
           #:*use-clim-retrieve*
           #:*current-stream*
           #:maxima-error
           #:maxima-error/cmd
           #:maxima-error/content
           #:plain-text
           #:set-rec-position
           #:move-rec
           #:maxima-expr-to-latex
           #:with-temp-form
           #:with-interactive-form
           #:button
           #:ok-button
           #:cancel-button))

(defpackage :maxima-client
  (:use :cl :maxima-client.common)
  (:documentation "CLIM Maxima client")
  (:export #:maxima-client))


(defpackage :maxima-client.gui-tools
  (:use :cl :maxima-client.common)
  (:export #:render-element
           #:get-element-filter-name
           #:select-completion-match))

(defpackage :maxima-client.markup
  (:use :cl :maxima-client.common)
  (:export #:display-markup
           #:text-commands))

(defpackage :maxima-client.clipboard
  (:use :cl)
  (:export #:bind-clipboard))

(defpackage :maxima-client.workbench
  (:use :cl)
  (:export #:make-workbench
           #:workbench-commands))
