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
           #:cancel-button
           #:load-image
           #:find-interactor-pane
           #:*font-directory*
           #:*image-directory*
           #:find-info-root-path
           #:*info-directory*
           #:xdimension-bind
           #:dimension-bind-new
           #:print-unreadable-safely
           #:eval-maxima-expression-to-float
           #:find-subpane-named))

(defpackage :maxima-client
  (:use :cl :maxima-client.common)
  (:documentation "CLIM Maxima client")
  (:export #:maxima-client))


(defpackage :maxima-client.gui-tools
  (:use :cl :maxima-client.common)
  (:export #:render-element
           #:get-element-filter-name
           #:select-completion-match
           #:element-dimensions))

(defpackage :maxima-client.markup
  (:use :cl :maxima-client.common)
  (:export #:display-markup
           #:text-commands
           #:with-word-wrap
           #:text-link-url
           #:text-link
           #:text-link/description
           #:maxima-function-reference
           #:markup-text-view
           #:node-reference
           #:node-reference/name
           #:named-reference
           #:named-reference/name
           #:named-reference/destination
           #:category-reference
           #:markup
           #:word-wrap-draw-string
           #:word-wrap-draw-presentation
           #:draw-current-line-and-reset
           #:add-vspacing
           #:font-char-width))

(defpackage :maxima-client.clipboard
  (:use :cl)
  (:export #:bind-clipboard))

(defpackage :maxima-client.workbench
  (:use :cl)
  (:export #:make-workbench
           #:workbench-commands
           #:add-top-pane
           #:add-right-pane
           #:pane-visible-p
           #:show-top-pane
           #:show-right-pane))

(defpackage :maxima-client.notes
  (:use :cl :maxima-client.common)
  (:export #:notes-pane
           #:focus-notes-pane
           #:insert-maxima-expr))

(defpackage :maxima-client.doc
  (:use :cl :maxima-client.common)
  (:export #:add-info-page
           #:info-pane
           #:make-info-panel))

(defpackage :maxima-client.doc-new
  (:use :cl :maxima-client.common)
  (:export
   #:open-documentation-frame
   #:display-function-help
   #:*maxima-toplevel-filename*
   #:maxima-function-name))

(defpackage :maxima-client.canvas
  (:use :cl :maxima-client.common)
  (:export #:canvas
           #:make-canvas-pane
           #:canvas-pane))
