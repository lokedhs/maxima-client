(in-package :maxima-client.clipboard)

(defmethod climb:bind-selection ((port clim-clx::clx-port) window &optional time)
  (log:info "bind selection")
  (let* ((mirror (clim-clx::sheet-direct-xmirror window))
         (display (xlib:window-display mirror)))
    (xlib:set-selection-owner display :primary mirror time)
    (eq (xlib:selection-owner display :primary) mirror)))

(defmethod climb:release-selection ((port clim-clx::clx-port) &optional time)
  (log:info "release selection")
  (xlib:set-selection-owner (clim-clx::clx-port-display port) :primary nil time)
  (setf (climb::selection-owner port) nil)
  (setf (climb::selection-timestamp port) nil))

(defmethod climb:request-selection ((port clim-clx::clx-port) requestor time)
  (log:info "request selection")
  (xlib:convert-selection :primary :UTF8_STRING requestor :bounce time))

(defmethod climb:get-selection-from-event ((port clim-clx::clx-port) (event clim-clx::clx-selection-notify-event))
  (log:info "get selection")
  (if (null (clim-clx::selection-event-property event))
      (progn
        (format *trace-output* "~&;; Oops, selection-notify property is null. Trying the cut buffer instead..~%")
        (xlib:cut-buffer (clim-clx::clx-port-display port)))                
      (let ((v (xlib:get-property (clim-clx::sheet-xmirror (clim::event-sheet event))
                                  (clim-clx::selection-event-property event)
                                  ;; :type :text
                                  :delete-p t
                                  :result-type '(vector (unsigned-byte 8)))))
        (case (clim-clx::selection-event-target event)
          (:string (babel:octets-to-string v :encoding :iso-88519-1))
          (:utf8_string (babel:octets-to-string v :encoding :utf-8))))))

(defmethod climb:send-selection ((port clim-clx::clx-port) (event clim-clx::clx-selection-request-event) string)
  (log:info "sending selection")
  (let ((requestor (climb::selection-event-requestor event))
        (property  (clim-clx::selection-event-property event))
        (target    (clim-clx::selection-event-target event))
        (time      (clim:event-timestamp event)))
    (flet ((send-event (&key target (property property))
	     ;; debugging output, but the KDE Klipper client turns out
	     ;; to poll other clients for selection, which means it
	     ;; would be bad to print at every request.
             (xlib:send-event requestor
			      :selection-notify nil
			      :window requestor
			      :event-window requestor
			      :selection (climi::selection-event-selection event)
			      :target target
			      :property property
			      :time time)))
      (case target
	((:UTF8_STRING)
	 (xlib:change-property requestor property
                               (babel:string-to-octets string :encoding :utf-8)
			       :UTF8_STRING 8)
	 (send-event :target :UTF8_STRING))
	((:STRING :COMPOUND_TEXT)
	 (xlib:change-property requestor property
			       (babel:string-to-octets string :encoding :utf-8)
			       target 8)            
	 (send-event :target target))
	((:TEXT)
	 (cond
	   ((clim-clx::exactly-encodable-as-string-p string)
	    (xlib:change-property requestor property
                                  (babel:string-to-octets string :encoding :utf-8)
				  :STRING 8)
	    (send-event :target :STRING))
	   (t 
	    (xlib:change-property requestor property
				  (babel:string-to-octets string :encoding :utf-8)
				  :UTF8_STRING 8)
	    (send-event :target :UTF8_STRING))))
	((:TARGETS)
	 (let* ((display (clim-clx::clx-port-display port))
		(targets (mapcar (lambda (x) (xlib:intern-atom display x))
				 '(:TARGETS :STRING :TEXT :UTF8_STRING
				   :COMPOUND_TEXT :TIMESTAMP))))
	   (xlib:change-property requestor property targets target 32))
	 (send-event :target :TARGETS))
	((:TIMESTAMP)
	 (when (null (climb::selection-timestamp port))
	   (format *trace-output* "~&;; selection-timestamp is null!~%"))
	 (xlib:change-property requestor property
			       (list (climb::selection-timestamp port))
			       target 32)
	 (send-event :target :TIMESTAMP))
	(t
	 (format *trace-output*
		 "~&;; Warning, unhandled type \"~A\". ~
                  Sending property NIL to target.~%" target)
	 (send-event :target target :property nil))))
    (xlib:display-force-output (xlib:window-display requestor))))

(defvar *port-clipboard-owners* (trivial-garbage:make-weak-hash-table :test 'eq :weakness :key :weakness-matters nil))

(defun bind-clipboard (pane object)
  (let* ((mirror (clim-clx::sheet-direct-xmirror pane))
         (display (xlib:window-display mirror)))
    (xlib:set-selection-owner display :clipboard mirror nil)
    (when (eq (xlib:selection-owner display :clipboard) mirror)
      (setf (gethash (clim:port pane) *port-clipboard-owners*) object))))

(defgeneric get-clipboard-value (value target event))

(defmethod get-clipboard-value (port (value string) (target (eql :utf8_string)))
  (values (babel:string-to-octets value :encoding :utf-8) 8))

(defmethod get-clipboard-value (port (value string) (target (eql :targets)))
  (values (list (xlib:intern-atom (clim-clx::clx-port-display port) :utf8_string)) 32))
