(in-package :maxima-client)

(defclass matrix-edit-view ()
  ())

(defparameter +matrix-edit-view+ (make-instance 'matrix-edit-view))

(clim:define-presentation-method clim:accept ((type maxima-native-expr) stream (view matrix-edit-view) &key)
  (log:info "Accepting native expr")
  (let ((result (read-plain-text stream)))
    (log:info "Read: ~s" result)
    (string-to-native-expr "1+x")))

(clim:define-presentation-method clim:accept-present-default ((type maxima-native-expr)
                                                              stream
                                                              (view matrix-edit-view)
                                                              default
                                                              default-supplied-p
                                                              present-p
                                                              query-identifier)
  (log:info "presenting value: ~s, p=~s, qi=~s" default present-p query-identifier)
  (format stream "foo"))

(clim:define-command (edit-matrix-command :name "Edit matrix" :menu t :command-table maxima-commands)
    ((varname maxima-native-symbol :prompt "Variable"))
  (declare (ignore varname))
  (let ((stream *standard-output*))
    (clim:accepting-values (stream :scroll-bars t
                                   :initially-select-query-identifier '(matrix-edit 0 0)
                                   :align-prompts nil
                                   :resize-frame t)
      (let ((values (make-array '(5 3) :initial-element (string-to-native-expr "0"))))
        (clim:formatting-table (stream)
          (loop
            for row from 0 below (array-dimension values 0)
            do (clim:formatting-row (stream)
                 (loop
                   for col from 0 below (array-dimension values 1)
                   do (clim:formatting-cell (stream)
                        (let ((result (clim:accept 'maxima-native-expr :stream stream :view +matrix-edit-view+ :prompt nil
                                                                       :query-identifier (list 'matrix-edit row col)
                                                                       :default (aref values row col))))
                          (setf (aref values row col) result)))))))
        values))))
