(in-package :maxima-client)

(defclass matrix-edit-view (clim:textual-view)
  ())

(defparameter +matrix-edit-view+ (make-instance 'matrix-edit-view)
  )

(defclass matrix-edit-value ()
  ((col :initarg :col
        :reader matrix-edit-value/col)
   (row :initarg :row
        :reader matrix-edit-value/row)
   (orig :initarg :orig)
   (output-record :accessor matrix-edit-value/record)
   (edit-stream :accessor matrix-edit-value/edit-stream)))

(clim:define-presentation-method clim:accept ((type matrix-edit-value) stream (view matrix-edit-view) &key)
  (log:info "Accepting native expr")
  (let ((result (read-plain-text stream)))
    (log:info "Read: ~s" result)
    (make-instance 'matrix-edit-value :orig (string-to-native-expr "1+x"))))

(clim:define-presentation-method clim:present (obj (type matrix-edit-value) stream (view matrix-edit-view) &key)
  (log:info "Presenting value: ~s" obj)
  (format stream "foo"))

#+nil
(clim:define-presentation-method clim:accept-present-default ((type maxima-native-expr)
                                                              stream
                                                              (view matrix-edit-view)
                                                              default
                                                              default-supplied-p
                                                              present-p
                                                              query-identifier)
  (log:info "presenting value: ~s, p=~s, qi=~s" default present-p query-identifier)
  (format stream "foo"))

(clim:define-command-table edit-matrix-commands)

(defun make-matrix-edit-field (stream value)
  (let* ((edit-stream nil)
         (rec (clim:updating-output (stream :unique-id (list 'matrix-edit
                                                             (matrix-edit-value/row value)
                                                             (matrix-edit-value/col value))
                                            :id-test #'equal)
                (setf edit-stream (make-instance 'clim:standard-input-editing-stream
                                                 :stream stream
                                                 :cursor-visibility nil
                                                 :single-line t)))))
    (format stream "some content")
    (setf (matrix-edit-value/record value) rec)
    (setf (matrix-edit-value/edit-stream value) edit-stream)))

(clim:define-command (edit-matrix-command :name "Edit matrix" :menu t :command-table maxima-commands)
    ((varname maxima-native-symbol :prompt "Variable"))
  (declare (ignore varname))
  (let ((stream *standard-output*))
    (let ((values (make-array '(5 3))))
      (let ((rec (clim:updating-output (stream)
                   (clim:formatting-table (stream)
                     (loop
                       for row from 0 below (array-dimension values 0)
                       do (clim:formatting-row (stream)
                            (loop
                              for col from 0 below (array-dimension values 1)
                              for value = (make-instance 'matrix-edit-value
                                                         :orig (string-to-native-expr "0")
                                                         :row row
                                                         :col col)
                              do (setf (aref values row col) value)
                              do (clim:formatting-cell (stream)
                                   (make-matrix-edit-field stream value))))))))))
      (loop
        with curr-row = 0
        with curr-col = 0
        do (clim:with-input-context ('(command :command-table edit-matrix-commands))
               (object)
               (let ((value (aref values curr-row curr-col)))
                 (multiple-value-bind (object type)
                     (clim:accept 'maxima-native-expr :stream (matrix-edit-value/edit-stream value)
                                                      :view +matrix-edit-view+
                                                      :prompt nil)
                   (log:info "got object=~s type=~s" object type))))))))
