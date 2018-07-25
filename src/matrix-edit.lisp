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
   (orig :initarg :orig
         :reader matrix-edit-value/orig)
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
                (clim:with-output-as-presentation (stream value 'matrix-edit-value :allow-sensitive-inferiors nil)
                  (let ((expr-rec (make-rendered-output-record (stream)
                               (render-maxima-expression stream (matrix-edit-value/orig value)))))
                    (clim:stream-add-output-record stream expr-rec))))))
    (setf (matrix-edit-value/record value) rec)
    (setf (matrix-edit-value/edit-stream value) edit-stream)))

(clim:define-command (edit-matrix-command :name "Edit matrix" :menu t :command-table maxima-commands)
    ((varname maxima-native-symbol :prompt "Variable"))
  (let ((matrix-content (symbol-value varname)))
    (unless (eq (caar matrix-content) 'maxima::$matrix)
      (error "Variable does not contain a matrix"))
    (let* ((stream *standard-output*)
           (rec nil)
           (values nil))
      (setq rec (clim:updating-output (stream)
                  (clim:formatting-table (stream)
                    (loop
                      for matrix-row in (cdr matrix-content)
                      for row from 0
                      for row-values = (let ((col-values nil))
                                         (assert (eq (caar matrix-row) 'maxima::mlist))
                                         (clim:formatting-row (stream)
                                           (loop
                                             for matrix-col in (cdr matrix-row)
                                             for col from 0
                                             for value = (make-instance 'matrix-edit-value
                                                                        :orig matrix-col
                                                                        :row row
                                                                        :col col)
                                             do (clim:formatting-cell (stream)
                                                  (make-matrix-edit-field stream value))
                                             collect value into result
                                             finally (setq col-values result)))
                                         col-values)
                      collect row-values into result
                      finally (setq values result)))))
      (let ((curr-row 0)
            (curr-col 0))
        (labels ((p ()
                   (log:info "pos: ~s,~s" curr-row curr-col))
                 (constrain-col (new-col)
                   (let ((row (nth curr-row values)))
                     (setf curr-col (max (min new-col (1- (length row))) 0))))
                 (move-x (delta)
                   (constrain-col (+ curr-col delta))
                   (p))
                 (move-y (delta)
                   (setf curr-row (max (min (+ curr-row delta) (1- (length values))) 0))
                   (constrain-col curr-col)
                   (p)))
          (loop
            for gesture = (clim:with-input-context ('matrix-edit-value :override t)
                              (object type)
                              (clim:read-gesture :stream stream)
                            (matrix-edit-value
                             (log:info "value clicked = ~s" object)))
            when (typep gesture 'clim:key-press-event)
              do (let ((event-name (clim:keyboard-event-key-name gesture)))
                   (if (gesture-modifier-p gesture :control)
                       (case event-name
                         (:|p| (move-y -1))
                         (:|n| (move-y 1))
                         (:|b| (move-x -1))
                         (:|f| (move-x 1)))
                       (case event-name
                         (:up (move-y -1))
                         (:down (move-y 1))
                         (:left (move-x -1))
                         (:right (move-x 1)))))))))))
