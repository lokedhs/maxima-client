(in-package :maxima-client)

(defclass matrix-edit-view (clim:textual-view)
  ())

(defparameter +matrix-edit-view+ (make-instance 'matrix-edit-view))

(clim:define-command-table edit-matrix-commands)

(defgeneric sheet-rows (sheet))
(defgeneric sheet-cols (sheet))
(defgeneric sheet-value-at-pos (sheet row col))
(defgeneric sheet-render-value (sheet row col stream))
(defgeneric sheet-render-col-title (sheet col stream))
(defgeneric sheet-render-row-title (sheet row stream))
(defgeneric sheet-edit-value-at-pos (sheet row col stream))

(defclass sheet-edit-value ()
  ((col :initarg :col
        :reader sheet-edit-value/col)
   (row :initarg :row
        :reader sheet-edit-value/row)))

(defun sheet-edit (stream sheet)
  (let ((curr-row 0)
        (curr-col 0)
        (margin-width 5))
    (with-interactive-form (stream :record-name rec :buttons-p t)
        (clim:formatting-table (stream)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream)
              (format stream " "))
            (loop
              for i from 0 below (sheet-rows sheet)
              do (clim:formatting-cell (stream :align-x :center :align-y :center)
                   (clim:updating-output (stream :unique-id (list 'col-header i)
                                                 :id-test #'equal
                                                 :cache-value i
                                                 :cache-test #'equal)
                     (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold nil))
                       (sheet-render-col-title sheet i stream))))))
          (loop
            for row from 0 below (sheet-rows sheet)
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream :align-x :center :align-y :center)
                   (clim:updating-output (stream :unique-id (list 'row-header row)
                                                 :id-test #'equal
                                                 :cache-value row
                                                 :cache-test #'equal)
                     (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold nil))
                       (sheet-render-row-title sheet row stream))))
                 (loop
                   for col from 0 below (sheet-cols sheet)
                   do (let ((selected (and (= curr-row row) (= curr-col col))))
                        (clim:formatting-cell (stream :align-x :center :align-y :center)
                          (clim:updating-output (stream :unique-id (list 'matrix-edit row col)
                                                        :id-test #'equal
                                                        :cache-value (list row col selected (sheet-value-at-pos sheet row col))
                                                        :cache-test #'equal)
                            (clim:with-output-as-presentation (stream (make-instance 'sheet-edit-value
                                                                                     :row row
                                                                                     :col col)
                                                                      'sheet-edit-value
                                                                      :allow-sensitive-inferiors nil)
                              (let ((value-rec (clim:with-output-to-output-record (stream)
                                                 (sheet-render-value sheet row col stream))))
                                (dimension-bind (value-rec :x x1 :y y1 :right right :bottom bottom)
                                  (let ((x2 (+ right (* margin-width 2)))
                                        (y2 (+ bottom (* margin-width 2))))
                                    (cond (selected
                                           (clim:draw-rectangle* stream x1 y1 x2 y2 :filled t :ink clim:+lightgrey+))
                                          (t
                                           (clim:draw-rectangle* stream x1 y1 x2 y2 :filled t :ink clim:+white+)))))
                                (move-rec value-rec margin-width margin-width)
                                (clim:stream-add-output-record stream value-rec))))))))))
      ;; Main loop for the form
      (labels ((redraw ()
                 (clim:redisplay rec stream))
               (move-x (delta)
                 (setf curr-col (max (min (+ curr-col delta) (1- (sheet-cols sheet))) 0))
                 (redraw))
               (move-y (delta)
                 (setf curr-row (max (min (+ curr-row delta) (1- (sheet-rows sheet))) 0))
                 (redraw)))
        (loop
          named matrix-edit-main-loop
          for gesture = (clim:with-input-context ('(or sheet-edit-value ok-button cancel-button) :override nil)
                            (object type)
                            (clim:read-gesture :stream stream)
                          (sheet-edit-value
                           (log:info "value clicked = ~s" object))
                          (ok-button
                           (log:info "ok button clicked: ~s" object))
                          (cancel-button
                           (return-from matrix-edit-main-loop nil)))
          when (characterp gesture)
            do (case gesture
                 (#\Newline
                  (sheet-edit-value-at-pos sheet curr-row curr-col stream)
                  (redraw)))
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
                       (:right (move-x 1))
                       (:escape
                        (return-from matrix-edit-main-loop nil))))))))))

(defclass matrix-sheet ()
  ((matrix :reader matrix-sheet/matrix)))

(defmethod initialize-instance :after ((obj matrix-sheet) &key matrix-content)
  (unless (eq (caar matrix-content) 'maxima::$matrix)
    (error "Variable does not contain a matrix"))
  (let* ((max-col (reduce #'max (mapcar (lambda (v)
                                          (length (cdr v)))
                                        (cdr matrix-content))))
         (content (make-array (list (length (cdr matrix-content)) max-col))))
    (loop
      for row-content in (cdr matrix-content)
      for row from 0
      do (loop
           for col-content in (cdr row-content)
           for col from 0
           do (setf (aref content row col) col-content)))
    (setf (slot-value obj 'matrix) content)))

(defmethod sheet-rows ((sheet matrix-sheet))
  (array-dimension (matrix-sheet/matrix sheet) 0))

(defmethod sheet-cols ((sheet matrix-sheet))
  (array-dimension (matrix-sheet/matrix sheet) 1))

(defmethod sheet-value-at-pos ((sheet matrix-sheet) row col)
  (aref (matrix-sheet/matrix sheet) row col))

(defmethod sheet-render-value ((sheet matrix-sheet) row col stream)
  (let ((rec (make-rendered-output-record (stream)
               (render-maxima-expression stream (aref (matrix-sheet/matrix sheet) row col)))))
    (clim:stream-add-output-record stream rec)))

(defmethod sheet-render-col-title ((sheet matrix-sheet) col stream)
  (format stream "~a" (1+ col)))

(defmethod sheet-render-row-title ((sheet matrix-sheet) row stream)
  (format stream "~a" (1+ row)))

(defmethod sheet-edit-value-at-pos ((sheet matrix-sheet) row col stream)
  (let ((prev-value (aref (matrix-sheet/matrix sheet) row col)))
    (log:info "prev value is: ~s" prev-value)
    (multiple-value-bind (result type)
        (clim:accept 'maxima-native-expr :default (make-instance 'maxima-native-expr :expr prev-value) :insert-default t)
      (log:info "Got result: ~s, type: ~s" result type)
      (ecase type
        (maxima-native-expr (setf (aref (matrix-sheet/matrix sheet) row col)
                                  (eval-maxima-expression (maxima-native-expr/expr result))))))))

(clim:define-command (edit-matrix-command :name "Edit matrix" :menu t :command-table maxima-commands)
    ((varname maxima-native-symbol :prompt "Variable"))
  (let ((stream (find-interactor-pane))
        (sheet (make-instance 'matrix-sheet :matrix-content (symbol-value varname))))
    (sheet-edit stream sheet)))
