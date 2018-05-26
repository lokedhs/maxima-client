(in-package :maxima-client)

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defmacro dimension-bind ((output-record &key
                                           ((:width width-sym)) ((:height height-sym))
                                           ((:x x-sym)) ((:y y-sym))
                                           ((:right right-sym)) ((:bottom bottom-sym))
                                           ((:baseline baseline-sym)))
                          &body body)
  (alexandria:with-gensyms (width height x y)
    (alexandria:once-only (output-record)
      (labels ((make-body ()
                 `(progn ,@body))
               (make-baseline ()
                 (if baseline-sym
                     `(let ((,baseline-sym (clim-extensions:output-record-baseline ,output-record)))
                        ,(make-body))
                     (make-body)))
               (make-position-form ()
                 (if (or x-sym y-sym right-sym bottom-sym)
                     `(multiple-value-bind (,x ,y)
                          (clim:output-record-position ,output-record)
                        (declare (ignorable ,x ,y))
                        (let (,@(if x-sym `((,x-sym ,x)))
                              ,@(if y-sym `((,y-sym ,y)))
                              ,@(if right-sym `((,right-sym (+ ,x ,width))))
                              ,@(if bottom-sym `((,bottom-sym (+ ,y ,height)))))
                          ,(make-baseline)))
                     (make-baseline))))
        (if (or width-sym height-sym right-sym bottom-sym)
            `(multiple-value-bind (,width ,height)
                 (clim:rectangle-size ,output-record)
               (declare (ignorable ,width ,height))
               (let (,@(if width-sym `((,width-sym ,width)))
                     ,@(if height-sym `((,height-sym ,height))))
                 ,(make-position-form)))
            (make-position-form))))))

(defun clamp (n min max)
  (min (max n min) max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maxima utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition maxima-native-error (error)
  ())

(defgeneric maxima-native-error/message (condition))

(define-condition maxima-expr-parse-error (maxima-native-error)
  ((src     :type string
            :initarg :src
            :reader maxima-expr-parse-error/src)
   (message :type string
            :initarg :message
            :reader maxima-expr-parse-error/message))
  (:report (lambda (condition stream)
             (format stream "Parse error:~%~a"
                     (maxima-expr-parse-error/message condition)))))

(defmacro with-maxima-error-handler (handler &rest body)
  (alexandria:once-only (handler)
    (alexandria:with-gensyms (maxima-stream eval-ret result)
      `(let* ((,maxima-stream (make-instance 'maxima-output))
              (,result nil)
              (,eval-ret (catch 'maxima::macsyma-quit
                           (setq ,result (let ((*standard-output* ,maxima-stream))
                                           (progn ,@body)))
                           nil)))
         (if ,eval-ret
             (funcall ,handler ,eval-ret (string-trim (format nil " ~c" #\Newline) (maxima-stream-text ,maxima-stream)))
             ,result)))))

#+nil
(defun string-to-maxima-expr (string)
  (with-input-from-string (s (format nil "~a;~%" string))
    (let ((form (maxima::dbm-read s nil nil)))
      (assert (and (listp form)
                   (= (length form) 3)
                   (equal (first form) '(maxima::displayinput))
                   (null (second form))))
      (third form))))

(defun string-to-maxima-expr (string)
  (with-maxima-error-handler
      (lambda (type text)
        (declare (ignore type))
        (error 'maxima-expr-parse-error :src string :message text))
    (with-input-from-string (s (format nil "~a;~%" string))
      (let ((form (maxima::dbm-read s nil nil)))
        (assert (and (listp form)
                     (= (length form) 3)
                     (equal (first form) '(maxima::displayinput))
                     (null (second form))))
        (third form)))))

(defun string-to-native-expr (string)
  (let ((maxima-expr (string-to-maxima-expr string)))
    (make-instance 'maxima-native-expr :src string :expr maxima-expr)))

(defun maxima-expr-as-string (expr)
  (coerce (maxima::mstring expr) 'string))

(defun maxima-native-expr-as-float (expr)
  (and expr
       (maxima::coerce-float (maxima-native-expr/expr expr))))
