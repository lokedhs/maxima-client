(in-package :maxima-client.common)

(defparameter *invert-readtable* (let ((readtable (copy-readtable)))
                                   (setf (readtable-case readtable) :invert)
                                   readtable))

(defclass maxima-native-expr ()
  ((src :initarg :src
        :initform nil)
   (expr :initarg :expr
         :reader maxima-native-expr/expr)))

(defmethod print-object ((obj maxima-native-expr) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (src expr) obj
      (format stream "SRC ~s EXPR ~s" src expr))))

(defun maxima-native-expr/src (expr)
  (or (slot-value expr 'src)
      (maxima-expr-as-string (maxima-native-expr/expr expr))))

(defun present-to-stream (obj stream &key (record-type 'clim:standard-presentation))
  (clim:present obj (clim:presentation-type-of obj) :stream stream
                :record-type record-type))

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
                          (etypecase ,output-record
                            (clim:output-record (clim:output-record-position ,output-record))
                            (clim:region (clim:bounding-rectangle-position ,output-record)))
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

(defun find-presentation-at-pos (stream x y)
  (let ((presentation (clim:find-innermost-applicable-presentation (list (cons t
                                                                               (lambda (&rest rest)
                                                                                 (log:info "Presentation test: ~s" rest))))
                                                                   stream x y)))
    (when presentation
      (log:trace "Found presentation: ~s" presentation))
    presentation))

(defun gesture-modifier-p (gesture modifiers)
  (let ((bitmap (cond
                  ((keywordp modifiers) (clim:make-modifier-state modifiers))
                  ((listp modifiers) (loop
                                       for m in modifiers
                                       for state = (clim:make-modifier-state m)
                                       for b = state then (logior b state)
                                       finally (return b)))
                  (t (error "Unexpected modifiers value: ~s" modifiers)))))
    (eql bitmap (logand (clim::event-modifier-state gesture) bitmap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maxima utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *use-clim-retrieve* nil)
(defvar *current-stream* nil)

(defmacro with-maxima-package (&body body)
  `(let ((*package* (find-package :maxima)))
     ,@body))

(define-condition maxima-native-error (error)
  ())

(defgeneric maxima-native-error/message (condition))

(define-condition maxima-expr-parse-error (maxima-native-error)
  ((src     :type string
            :initarg :src
            :reader maxima-expr-parse-error/src)
   (message :type string
            :initarg :message
            :reader maxima-expr-parse-error/message)
   (pos     :initarg :pos
            :reader maxima-expr-parse-error/pos))
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
             (funcall ,handler ,eval-ret (maxima-stream-text ,maxima-stream))
             ,result)))))

(defmacro wrap-function (name args &body body)
  (let* ((new-fn-name (intern (concatenate 'string "CLIM-" (symbol-name name))))
         (wrapped-fn-name (intern (concatenate 'string "WRAPPED-" (symbol-name name))))
         (old-fn-ptr (intern (concatenate 'string "*OLD-FN-" (symbol-name name) "*")))
         (rest-var (lambda-fiddle:rest-lambda-var args))
         (forward-call-args (let ((standard-args (append (lambda-fiddle:required-lambda-vars args)
                                                         (lambda-fiddle:optional-lambda-vars args)))
                                  (keyword-args (loop
                                                  for kw in (lambda-fiddle:key-lambda-vars args)
                                                  append (list (intern (symbol-name kw) "KEYWORD") kw))))
                              (if rest-var
                                  (append standard-args keyword-args (list rest-var))
                                  (append standard-args keyword-args)))))
    (labels ((forward (fn)
               `(,(if rest-var 'apply 'funcall) ,fn ,@forward-call-args)))
      `(progn
         (defvar ,old-fn-ptr nil)
         (defun ,new-fn-name ,args ,@body)
         (defun ,wrapped-fn-name ,args
           (if *use-clim-retrieve*
               ,(forward `(function ,new-fn-name))
               ,(forward old-fn-ptr)))
         (unless ,old-fn-ptr
           (setq ,old-fn-ptr (symbol-function ',name))
           (setf (symbol-function ',name) #',wrapped-fn-name))))))

(defun string-to-maxima-expr (string)
  (with-maxima-error-handler
      (lambda (type text)
        (declare (ignore type))
        ;; Copied from MAXIMA:MREAD-SYERR
        (flet ((column ()
                 (let ((n (get 'maxima::*parse-window* 'maxima::length))
	               ch some)
	           (loop for i from (1- n) downto (- n 20)
	     	         while (setq ch (nth i maxima::*parse-window*))
		         do (cond ((char= ch #\newline)
			           (return-from column some))
			          (t (push ch some))))
	           some)))
          (error 'maxima-expr-parse-error :src string :message text :pos (min (length string) (- (length (column)) 2)))))
    (with-input-from-string (s (format nil "~a;" string))
      (let ((form (maxima::dbm-read s nil nil)))
        (assert (and (listp form)
                     (= (length form) 3)
                     (or (equal (first form) '(maxima::displayinput))
                         (equal (first form) '(maxima::nodisplayinput)))
                     (null (second form))))
        (third form)))))

(defun string-to-native-expr (string)
  (let ((maxima-expr (string-to-maxima-expr string)))
    (make-instance 'maxima-native-expr :src string :expr maxima-expr)))

(defun maxima-expr-as-string (expr)
  (coerce (maxima::mstring expr) 'string))

(defun maxima-coerce-float (expr)
  (with-maxima-package
    (maxima::coerce-float expr)))

(defun maxima-native-expr-as-float (expr)
  (and expr
       (maxima-coerce-float (maxima-native-expr/expr expr))))

(defun eval-maxima-expression (expr)
  (with-maxima-package
    (maxima::meval* expr)))

(defun maxima-list-to-list (expr)
  (unless (maxima::$listp expr)
    (error "Argument is not a maxima list: ~s" expr))
  (cdr expr))

(defun format-sym-name (sym &key any-sym)
  (check-type sym symbol)
  (let ((n (let ((*readtable* *invert-readtable*))
             (princ-to-string sym))))
    (cond ((and (plusp (length n))
                (or (eql (aref n 0) #\$)
                    (eql (aref n 0) #\%)))
           (subseq n 1))
          (any-sym
           n)
          (t
           (format nil "~s" sym)))))
