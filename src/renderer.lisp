(in-package :maxima-client)

(defvar *font-roman* '("Latin Modern Math" "Regular"))
(defvar *font-italic* '("Noto Serif" "Italic"))
(defvar *font-fixed* '("Source Code Pro" "Regular"))
(defvar *draw-boxes* nil)

(defvar *aligned-rendering-pos*)
(defvar *aligned-rendering-stream*)
(defvar *font-size*)
(defvar *rop*)
(defvar *lop*)

(defparameter *invert-readtable* (let ((readtable (copy-readtable)))
                                   (setf (readtable-case readtable) :invert)
                                   readtable))

(defclass maxima-renderer-view ()
  ())

(defclass maxima-interactor-view (maxima-renderer-view clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

(defun format-sym-name (sym) 
  (let ((*readtable* *invert-readtable*))
    (princ-to-string sym)))

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

(defun set-rec-position (output-record x y)
  (dimension-bind (output-record :x old-x :y old-y)
    (setf (clim:output-record-position output-record)
          (values (or x old-x)
                  (or y old-y)))))

(defun move-rec (output-record dx dy)
  (dimension-bind (output-record :x old-x :y old-y)
    (setf (clim:output-record-position output-record)
          (values (+ dx old-x)
                  (+ dy old-y)))))

(defun make-boxed-output-record (stream rec)
  (if *draw-boxes*
      (clim:with-output-to-output-record (stream)
        (dimension-bind (rec :x x :y y :right right :bottom bottom)
          (clim:stream-add-output-record stream rec)
          (clim:draw-rectangle* stream x y (1- right) (1- bottom) :filled nil)))
      rec))

(defmacro with-font ((stream font &optional size) &body body)
  (alexandria:once-only (stream font)
    (alexandria:with-gensyms (body-fn)
      `(labels ((,body-fn ()
                  (clim:with-text-style (,stream (clim:make-text-style (first ,font) (second ,font) *font-size*))
                    ,@body)))
         ,(if size
              `(let ((*font-size* ,size))
                 (,body-fn))
              `(,body-fn))))))

(defmacro with-roman-text-style ((stream &optional size) &body body)
  `(with-font (,stream *font-roman* ,size)
     ,@body))

(defmacro with-italic-text-style ((stream &optional size) &body body)
  `(with-font (,stream *font-italic* ,size)
     ,@body))

(defmacro with-fix-text-style ((stream) &body body)
  `(clim:with-text-style (,stream (clim:make-text-style (first *font-fixed*)
                                                        (second *font-fixed*)
                                                        *font-size*))
     ,@body))

(defmacro with-font-size ((stream size) &body body)
  (alexandria:once-only (stream size)
    `(let ((*font-size* ,size))
       (clim:with-text-size (,stream *font-size*)
         ,@body))))

(defmacro with-font-size-change ((stream mod) &body body)
  (alexandria:once-only (stream mod)
    `(let ((*font-size* (max (* *font-size* ,mod) 10)))
       (clim:with-text-size (,stream *font-size*)
         ,@body))))

(defun %aligned-render-and-move (stream pos fn)
  (let ((output-record (clim:with-output-to-output-record (stream)
                         (funcall fn))))
    (multiple-value-bind (w)
        (clim:rectangle-size output-record)
      (move-rec output-record pos 0)
      (clim:stream-add-output-record stream output-record)
      (+ pos w))))

(defmacro with-aligned-rendering ((stream) &body body)
  `(let ((*aligned-rendering-pos* 0)
         (*aligned-rendering-stream* ,stream))
     ,@body))

(defmacro render-aligned (() &body body)
  `(setf *aligned-rendering-pos* (%aligned-render-and-move *aligned-rendering-stream* *aligned-rendering-pos*
                                                           (lambda () ,@body))))

(defun aligned-spacing (spacing)
  (incf *aligned-rendering-pos* (* (char-width *aligned-rendering-stream*) spacing)))

(defun render-aligned-string (fmt &rest args)
  (render-aligned ()
    (clim:draw-text* *aligned-rendering-stream* (apply #'format nil fmt args) 0 0)))

(defun render-formatted (stream fmt &rest args)
  (with-aligned-rendering (stream)
    (apply #'render-aligned-string fmt args)))

(defmacro with-paren-op (&body body)
  `(let ((*lop* 'maxima::mparen)
         (*rop* 'maxima::mparen))
     ,@body))

(defmacro iterate-exprs ((sym exprs op &key first-sym) &body body)
  (alexandria:with-gensyms (run-body p v first)
    (alexandria:once-only (exprs op)
      `(labels ((,run-body (,p ,first)
                  (let ((,sym ,p))
                    ,(if first-sym
                         `(let ((,first-sym ,first))
                            ,@body)
                         `(progn ,@body)))))
         (when ,exprs
           (cond ((and (car ,exprs)
                       (null (cdr ,exprs)))
                  (,run-body (car ,exprs) t))
                 (t
                  (let ((*rop* ,op))
                    (,run-body (car ,exprs) t))
                  (let ((*lop* ,op))
                    (loop
                      for ,v on (cdr ,exprs)
                      if (cdr ,v)
                        do (let ((*rop* ,op))
                             (,run-body (car ,v) nil))
                      else
                        do (,run-body (car ,v) nil))))))))))

(defun char-height (stream)
  (multiple-value-bind (width height)
      (clim:text-size stream "M")
    (declare (ignore width))
    height))

(defun char-width (stream)
  (multiple-value-bind (width height)
      (clim:text-size stream "M")
    (declare (ignore height))
    width))

(defun char-descent (stream char)
  (multiple-value-bind (width height x y baseline)
      (clim:text-size stream (format nil "~c" char))
    (declare (ignore width x y))
    (- height baseline)))

(defun render-quotient (stream top-expr bottom-expr)
  (let ((fraction-spacing 2)
        (top (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream top-expr)))
        (bottom (clim:with-output-to-output-record (stream)
                  (render-maxima-expression stream bottom-expr))))
    (dimension-bind (top :width top-width :height top-height)
      (dimension-bind (bottom :width bottom-width)
        (let* ((max-width (max top-width bottom-width))
               (y (+ top-height fraction-spacing))
               (centre (+ y (/ (char-height stream) 2))))
          (set-rec-position top
                            (/ (- max-width top-width) 2)
                            (- centre))
          (clim:stream-add-output-record stream top)
          (set-rec-position bottom
                            (/ (- max-width bottom-width) 2)
                            (- (+ top-height (+ (* fraction-spacing 2) 1)) centre))
          (clim:stream-add-output-record stream bottom)
          (clim:draw-line* stream 0 (- y centre) max-width (- y centre)))))))

(defun render-symbol (stream sym &key roman-font)
  (case sym
    (maxima::$inf (render-formatted stream "~c" #\INFINITY))
    (maxima::$%pi (render-formatted stream "~c" #\GREEK_SMALL_LETTER_PI))
    (t (let ((n (format-sym-name sym)))
         (if (or (eql (aref n 0) #\$)
                 (eql (aref n 0) #\%))
             (with-font (stream (if roman-font *font-roman* *font-italic*))
               (render-formatted stream "~a" (subseq n 1)))
             (render-formatted stream "~s" sym))))))

(defun render-negation (stream expr spacing)
  (with-aligned-rendering (stream)
    (render-aligned-string "~c" #\MINUS_SIGN)
    (aligned-spacing spacing)
    (let ((*lop* 'maxima::mminus))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-plus (stream exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs 'maxima::mplus :first-sym first)
      (unless first
        (aligned-spacing 0.2))
      (cond ((and (listp expr)
                  (alexandria:length= (length expr) 2)
                  (listp (car expr))
                  (eq (caar expr) 'maxima::mminus))
             (render-aligned () (render-negation stream (second expr) 0.2)))
            (t
             (unless first
               (render-aligned-string "+")
               (aligned-spacing 0.2))
             (render-aligned () (render-maxima-expression stream expr)))))))

(defun render-times (stream exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs 'maxima::mtimes :first-sym first)
      (unless first
        (render-aligned-string "~c" #\MIDDLE_DOT))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-expt (stream a b)
  (let ((base (clim:with-output-to-output-record (stream)
                (let ((*rop* 'maxima::mexpt))
                 (render-maxima-expression stream a))))
        (exp (clim:with-output-to-output-record (stream)
               (with-font-size-change (stream 2/3)
                 (let ((*lop* 'maxima::mexpt))
                   (render-maxima-expression stream b))))))
    (dimension-bind (base :height base-height :y base-y :right base-right)
      (dimension-bind (exp :height exp-height)
        (clim:stream-add-output-record stream (make-boxed-output-record stream base))
        (set-rec-position exp base-right
                          (if (>= exp-height (/ base-height 2))
                              ;; exp is high enough that it needs to have the bottom aligned to
                              ;; the middle of base
                              (- (+ base-y (/ base-height 2)) exp-height)
                              ;; ELSE: the top of exp should be slightly above the top of base
                              ;; For now, just align them
                              base-y))
        (clim:stream-add-output-record stream (make-boxed-output-record stream exp))))))

(defun render-plain (stream spacing ch a b)
  (with-aligned-rendering (stream)
    (render-aligned () (render-maxima-expression stream a))
    (incf *aligned-rendering-pos* spacing)
    (render-aligned-string "~c" ch)
    (incf *aligned-rendering-pos* spacing)
    (render-aligned () (render-maxima-expression stream b))))

(defun render-equal (stream a b)
  (render-plain stream 4 #\= a b))

(defun wrap-with-parens (stream output-record)
  (dimension-bind (output-record :x x :y y :width width :height height)
    (destructuring-bind (left-paren left-paren-ascent left-paren-descent)
        (with-roman-text-style (stream height)
          (render-and-measure-string stream "("))
      (let ((right-paren (clim:with-output-to-output-record (stream)
                           (with-roman-text-style (stream height)
                             (clim:draw-text* stream ")" 0 0)))))
        (dimension-bind (left-paren :width left-paren-width)
          (let* ((centre (+ (/ height 2) y))
                 (p-centre (- left-paren-descent
                              (/ (+ left-paren-ascent left-paren-descent) 2)))
                 (p-offset (- centre p-centre)))
            (move-rec left-paren x p-offset)
            (clim:stream-add-output-record stream left-paren)
            ;;
            (move-rec output-record left-paren-width 0)
            (clim:stream-add-output-record stream output-record)
            ;;
            (move-rec right-paren (+ x left-paren-width width) p-offset)
            (clim:stream-add-output-record stream right-paren)))))))

(defmacro with-wrapped-parens ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (rec)
      `(let ((,rec (clim:with-output-to-output-record (,stream)
                     (with-paren-op
                       ,@body))))
         (wrap-with-parens ,stream ,rec)))))

(defun render-param-list (stream prefix-renderer params)
  (with-aligned-rendering (stream)
    (render-aligned () (funcall prefix-renderer stream))
    (let ((params (clim:with-output-to-output-record (stream)
                    (with-aligned-rendering (stream)
                      (loop
                        for expr in params
                        for first = t then nil
                        unless first
                          do (render-aligned-string ", ")
                        do (render-aligned () (render-maxima-expression stream expr)))))))
      (render-aligned () (wrap-with-parens stream params)))))

(defun render-function (stream name exprs)
  (render-param-list stream
                     (lambda (stream) (render-symbol stream (car name) :roman-font t))
                     exprs))

(defun render-and-measure-string (stream string &optional (x 0) (y 0))
  (multiple-value-bind (width height final-x final-y baseline)
      (clim:text-size stream string)
    (declare (ignore width final-x final-y))
    (let ((rec (clim:with-output-to-output-record (stream)
                 (clim:draw-text* stream string x y))))
      (list rec baseline (- height baseline)))))

(defun render-intsum-inner (stream f var from to symbol sym2)
  (let* ((bottom (clim:with-output-to-output-record (stream)
                   (with-aligned-rendering (stream)
                     (with-paren-op
                       (when var
                         (render-aligned () (render-maxima-expression stream var))
                         (render-aligned () (render-formatted stream "=")))
                       (render-aligned () (render-maxima-expression stream from))))))
         (top    (clim:with-output-to-output-record (stream)
                   (with-paren-op
                     (render-maxima-expression stream to))))
         (exp    (clim:with-output-to-output-record (stream)
                   (let ((*lop* 'maxima::%sum)
                         (*rop* 'maxima::mparen))
                     (render-maxima-expression stream f)))))
    (dimension-bind (exp :height exp-height)
      (destructuring-bind (sigma sigma-ascent sigma-descent)
          (clim:with-text-size (stream (+ 10 exp-height))
            (render-and-measure-string stream (format nil "~c" symbol)))
        (dimension-bind (sigma :width sigma-width :height sigma-height :right sigma-right :x sigma-x :y sigma-y)
          (let ((centre (+ (/ sigma-height 2)
                           (/ (char-height stream) 2)
                           sigma-y)))
            ;; centre is the y coordinate where the baseline should be located, so we move sigma
            ;; it is a negative value indicating the number of pixels above the baseline of the sigma itself
            (move-rec sigma 0 (- centre))
            (clim:stream-add-output-record stream (make-boxed-output-record stream sigma))
            ;;
            (dimension-bind (bottom :width bottom-width)
              (set-rec-position bottom
                                (+ sigma-x (/ (- sigma-width bottom-width) 2))
                                ;; centre is negative, so subtracting that value from sigma-descent
                                ;; moves the bottom rec downards to compensate for the adjustment of sigma
                                (- sigma-descent centre))
              (clim:stream-add-output-record stream (make-boxed-output-record stream bottom)))
            ;;
            (dimension-bind (top :width top-width :height top-height)
              (set-rec-position top
                                (/ (- sigma-width top-width) 2)
                                (- (- sigma-ascent) centre top-height))
              (clim:stream-add-output-record stream (make-boxed-output-record stream top)))
            ;;
            (set-rec-position exp (+ sigma-right 2) nil)
            (clim:stream-add-output-record stream (make-boxed-output-record stream exp))
            ;;
            (when sym2
              (let ((variable (clim:with-output-to-output-record (stream)
                                (with-aligned-rendering (stream)
                                  (aligned-spacing 0.5)
                                  (render-aligned () (with-italic-text-style (stream)
                                                       (render-formatted stream "d")))
                                  (render-aligned () (render-maxima-expression stream sym2))))))
                (dimension-bind (exp :right x)
                  (move-rec variable (+ x 2) 0)
                  (clim:stream-add-output-record stream variable))))))))))

(defun render-intsum (stream f var from to symbol sym2)
  (if (> (maxima::lbp *rop*) (maxima::rbp 'maxima::mparen))
      (let ((rec (clim:with-output-to-output-record (stream)
                   (render-intsum-inner stream f var from to symbol sym2))))
        (wrap-with-parens stream rec))
      (render-intsum-inner stream f var from to symbol sym2)))

(defun render-sum (stream f var from to)
  (render-intsum stream f var from to #\GREEK_CAPITAL_LETTER_SIGMA nil))

(defun render-integrate (stream f var from to)
  (render-intsum stream f nil from to #\INTEGRAL var))

(defun render-product (stream f var from to)
  (render-intsum stream f var from to #\GREEK_CAPITAL_LETTER_PI nil))

(defun render-sqrt (stream expr)
  (let ((exp (clim:with-output-to-output-record (stream)
               (let ((*lop* 'maxima::mparen))
                 (render-maxima-expression stream expr)))))
    (dimension-bind (exp :height height :x x :y y :bottom bottom :right right)
      (let* ((angle 0.2)
             (hg 0.4)
             (hg-angle 0.4)
             (hg-height (* height hg))
             (x-offset (+ (* height angle) (* hg-height hg-angle))))
        (clim:draw-design stream (clim:make-polyline* (list x (- bottom hg-height)
                                                            (+ x (* hg-height hg-angle)) bottom
                                                            (+ x x-offset) y
                                                            (+ right x-offset) y))
                          :line-thickness 1)
        (move-rec exp x-offset 0)
        (clim:stream-add-output-record stream exp)))))

(defun render-mlist (stream exprs)
  (with-aligned-rendering (stream)
    (render-aligned () (render-formatted stream "["))
    (aligned-spacing 0.5)
    (loop
      for expr in exprs
      for first = t then nil
      unless first
        do (render-aligned-string ", ")
      do (render-aligned () (render-maxima-expression stream expr)))
    (aligned-spacing 0.5)
    (render-aligned () (render-formatted stream "]"))))

(defun render-string (stream string)
  (with-fix-text-style (stream)
    (render-formatted stream "~s" string)))

(defun render-mdefine (stream f definition)
  (let ((function-name (car f))
        (function-args (cdr f)))
    ;; FUNCTION-NAME should always be a list of a single symbol which
    ;; is the name of the function. For now, we'll just bail if it
    ;; isn't, but perhaps there are situations where some more complex
    ;; stucture can be passed in. If that's the case, we should
    ;; probably just call RENDER-MAXIMA-EXPRESSION on it.
    (with-aligned-rendering (stream)
      (render-aligned () (render-param-list stream
                                            (lambda (stream)
                                              (unless (and (alexandria:sequence-of-length-p function-name 1)
                                                           (symbolp (car function-name)))
                                                (error "Unexpected content in function name: ~s" function-name))
                                              (render-symbol stream (car function-name) :roman-font t))
                                            function-args))
      (aligned-spacing 0.5)
      (render-aligned-string ":=")
      (aligned-spacing 0.5)
      (render-aligned () (render-maxima-expression stream definition)))))

(defun render-maxima-expression (stream expr)
  (labels ((render-inner (fixed)
             (case (caar fixed)
               (maxima::mlist (render-mlist stream (cdr fixed)))
               (maxima::mquotient (render-quotient stream (second fixed) (third fixed)))
               (maxima::rat (render-quotient stream (second fixed) (third fixed)))
               (maxima::mplus (render-plus stream (cdr fixed)))
               (maxima::mminus (render-negation stream (second fixed) 0.2))
               (maxima::mtimes (render-times stream (cdr fixed)))
               (maxima::mexpt (render-expt stream (second fixed) (third fixed)))
               (maxima::mequal (render-equal stream (second fixed) (third fixed)))
               (maxima::mdefine (render-mdefine stream (second fixed) (third fixed)))
               ((maxima::%sum maxima::$sum) (render-sum stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               ((maxima::%integrate maxima::$integrate) (render-integrate stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               ((maxima::%product maxima::$product) (render-product stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               (maxima::%sqrt (render-sqrt stream (second fixed)))
               (t (render-function stream (car fixed) (cdr fixed))))))
    (let ((fixed (maxima::nformat-check expr)))
      (log:info "Calling render expression on: ~s (lop=~a rop=~a)" fixed *lop* *rop*)
      (etypecase fixed
        (number (render-formatted stream "~a" fixed))
        (symbol (render-symbol stream fixed))
        (string (render-string stream fixed))
        (list (if (or (<= (maxima::lbp (caar fixed)) (maxima::rbp *lop*))
                      (<= (maxima::rbp (caar fixed)) (maxima::lbp *rop*)))
                  (with-wrapped-parens (stream)
                    (render-inner fixed))
                  (render-inner fixed)))))))

(defmacro make-rendered-output-record ((stream) &body body)
  (alexandria:with-gensyms (output-record)
    `(let ((*font-size* 14))
       (with-roman-text-style (stream)
         (let ((,output-record (clim:with-output-to-output-record (,stream)
                                 (with-paren-op
                                   ,@body))))
           (setf (clim:output-record-position ,output-record) (values 0 0))
           ,output-record)))))

(defun make-expression-output-record (stream expr)
  (log:info "Making output record for expr: ~s" expr)
  (make-rendered-output-record (stream)
    (render-maxima-expression stream expr)))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view maxima-renderer-view) &key)
  (let* ((expr (maxima-native-expr/expr obj))
         (output-record (make-expression-output-record stream expr)))
    (clim:with-room-for-graphics (stream)
      (clim:with-identity-transformation (stream)
        (clim:stream-add-output-record stream output-record)))))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view clim:textual-view) &key)
  (format stream "~a" (maxima-native-expr/src obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-error-message (stream message)
  (let ((size (clim:text-style-size (clim:medium-text-style stream))))
    (clim:surrounding-output-with-border (stream :ink clim:+black+ :line-thickness 1)
      (clim:with-drawing-options (stream :ink clim:+red+ :text-style (clim:make-text-style :fix :roman size))
        (format stream "~a~%" message)))))
