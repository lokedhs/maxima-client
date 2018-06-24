(in-package :maxima-client)

(defvar maxima::$font_size 14)

(defvar *font-roman* '("MathJax_Main" "Regular"))
(defvar *font-roman-math* '("MathJax_Math" "Regular"))
(defvar *font-italic* '("MathJax_Main" "Italic"))
(defvar *font-fixed* '("Source Code Pro" "Regular"))
(defvar *font-sigma* '("MathJax_Main" "Regular"))
(defvar *font-integrate-size1* '("MathJax_Size1" "Regular"))
(defvar *font-integrate-size2* '("MathJax_Size2" "Regular"))
(defvar *font-product* '("MathJax_Main" "Regular"))
(defvar *font-paren-size3* '("MathJax_Size3" "Regular"))
(defvar *font-paren-size4* '("MathJax_Size4" "Regular"))
(defvar *draw-boxes* nil)

(defvar *aligned-rendering-pos*)
(defvar *aligned-rendering-stream*)
(defvar *font-size*)
(defvar *rop*)
(defvar *lop*)
(defvar *inhibit-presentations* nil)

(defclass maxima-renderer-view ()
  ())

(defclass maxima-interactor-view (maxima-renderer-view clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

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

(defun %render-quotient (stream top-expr bottom-expr)
  (let ((fraction-spacing 2)
        (top (clim:with-output-to-output-record (stream)
               (funcall top-expr stream)))
        (bottom (clim:with-output-to-output-record (stream)
                  (funcall bottom-expr stream))))
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

(defun render-quotient (stream top-expr bottom-expr)
  (%render-quotient stream
                    (lambda (stream)
                      (render-maxima-expression stream top-expr))
                    (lambda (stream)
                      (render-maxima-expression stream bottom-expr))))

(defun render-number (stream n)
  (render-formatted stream "~a" n))

(defun render-symbol (stream sym &key roman-font)
  (case sym
    (maxima::$inf (render-formatted stream "~c" #\INFINITY))
    (maxima::$%pi (with-font (stream *font-roman-math*) (render-formatted stream "~c" #\GREEK_SMALL_LETTER_PI)))
    (maxima::$%lambda (with-font (stream *font-roman-math*) (render-formatted stream "~c" #\GREEK_SMALL_LETTER_LAMBDA)))
    (t (with-font (stream (if roman-font *font-roman* *font-italic*))
         (render-formatted stream "~a" (format-sym-name sym))))))

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
        (aligned-spacing 0.4))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-mnctimes (stream exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs 'maxima::mnctimes :first-sym first)
      (unless first
        (aligned-spacing 0.2)
        (render-aligned-string "~c" #\DOT_OPERATOR)
        (aligned-spacing 0.2))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun %render-expt (stream fn-a fn-b)
  (let ((base (clim:with-output-to-output-record (stream)
                (funcall fn-a stream)))
        (exp (clim:with-output-to-output-record (stream)
               (with-font-size-change (stream 0.8)
                 (funcall fn-b stream)))))
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

(defun render-expt (stream a b)
  (%render-expt stream
                (lambda (stream)
                  (let ((*rop* 'maxima::mexpt))
                    (render-maxima-expression stream a)))
                (lambda (stream)
                  (let ((*lop* 'maxima::mexpt))
                    (render-maxima-expression stream b)))))

(defun render-plain (stream spacing ch a b)
  (with-aligned-rendering (stream)
    (render-aligned () (render-maxima-expression stream a))
    (incf *aligned-rendering-pos* spacing)
    (render-aligned-string "~c" ch)
    (incf *aligned-rendering-pos* spacing)
    (render-aligned () (render-maxima-expression stream b))))

(defun render-equal (stream a b)
  (render-plain stream 4 #\= a b))

(defun find-paren-font (size)
  (cond ((< size 16)
         (list *font-roman* size))
        ((< size 80)
         (list *font-paren-size3* (* size 0.4)))
        (t
         (list *font-paren-size4* (* size 0.35)))))

(defun wrap-with-parens (stream output-record
                         &key (left-paren "(") (right-paren ")") (left-spacing 0) (right-spacing 0))
  (dimension-bind (output-record :x x :y y :width width :height height)
    (destructuring-bind (paren-font paren-size)
        (find-paren-font height)
      (destructuring-bind (left-paren left-paren-ascent left-paren-descent)
          (with-font (stream paren-font paren-size)
            (render-and-measure-string stream left-paren))
        (let ((right-paren (clim:with-output-to-output-record (stream)
                             (with-font (stream paren-font paren-size)
                               (clim:draw-text* stream right-paren 0 0)))))
          (dimension-bind (left-paren :width left-paren-width)
            (let* ((centre (+ (/ height 2) y))
                   (p-centre (- left-paren-descent
                                (/ (+ left-paren-ascent left-paren-descent) 2)))
                   (p-offset (- centre p-centre)))
              (move-rec left-paren x p-offset)
              (clim:stream-add-output-record stream left-paren)
              ;;
              (move-rec output-record (+ left-paren-width left-spacing) 0)
              (clim:stream-add-output-record stream output-record)
              ;;
              (move-rec right-paren (+ x left-paren-width width left-spacing right-spacing) p-offset)
              (clim:stream-add-output-record stream right-paren))))))))

(defmacro with-wrapped-parens ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (rec)
      `(let ((,rec (clim:with-output-to-output-record (,stream)
                     (with-paren-op
                       ,@body))))
         (wrap-with-parens ,stream ,rec)))))

(defmacro with-wrapped-optional-parens ((stream enabled-p) &body body)
  (alexandria:once-only (stream enabled-p)
    (alexandria:with-gensyms (fn)
      `(flet ((,fn () ,@body))
         (if ,enabled-p
             (with-wrapped-parens (,stream)
               (,fn))
             (,fn))))))

(defun render-arg-list (stream params &key (spacing 0.5))
  (with-aligned-rendering (stream)
    (loop
      for expr in params
      for first = t then nil
      unless first
        do (progn
             (render-aligned-string ",")
             (aligned-spacing spacing))
      do (render-aligned ()
           (with-paren-op
             (render-maxima-expression stream expr))))))

(defun render-param-list (stream params &key prefix-renderer (spacing 0.5))
  (with-aligned-rendering (stream)
    (when prefix-renderer
      (render-aligned () (funcall prefix-renderer stream)))
    (let ((params (clim:with-output-to-output-record (stream)
                    (render-arg-list stream params :spacing spacing))))
      (render-aligned () (wrap-with-parens stream params)))))

(defun render-function (stream name exprs &key paren-p)
  (log:trace "Render function. name=~s, exprs=~s" name exprs)
  (render-param-list stream exprs
                     :prefix-renderer (lambda (stream)
                                        (with-wrapped-optional-parens (stream paren-p)
                                          (render-symbol stream name :roman-font t)))))

(defun render-and-measure-string (stream string &optional (x 0) (y 0))
  (multiple-value-bind (width height final-x final-y baseline)
      (clim:text-size stream string)
    (declare (ignore width final-x final-y))
    (let ((rec (clim:with-output-to-output-record (stream)
                 (clim:draw-text* stream string x y))))
      (list rec baseline (- height baseline)))))

(defun render-intsum-inner (stream f var from to symbol sym2 font-fn)
  (let ((exp (clim:with-output-to-output-record (stream)
               (let ((*lop* 'maxima::%sum)
                     (*rop* 'maxima::mparen))
                 (render-maxima-expression stream f)))))
    (dimension-bind (exp :height exp-height)
      (destructuring-bind (sigma sigma-ascent sigma-descent)
          (destructuring-bind (sigma-font sigma-size)
              (funcall font-fn exp-height)
            (with-font (stream sigma-font sigma-size)
              (render-and-measure-string stream (format nil "~c" symbol))))
        (dimension-bind (sigma :width sigma-width :height sigma-height :right sigma-right :x sigma-x :y sigma-y)
          (let ((centre (+ (/ sigma-height 2)
                           (/ (char-height stream) 2)
                           sigma-y)))
            ;; centre is the y coordinate where the baseline should be located, so we move sigma
            ;; it is a negative value indicating the number of pixels above the baseline of the sigma itself
            (move-rec sigma 0 (- centre))
            (clim:stream-add-output-record stream (make-boxed-output-record stream sigma))
            ;;
            (when from
              (let ((bottom (clim:with-output-to-output-record (stream)
                              (with-font-size-change (stream 0.8)
                                (with-aligned-rendering (stream)
                                  (with-paren-op
                                    (when var
                                      (render-aligned () (render-maxima-expression stream var))
                                      (render-aligned () (render-formatted stream "=")))
                                    (render-aligned () (render-maxima-expression stream from))))))))
                (dimension-bind (bottom :width bottom-width)
                  (set-rec-position bottom
                                    (+ sigma-x (/ (- sigma-width bottom-width) 2))
                                    ;; centre is negative, so subtracting that value from sigma-descent
                                    ;; moves the bottom rec downards to compensate for the adjustment of sigma
                                    (- sigma-descent centre))
                  (clim:stream-add-output-record stream (make-boxed-output-record stream bottom)))))
            ;;
            (when to
              (let ((top (clim:with-output-to-output-record (stream)
                           (with-font-size-change (stream 0.8)
                             (with-paren-op
                               (render-maxima-expression stream to))))))
                (dimension-bind (top :width top-width :height top-height)
                  (set-rec-position top
                                    (/ (- sigma-width top-width) 2)
                                    (- (- sigma-ascent) centre top-height))
                  (clim:stream-add-output-record stream (make-boxed-output-record stream top)))))
            ;;
            (set-rec-position exp (+ sigma-right (/ (char-width stream) 2)) nil)
            (clim:stream-add-output-record stream (make-boxed-output-record stream exp))
            ;;
            (when sym2
              (let ((variable (clim:with-output-to-output-record (stream)
                                (with-aligned-rendering (stream)
                                  (render-aligned () (with-roman-text-style (stream)
                                                       (render-formatted stream "d")))
                                  (render-aligned () (render-maxima-expression stream sym2))))))
                (dimension-bind (exp :right x)
                  (move-rec variable (+ x (/ (char-width stream) 2)) 0)
                  (clim:stream-add-output-record stream variable))))))))))

(defun render-intsum (stream f var from to symbol sym2 font-fn)
  (if (> (maxima::lbp *rop*) (maxima::rbp 'maxima::mparen))
      (let ((rec (clim:with-output-to-output-record (stream)
                   (render-intsum-inner stream f var from to symbol sym2 font-fn))))
        (wrap-with-parens stream rec))
      (render-intsum-inner stream f var from to symbol sym2 font-fn)))

(defun find-integrate-font (size)
  (log:info "Finding integrate font: ~s" size)
  (let ((adjusted-size (+ size 10)))
    (cond ((< adjusted-size 65)
           (list *font-integrate-size1* (* adjusted-size 0.7)))
          (t
           (list *font-integrate-size2* (* adjusted-size 0.4))))))

(defun render-sum (stream f var from to)
  (render-intsum stream f var from to #\GREEK_CAPITAL_LETTER_SIGMA nil (lambda (size) (list *font-sigma* (max size 40)))))

(defun render-integrate (stream f var from to)
  (render-intsum stream f nil from to #\INTEGRAL var #'find-integrate-font))

(defun render-product (stream f var from to)
  (render-intsum stream f var from to #\GREEK_CAPITAL_LETTER_PI nil (lambda (size) (list  *font-product* (max size 40)))))

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

(defun render-mdefine (stream f definition symbol)
  (let ((function-name (car f))
        (function-args (cdr f)))
    ;; FUNCTION-NAME should always be a list of a single symbol which
    ;; is the name of the function. For now, we'll just bail if it
    ;; isn't, but perhaps there are situations where some more complex
    ;; stucture can be passed in. If that's the case, we should
    ;; probably just call RENDER-MAXIMA-EXPRESSION on it.
    (with-aligned-rendering (stream)
      (render-aligned ()
        (render-param-list stream function-args
                           :prefix-renderer (lambda (stream)
                                              (unless (and (alexandria:sequence-of-length-p function-name 1)
                                                           (symbolp (car function-name)))
                                                (error "Unexpected content in function name: ~s" function-name))
                                              (render-symbol stream (car function-name) :roman-font t))))
      (aligned-spacing 0.5)
      (render-aligned-string symbol)
      (aligned-spacing 0.5)
      (render-aligned () (render-maxima-expression stream definition)))))

(defun render-limit (stream expr sym to direction)
  (log:trace "rendering limit: expr=~s, sym=~s, to=~s, direction=~s" expr sym to direction)
  (destructuring-bind (top top-ascent top-descent)
      (with-roman-text-style (stream)
        (render-and-measure-string stream "lim"))
    (declare (ignore top-ascent))
    (let* ((bottom (clim:with-output-to-output-record (stream)
                     (with-font-size-change (stream 0.8)
                       (with-aligned-rendering (stream)
                         (render-aligned () (render-maxima-expression stream sym))
                         (aligned-spacing 0.5)
                         (render-aligned-string "~c" #\RIGHTWARDS_ARROW)
                         (aligned-spacing 0.5)
                         (render-aligned () (render-maxima-expression stream to))
                         (when direction
                           (render-aligned-string (case direction
                                                    (maxima::$plus "+")
                                                    (maxima::$minus (format nil "~c" #\MINUS_SIGN))
                                                    (t (format nil "~a" direction))))))))))
      (dimension-bind (top :width top-width)
        (dimension-bind (bottom :width bottom-width)
          (let ((lim-rec (clim:with-output-to-output-record (stream)
                           (let ((horiz-centre (/ (max top-width bottom-width) 2)))
                             (set-rec-position top (- horiz-centre (/ top-width 2)) nil)
                             (set-rec-position bottom (- horiz-centre (/ bottom-width 2)) top-descent))
                           (clim:stream-add-output-record stream top)
                           (clim:stream-add-output-record stream bottom)))
                (rec (clim:with-output-to-output-record (stream)
                       (render-maxima-expression stream expr))))
            (dimension-bind (lim-rec :right lim-rec-right)
              (clim:stream-add-output-record stream lim-rec)
              (move-rec rec (+ lim-rec-right (/ (char-width stream) 2)) 0)
              (clim:stream-add-output-record stream rec))))))))

(defun render-matrix (stream rows)
  (let ((rec (clim:with-output-to-output-record (stream)
               (clim:formatting-table (stream :x-spacing (char-width stream) :y-spacing (char-height stream))
                 (loop
                   for row in rows
                   do (clim:formatting-row (stream)
                        (loop
                          for col in (maxima-list-to-list row)
                          do (clim:formatting-cell (stream :align-y :center :align-x :center)
                               (with-paren-op
                                 (render-maxima-expression stream col))))))))))
    (wrap-with-parens stream rec :left-paren "[" :right-paren "]"
                                 :left-spacing (char-width stream) :right-spacing (char-width stream))))

(defun render-msetq (stream a b)
  (with-aligned-rendering (stream)
    (render-aligned () (render-maxima-expression stream a))
    (aligned-spacing 0.1)
    (render-aligned-string ":")
    (aligned-spacing 0.1)
    (render-aligned () (render-maxima-expression stream b))))

(defun render-mabs (stream expr)
  (let ((rec (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream expr))))
    (dimension-bind (rec :x x :y y :right right :bottom bottom)
      (let ((spacing (* (char-width stream) 0.2)))
        (clim:draw-line* stream 0 y 0 bottom)
        (move-rec rec (+ spacing (- x)) 0)
        (clim:stream-add-output-record stream rec)
        (let ((line-x (+ (- right x) (* spacing 2))))
          (clim:draw-line* stream line-x y line-x bottom))))))

(defun render-derivative (stream expr sym exp)
  (let ((compressed-form-p (symbolp expr)))
    (labels ((render-sym (stream bottom-sym-p)
               (with-aligned-rendering (stream)
                 (render-aligned () (render-symbol stream '$d))
                 (when bottom-sym-p
                   (render-aligned () (render-symbol stream sym)))))
             (render-sym-expt (stream bottom-sym-p)
               (if (eql exp 1)
                   (render-sym stream bottom-sym-p)
                   (%render-expt stream
                                 (lambda (stream)
                                   (render-sym stream bottom-sym-p))
                                 (lambda (stream)
                                   (render-maxima-expression stream exp))))))
      (with-aligned-rendering (stream)
        (render-aligned () (let ((*inhibit-presentations* t))
                             (%render-quotient stream
                                               (lambda (stream)
                                                 (with-aligned-rendering (stream)
                                                   (render-aligned () (render-sym-expt stream nil))
                                                   (when compressed-form-p
                                                     (render-aligned () (render-symbol stream expr)))))
                                               (lambda (stream)
                                                 (render-sym-expt stream t)))))
        (unless compressed-form-p
          (aligned-spacing 0.5)
          (with-wrapped-parens (stream)
            (render-aligned () (render-maxima-expression stream expr))))))))

#+nil
(defun xrender-array-reference (stream expr args &key paren-p)
  (unless args
    (error "Arg list should have at least one element"))
  (let ((base (clim:with-output-to-output-record (stream)
                (with-wrapped-optional-parens (stream paren-p)
                  (render-maxima-expression stream expr))))
        (args-rec (clim:with-output-to-output-record (stream)
                    (with-font-size-change (stream 0.8)
                      (render-arg-list stream args :spacing 0)))))
    (dimension-bind (base :height base-height :y base-y :right base-right :bottom base-bottom)
      (dimension-bind (args-rec :height args-height)
        (clim:stream-add-output-record stream (make-boxed-output-record stream base))
        (set-rec-position args-rec base-right
                          (if (>= args-height (/ base-height 2))
                              (+ base-y (/ base-height 2))
                              (- base-bottom (/ args-))))
        (clim:stream-add-output-record stream args-rec)))))

(defun render-array-reference (stream expr args &key paren-p)
  (unless args
    (error "Arg list should have at least one element"))
  (let ((rec (clim:with-output-to-output-record (stream)
               (with-wrapped-optional-parens (stream paren-p)
                 (render-maxima-expression stream expr)))))
    (clim:stream-add-output-record stream rec)
    (dimension-bind (rec :y y :right right :bottom bottom :height height)
      (with-font-size-change (stream 0.8)
        (let ((args-rec (clim:with-output-to-output-record (stream)
                          (render-arg-list stream args :spacing 0))))
          (dimension-bind (args-rec :x args-x :y args-y :bottom args-bottom :height args-height)
            (move-rec args-rec (- right args-x)
                      (if (>= args-height (/ height 2))
                          (- (+ y (/ height 2)) args-y)
                          (let ((args-centre (/ (+ args-y args-bottom) 2)))
                            (- bottom args-centre))))
            (clim:stream-add-output-record stream args-rec)))))))

(defun render-function-or-array-ref (stream arrayref-p paren-p expr args)
  (if arrayref-p
      (render-array-reference stream expr args :paren-p paren-p)
      (render-function stream expr args :paren-p paren-p)))

(defun render-maxima-expression (stream expr &optional toplevel-p)
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
               (maxima::mdefine (render-mdefine stream (second fixed) (third fixed) ":="))
               (maxima::mdefmacro (render-mdefine stream (second fixed) (third fixed) "::="))
               ((maxima::%sum maxima::$sum) (render-sum stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               ((maxima::%integrate maxima::$integrate) (render-integrate stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               ((maxima::%product maxima::$product) (render-product stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               (maxima::%sqrt (render-sqrt stream (second fixed)))
               ((maxima::%limit maxima::$limit) (render-limit stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               (maxima::$matrix (render-matrix stream (cdr fixed)))
               (maxima::mprog (render-function stream '(maxima::$block) (cdr fixed)))
               (maxima::msetq (render-msetq stream (second fixed) (third fixed)))
               (maxima::mabs (render-mabs stream (second fixed)))
               (maxima::%derivative (render-derivative stream (second fixed) (third fixed) (fourth fixed)))
               (maxima::mqapply (render-function-or-array-ref stream (member 'maxima::array (car fixed)) t (second fixed) (cddr fixed)))
               (maxima::mnctimes (render-mnctimes stream (cdr fixed)))
               (t (render-function-or-array-ref stream (member 'maxima::array (car fixed)) nil (caar fixed) (cdr fixed)))))
           (render-with-presentation (fixed)
             (if (or toplevel-p *inhibit-presentations*)
                 (render-inner fixed)
                 (clim:with-output-as-presentation (stream (make-instance 'maxima-native-expr :expr fixed)
                                                           'maxima-native-expr
                                                           :view (clim:stream-default-view stream)
                                                           :allow-sensitive-inferiors t)
                   (render-inner fixed)))))
    (let ((fixed (maxima::nformat-check expr)))
      (log:trace "Calling render expression on: ~s (lop=~a rop=~a)" fixed *lop* *rop*)
      (etypecase fixed
        (number (render-number stream fixed))
        (symbol (render-symbol stream fixed))
        (string (render-string stream fixed))
        (list (if (or (<= (maxima::lbp (caar fixed)) (maxima::rbp *lop*))
                      (<= (maxima::rbp (caar fixed)) (maxima::lbp *rop*)))
                  (with-wrapped-parens (stream)
                    (render-with-presentation fixed))
                  (render-with-presentation fixed)))))))

(defmacro make-rendered-output-record ((stream) &body body)
  (alexandria:with-gensyms (output-record)
    `(let ((*font-size* maxima::$font_size))
       (with-roman-text-style (stream)
         (let ((,output-record (clim:with-output-to-output-record (,stream)
                                 (with-paren-op
                                   ,@body))))
           (setf (clim:output-record-position ,output-record) (values 0 0))
           ,output-record)))))

(defun make-expression-output-record (stream expr)
  (log:trace "Making output record for expr: ~s" expr)
  (make-rendered-output-record (stream)
    (render-maxima-expression stream expr t)))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view maxima-renderer-view) &key)
  (let* ((expr (maxima-native-expr/expr obj))
         (output-record (make-expression-output-record stream expr)))
    (clim:with-room-for-graphics (stream)
      (clim:with-identity-transformation (stream)
        (clim:stream-add-output-record stream output-record)))))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) stream (view clim:textual-view) &key)
  (format stream "~a" (maxima-native-expr/src obj)))

(clim:define-presentation-method clim:present (obj (type maxima-native-expr) (stream string-stream) (view t) &key)
  (when obj
    (format stream "~a" (maxima-native-expr/src obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-error-message (stream message)
  (let ((size (clim:text-style-size (clim:medium-text-style stream))))
    (clim:surrounding-output-with-border (stream :ink clim:+black+ :line-thickness 1)
      (clim:with-drawing-options (stream :ink clim:+red+ :text-style (clim:make-text-style :fix :roman size))
        (format stream "~a~%" message)))))
