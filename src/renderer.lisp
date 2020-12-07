(in-package :maxima-client)

(defvar *font-roman* '("MathJax_Main" "Regular"))
(defvar *font-roman-math* '("MathJax_Math" "Regular"))
(defvar *font-italic* '("MathJax_Main" "Italic"))
(defvar *font-italic-math* '("MathJax_Math" "Italic"))
(defvar *font-fixed* '("Source Code Pro" "Regular"))
(defvar *font-integrate-size1* '("MathJax_Size1" "Regular"))
(defvar *font-integrate-size1-scale* 0.8)
(defvar *font-integrate-size2* '("MathJax_Size2" "Regular"))
(defvar *font-integrate-size2-scale* 0.54)
(defvar *font-paren-size3* '("MathJax_Size3" "Regular"))
(defvar *font-paren-size3-scale* 0.40)
(defvar *font-paren-size4* '("MathJax_Size4" "Regular"))
(defvar *font-paren-size4-scale* 0.35)
(defvar *draw-boxes* nil)

(defvar *rop*)
(defvar *lop*)
(defvar *inhibit-presentations* nil)

(defvar *render-functions* (make-hash-table))

;;; margin output records

(defclass margin-output-record (clim:standard-sequence-output-record)
  ((top-margin    :initform 0
                  :accessor margin-output-record/top-margin)
   (bottom-margin :initform 0
                  :accessor margin-output-record/bottom-margin)))

(defun update-record-margins (record &key (top 0) (bottom 0))
  (dimension-bind (record :y y1 :bottom y2)
    (setf (margin-output-record/top-margin record) (- top y1))
    (setf (margin-output-record/bottom-margin record) (- bottom y2))))

(defgeneric output-record-margins (record))

(defmethod output-record-margins (record)
  (dimension-bind (record :x x1 :y y1 :right x2 :bottom y2)
    (list x1 y1 x2 y2)))

(defmethod output-record-margins ((record clim:standard-sequence-output-record))
  (let ((min-x nil)
        (max-x nil)
        (min-y nil)
        (max-y nil))
    (clim:map-over-output-records (lambda (rec)
                                    (destructuring-bind (x1 y1 x2 y2)
                                        (output-record-margins rec)
                                      (setf min-x (if min-x (min x1 min-x) x1))
                                      (setf max-x (if max-x (max x2 max-x) x2))
                                      (setf min-y (if min-y (min y1 min-y) y1))
                                      (setf max-y (if max-y (max y2 max-y) y2))))
                                  record)
    (list min-x min-y max-x max-y)))

(defmethod output-record-margins ((record margin-output-record))
  (dimension-bind (record :x x1 :y y1 :right x2 :bottom y2)
    (list x1 (+ y1 (margin-output-record/top-margin record))
          x2 (+ y2 (margin-output-record/bottom-margin record)))))

(defmacro with-record-margins ((stream &key top bottom) &body body)
  (check-type stream symbol)
  (alexandria:once-only (top bottom)
    (alexandria:with-gensyms (rec)
      `(let ((,rec (clim:with-output-to-output-record (,stream 'margin-output-record)
                     ,@body)))
         (update-record-margins ,rec :top ,top :bottom ,bottom)
         (clim:stream-add-output-record ,stream ,rec)))))

(defmacro with-char-margins ((stream) &body body)
  (check-type stream symbol)
  `(with-record-margins (,stream :top (- (m-ascent ,stream)) :bottom 0)
     ,@body))

;;; render function

(defmacro define-render-function ((name symbol stream-sym args-sym &key inhibit-presentation op-args) &body body)
  (check-type name symbol)
  (check-type stream-sym symbol)
  (check-type args-sym symbol)
  (alexandria:with-gensyms (op-args-sym)
    (let ((symbol-list (etypecase symbol
                         (symbol (list symbol))
                         (list
                          (unless (every #'symbolp symbol)
                            (error "All elements must be symbols"))
                          symbol))))
      `(progn
         (defun ,name (,stream-sym ,op-args-sym ,args-sym)
           (declare (ignorable ,op-args-sym))
           ,(if op-args
                `(let ((,op-args ,op-args-sym)) ,@body)
                `(progn ,@body)))
         ,@(mapcar (lambda (v)
                     `(setf (gethash ',v *render-functions*) (list ',name ,(if inhibit-presentation t nil))))
                   symbol-list)))))

;;; view definitions

(defclass maxima-renderer-view (maxima-client.markup:markup-text-view)
  ())

(defclass maxima-interactor-view (maxima-renderer-view clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

;;; renderer stuff

(defun make-boxed-output-record (stream rec)
  (if *draw-boxes*
      (clim:with-output-to-output-record (stream)
        (dimension-bind (rec :x x :y y :right right :bottom bottom)
          (clim:stream-add-output-record stream rec)
          (clim:draw-rectangle* stream x y (1- right) (1- bottom) :filled nil)))
      rec))

(defmacro with-font ((stream font &optional size replacement) &body body)
  (alexandria:once-only (stream font)
    (alexandria:with-gensyms (body-fn)
      `(labels ((,body-fn ()
                  (clim:with-text-style (,stream (clim-freetype:make-font-replacement-text-style (first ,font) (second ,font) *font-size* ,replacement))
                    ,@body)))
         ,(if size
              `(let ((*font-size* ,size))
                 (,body-fn))
              `(,body-fn))))))

(defmacro with-roman-text-style ((stream &optional size) &body body)
  `(with-font (,stream *font-roman* ,size (list *font-roman-math*))
     ,@body))

(defmacro with-italic-text-style ((stream &optional size) &body body)
  `(with-font (,stream *font-italic* ,size (list *font-italic-math*))
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

(defmacro with-paren-op (&body body)
  `(let ((*lop* 'maxima::mparen)
         (*rop* 'maxima::mparen))
     ,@body))

(defun expr-as-fn (expr)
  (lambda (stream)
    (render-maxima-expression stream expr)))

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

(defun char-descent (stream char)
  (multiple-value-bind (width height x y baseline)
      (clim:text-size stream (format nil "~c" char))
    (declare (ignore width x y))
    (- height baseline)))

(defun %render-quotient (stream top-expr bottom-expr)
  (let ((top-fraction-spacing (/ (char-height stream) 4))
        (bottom-fraction-spacing (/ (char-height stream) 8))
        (top (clim:with-output-to-output-record (stream)
               (with-paren-op
                 (funcall top-expr stream))))
        (bottom (clim:with-output-to-output-record (stream)
                  (with-paren-op
                    (funcall bottom-expr stream)))))
    (dimension-bind (top :width top-width :height top-height)
      (dimension-bind (bottom :width bottom-width :y bottom-y)
        (let* ((max-width (max top-width bottom-width))
               (centre (- (/ (char-height stream) 4))))
          (set-rec-position top
                            (/ (- max-width top-width) 2)
                            (- centre top-height top-fraction-spacing))
          (clim:stream-add-output-record stream top)
          (set-rec-position bottom
                            (/ (- max-width bottom-width) 2)
                            (+ centre
                               bottom-fraction-spacing
                               (max (+ (text-style-font-ascent stream) bottom-y) 0)))
          (clim:stream-add-output-record stream bottom)
          (clim:draw-line* stream 0 centre max-width centre))))))

(defun render-quotient (stream top-expr bottom-expr)
      (%render-quotient stream
                        (lambda (stream)
                          (render-maxima-expression stream top-expr))
                        (lambda (stream)
                          (render-maxima-expression stream bottom-expr))))

(define-render-function (render-mquotient maxima::mquotient stream args)
  (destructuring-bind (top-expr bottom-expr)
      args
    (render-quotient stream top-expr bottom-expr)))

(define-render-function (render-rat maxima::rat stream args)
  (destructuring-bind (top-expr bottom-expr)
      args
    (render-quotient stream top-expr bottom-expr)))

(defun render-number (stream n)
  (with-char-margins (stream)
    (render-formatted stream "~a" n)))

(defun render-inf (stream)
  (with-char-margins (stream)
    (render-formatted stream "~c" #\INFINITY)))

(defun render-symbol-name-inner (stream formatted roman-font)
  (labels ((render ()
             (if (alexandria:starts-with-subseq "%" formatted)
                 (with-aligned-rendering (stream)
                   (with-font-size-change (stream (- (* (char-height stream) 0.6)))
                     (render-aligned () (render-symbol-str stream "%")))
                   (render-aligned () (render-formatted-with-replacement stream "~a" (subseq formatted 1))))
                 (render-formatted-with-replacement stream "~a" formatted))))
    (if roman-font
        (with-roman-text-style (stream) (render))
        (with-italic-text-style (stream) (render)))))

(defun render-symbol-inner (stream sym roman-font)
  (case sym
    (maxima::$inf (render-inf stream))
    (maxima::$minf (%render-negation stream (lambda (stream) (render-inf stream))))
    (maxima::$%pi (with-font (stream *font-roman-math*)
                    (with-char-margins (stream)
                      (render-formatted stream "~c" #\GREEK_SMALL_LETTER_PI))))
    (maxima::$%lambda (with-font (stream *font-roman-math*)
                        (with-char-margins (stream)
                          (render-formatted stream "~c" #\GREEK_SMALL_LETTER_LAMDA))))
    (maxima::%gamma (with-char-margins (stream)
                      (render-formatted stream "~c" #\GREEK_CAPITAL_LETTER_GAMMA)))
    (t
     ;; Check if the symbol should be rendered using subscript. The
     ;; rule is: If the name contains a single underscore, and one
     ;; of the parts is a single letter, then the latter part should
     ;; be subscripted.
     (let* ((name (format-sym-name sym))
            (parts (split-symbol-if-subscript name)))
       (if parts
           (render-subscript stream
                             (lambda (stream)
                               (render-symbol-name-inner stream (first parts) roman-font))
                             (lambda (stream)
                               (render-formatted-with-replacement stream "~a" (second parts))))
           ;; ELSE: Don't render the symbol using subscript
           (render-symbol-name-inner stream name roman-font))))))

(defun split-symbol-if-subscript (name)
  (let ((pos (position #\_ name)))
    (when pos
      (unless (position #\_ name :start (1+ pos))
        (when (or (= pos 1)
                  (= (- (length name) pos) 2))
          (list (subseq name 0 pos)  (subseq name (1+ pos))))))))

(defun render-symbol (stream sym &key roman-font)
  (case sym
    (maxima::$test_aa (render-size-test stream))
    (maxima::$test_bb (render-paren-test stream))
    (maxima::$test_cc (render-sqrt-test stream))
    (t
     (clim:with-output-as-presentation (stream sym 'maxima-native-symbol :view (clim:stream-default-view stream))
       (with-char-margins (stream)
         (render-symbol-inner stream sym roman-font))))))

(defun %render-negation (stream fn &optional (spacing 0.2))
  (with-aligned-rendering (stream)
    (render-aligned-string "~c" #\MINUS_SIGN)
    (aligned-spacing spacing)
    (let ((*lop* 'maxima::mminus))
      (render-aligned () (funcall fn stream)))))

(defun render-negation (stream expr &optional (spacing 0.2))
  (%render-negation stream
                    (lambda (stream)
                      (render-maxima-expression stream expr))
                    spacing))

(define-render-function (process-render-negation maxima::mminus stream args)
  (destructuring-bind (expr)
      args
    (render-negation stream expr)))

(define-render-function (render-plus maxima::mplus stream args :op-args op-args)
  (let ((truncated-p (member 'maxima::trunc (cdr op-args))))
    (with-aligned-rendering (stream)
      (iterate-exprs (expr args 'maxima::mplus :first-sym first)
        (unless first
          (aligned-spacing 0.2))
        (cond ((and (listp expr)
                    (alexandria:length= (length expr) 2)
                    (listp (car expr))
                    (eq (caar expr) 'maxima::mminus))
               (render-aligned () (render-negation stream (second expr))))
              (t
               (unless first
                 (render-aligned-string "+")
                 (aligned-spacing 0.2))
               (render-aligned () (render-maxima-expression stream expr)))))
      ;; If this is a truncated expression, render the ... part
      (when truncated-p
        (aligned-spacing 0.2)
        (render-aligned-string "+")
        (loop
          repeat 3
          do (progn
               (aligned-spacing 0.2)
               (render-aligned-string ".")))))))

(define-render-function (render-times maxima::mtimes stream args)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr args 'maxima::mtimes :first-sym first)
      (unless first
        (aligned-spacing 0.4))
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

(defun render-simple-fraction (stream a b)
  "Renders a fraction with both arguments separated by a slash instead of the regular style."
  (clim:draw-text* stream (format nil "~a/~a" a b) 0 0))

(defun render-expt (stream a b)
  (%render-expt stream
                (lambda (stream)
                  (let ((*rop* 'maxima::mexpt))
                    (render-maxima-expression stream a)))
                (lambda (stream)
                  (if (and (listp b)
                           (eq (caar b) 'maxima::rat)
                           (integerp (second b))
                           (integerp (third b)))
                      ;; This is a simple fraction, render with a slash
                      (render-simple-fraction stream (second b) (third b))
                      ;; ELSE: All other exponents are rendered normally
                      (let ((*lop* 'maxima::mexpt))
                        (render-maxima-expression stream b))))))

(defun render-op-list (stream maxima-sym displayed-sym exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs maxima-sym :first-sym first)
      (unless first
        (aligned-spacing 0.5)
        (render-aligned-string "~a" displayed-sym)
        (aligned-spacing 0.5))
      (render-aligned () (render-maxima-expression stream expr)))))

(define-render-function (render-mand maxima::mand stream args)
  (render-op-list stream 'maxima::mand (format nil "~c" #\LOGICAL_AND) args))

(define-render-function (render-mor maxima::mor stream args)
  (render-op-list stream 'maxima::mor (format nil "~c" #\LOGICAL_OR) args))

(defun render-plain (stream fun symbol a b &key (spacing 0.4))
  (with-aligned-rendering (stream)
    (render-aligned () (let ((*rop* fun))
                         (render-maxima-expression stream a)))
    (aligned-spacing spacing)
    (render-aligned-string "~c" symbol)
    (aligned-spacing spacing)
    (render-aligned () (let ((*lop* fun))
                         (render-maxima-expression stream b)))))

(define-render-function (render-mgreaterp maxima::mgreaterp stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mgreaterp #\> a b)))

(define-render-function (render-mlessp maxima::mlessp stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mlessp #\< a b)))

(define-render-function (render-mleqp maxima::mleqp stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mleqp #\LESS-THAN_OR_EQUAL_TO a b)))

(define-render-function (render-mgeqp maxima::mgeqp stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mgeqp #\GREATER-THAN_OR_EQUAL_TO a b)))

(define-render-function (render-mnotequal maxima::mnotequal stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mnotequal #\NOT_EQUAL_TO a b)))

(define-render-function (render-mequal maxima::mequal stream args)
  (destructuring-bind (a b)
      args
    (render-plain stream 'maxima::mequal #\= a b)))

(defun wrap-with-plain-paren (stream output-record style paren-font paren-font-scale left-spacing right-spacing)
  (destructuring-bind (x1 y1 x2 y2)
      (output-record-margins output-record)
    (let ((width (- x2 x1))
          (height (- y2 y1)))
      ;; MKREC generates the output record for a left or right paren
      (flet ((mkrec (paren-char square-char)
               (clim:with-output-to-output-record (stream)
                 (with-font (stream paren-font (* height paren-font-scale))
                   (with-record-margins (stream :top y1 :bottom y2)
                     (clim:draw-text* stream (ecase style
                                               (:paren paren-char)
                                               (:square square-char))
                                      0 0))))))
        ;;
        (let ((left-paren (mkrec "(" "[")))
          (dimension-bind (left-paren :right left-paren-x2 :y left-paren-y1 :bottom left-paren-y2)
            (let* ((centre (+ (/ height 2) y1))
                   (p-centre (/ (+ left-paren-y1 left-paren-y2) 2))
                   (p-offset (- centre p-centre))
                   (new-rec-x (+ left-paren-x2 left-spacing)))
              (move-rec left-paren x1 p-offset)
              (clim:stream-add-output-record stream left-paren)
              ;;
              (move-rec output-record new-rec-x 0)
              (clim:stream-add-output-record stream output-record)
              ;;
              (let ((right-paren (mkrec ")" "]")))
                (move-rec right-paren (+ x1 new-rec-x width right-spacing) p-offset)
                (clim:stream-add-output-record stream right-paren)))))))))

(defun draw-vertical-paren (stream height top-char bottom-char ext-char)
  (with-font (stream *font-paren-size4*)
    (flet ((make-vertical-bar ()
             (clim:with-output-to-output-record (stream)
               (clim:draw-text* stream (string ext-char) 0 0))))
      ;;
      (let ((top (clim:with-output-to-output-record (stream)
                   (clim:draw-text* stream (string top-char) 0 0)))
            (bottom (clim:with-output-to-output-record (stream)
                      (clim:draw-text* stream (string bottom-char) 0 0))))
        (clim:stream-add-output-record stream top)
        (dimension-bind (top :height top-height :bottom top-y2)
          (dimension-bind (bottom :height bottom-height)
            (let ((pos (1- top-y2)))
              (loop
                while (< pos (+ top-y2 (- height top-height bottom-height)))
                do (let ((rec (make-vertical-bar)))
                     (set-rec-position rec nil pos)
                     (clim:stream-add-output-record stream rec)
                     (dimension-bind (rec :bottom bottom)
                       (setq pos (1- bottom)))))
              (set-rec-position bottom nil pos)
              (clim:stream-add-output-record stream bottom))))))))

(defun wrap-with-split-paren (stream output-record style)
  (dimension-bind (output-record :height rec-height :y rec-y1 :right rec-x2)
    (let ((left (clim:with-output-to-output-record (stream)
                  (apply #'draw-vertical-paren stream rec-height
                         (ecase style
                           (:paren (list #\LEFT_PARENTHESIS_UPPER_HOOK #\LEFT_PARENTHESIS_LOWER_HOOK #\LEFT_PARENTHESIS_EXTENSION))
                           (:square (list #\LEFT_SQUARE_BRACKET_UPPER_CORNER #\LEFT_SQUARE_BRACKET_LOWER_CORNER #\LEFT_SQUARE_BRACKET_EXTENSION))))))
          (right (clim:with-output-to-output-record (stream)
                   (apply #'draw-vertical-paren stream rec-height
                          (ecase style
                            (:paren (list #\RIGHT_PARENTHESIS_UPPER_HOOK #\RIGHT_PARENTHESIS_LOWER_HOOK #\RIGHT_PARENTHESIS_EXTENSION))
                            (:square (list #\RIGHT_SQUARE_BRACKET_UPPER_CORNER #\RIGHT_SQUARE_BRACKET_LOWER_CORNER #\RIGHT_SQUARE_BRACKET_EXTENSION)))))))
      (dimension-bind (left :height paren-height :right left-x2)
        (let ((paren-top-pos (- (+ rec-y1 (/ rec-height 2))
                                (/ paren-height 2))))
          (set-rec-position left nil paren-top-pos)
          (clim:stream-add-output-record stream left)
          (move-rec output-record left-x2 0)
          (clim:stream-add-output-record stream output-record)
          (set-rec-position right (+ left-x2 rec-x2) paren-top-pos)
          (clim:stream-add-output-record stream right))))))

(defun wrap-with-parens (stream output-record &key (style :paren) (left-spacing 0) (right-spacing 0))
  ;; Three use-cases:
  ;;   - standard size
  ;;   - narrow plain paren
  ;;   - multi-segment paren
  ;;
  (let ((char-height (char-height stream)))
    (dimension-bind (output-record :height height)
      (cond ((> height (* char-height 5))
             ;; Height is very large, need to render by splitting
             (wrap-with-split-paren stream output-record style))
            ((> height (* char-height 2))
             ;; Use thin paren
             (wrap-with-plain-paren stream output-record style *font-paren-size4* 0.35 left-spacing right-spacing))
            (t
             ;; Use standad paren
             (wrap-with-plain-paren stream output-record style *font-roman* 1 left-spacing right-spacing))))))

(defmacro with-wrapped-parens ((stream &key (style :paren)) &body body)
  (alexandria:once-only (stream style)
    (alexandria:with-gensyms (rec)
      `(let ((,rec (clim:with-output-to-output-record (,stream)
                     (with-paren-op
                       ,@body))))
         (wrap-with-parens ,stream ,rec :style ,style)))))

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
                                          ;; An array reference can be a function call. Here we simply call
                                          ;; the normal rendering function, but with the name not rendered in
                                          ;; italics. If so, a special function is needed to render this properly.
                                          ;; Also, the function will be wrapped in parens, which is different
                                          ;; from what normal Maxima does. We'll just accept this since the
                                          ;; extra parens shouldn't be an issue.
                                          (cond
                                            ((symbolp name) (render-symbol stream name :roman-font t))
                                            ;; TODO: In some cases, the list (MAXIMA::$BLOCK) may be found
                                            ;; in the name position. Currently it is not known if $BLOCK is a
                                            ;; special case, or if there are other symbols that can be found
                                            ;; in this list. For now, let's just implement it like this until
                                            ;; a better way is proposed.
                                            ((and (listp name)
                                                  (alexandria:sequence-of-length-p name 1)
                                                  (eq (car name) 'maxima::$block))
                                             (render-symbol stream (car name) :roman-font t))
                                            (t (render-maxima-expression stream name)))))))

(defun render-and-measure-string (stream string &optional (x 0) (y 0))
  (multiple-value-bind (width height final-x final-y baseline)
      (clim:text-size stream string)
    (declare (ignore final-x final-y))
    (let ((rec (clim:with-output-to-output-record (stream)
                 (clim:draw-text* stream string x y))))
      (values rec baseline (- height baseline) width))))

(defun render-intsum-inner (stream f var from-fn to symbol sym2 font-fn)
  (let ((exp (clim:with-output-to-output-record (stream)
               (let ((*lop* 'maxima::%sum)
                     (*rop* 'maxima::mparen))
                 (render-maxima-expression stream f)))))
    (dimension-bind (exp :height exp-height)
      (destructuring-bind (sigma-font sigma-size)
          (funcall font-fn exp-height)
        (let ((sigma (clim:with-output-to-output-record (stream)
                       (with-font (stream sigma-font sigma-size)
                         (clim:draw-text* stream (format nil "~c" symbol) 0 0)))))
          (dimension-bind (sigma :width sigma-width :height sigma-height :y sigma-y-orig)
            (let ((centre (+ (/ sigma-height 2)
                             (/ (m-ascent stream) 2)
                             sigma-y-orig)))
              ;; centre is the y coordinate where the baseline should be located, so we move sigma
              ;; it is a negative value indicating the number of pixels above the baseline of the sigma itself
              (move-rec sigma 0 (- centre))
              (clim:stream-add-output-record stream (make-boxed-output-record stream sigma))
              ;;
              (dimension-bind (sigma :right sigma-right :x sigma-x :y sigma-y :bottom sigma-y2)
                ;;
                (when from-fn
                  (let ((bottom (clim:with-output-to-output-record (stream)
                                  (with-font-size-change (stream 0.8)
                                    (with-aligned-rendering (stream)
                                      (with-paren-op
                                        (when var
                                          (render-aligned () (render-maxima-expression stream var))
                                          (render-aligned () (render-formatted stream "=")))
                                        (render-aligned () (funcall from-fn stream))))))))
                    (dimension-bind (bottom :width bottom-width)
                      (set-rec-position bottom
                                        (+ sigma-x (/ (- sigma-width bottom-width) 2))
                                        (+ sigma-y2 (* (char-height stream) 0.2)))
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
                                        (- sigma-y top-height (* (char-height stream) 0.2)))
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
                      (clim:stream-add-output-record stream variable))))))))))))

(defun render-intsum (stream f var from-fn to symbol sym2 font-fn)
  (if (> (maxima::lbp *rop*) (maxima::rbp 'maxima::mparen))
      (let ((rec (clim:with-output-to-output-record (stream)
                   (render-intsum-inner stream f var from-fn to symbol sym2 font-fn))))
        (wrap-with-parens stream rec))
      (render-intsum-inner stream f var from-fn to symbol sym2 font-fn)))

(defun find-integrate-font (size)
  (let ((adjusted-size (+ size 10)))
    (cond ((< adjusted-size 65)
           (list *font-integrate-size1* (* adjusted-size *font-integrate-size1-scale*)))
          (t
           (list *font-integrate-size2* (* adjusted-size *font-integrate-size2-scale*))))))

(define-render-function (render-sum (maxima::%sum maxima::$sum) stream args)
  (destructuring-bind (f var from to)
      args
    (render-intsum stream f var (if from (expr-as-fn from) nil) to
                   #\N-ARY_SUMMATION nil (lambda (size) (list *font-integrate-size1*
                                                              (* (max size 40) *font-integrate-size1-scale*))))))
(define-render-function (render-integrate (maxima::%integrate maxima::$integrate) stream args)
  (destructuring-bind (f var &optional from to)
      args
    (render-intsum stream f nil (if from (expr-as-fn from) nil) to #\INTEGRAL var #'find-integrate-font)))

(define-render-function (render-product (maxima::%product maxima::$product) stream args)
  (destructuring-bind (f var from to)
      args
    (render-intsum stream f var (if from (expr-as-fn from) nil) to
                   #\N-ARY_PRODUCT nil (lambda (size) (list  *font-integrate-size1*
                                                             (* (max size 40) *font-integrate-size1-scale*))))))

(defun render-mlist-one-line (stream rec-list)
  "Render an mlist on a single line."
  (with-aligned-rendering (stream)
    (render-aligned () (render-formatted stream "["))
    (aligned-spacing 0.5)
    (loop
      for rec in rec-list
      for first = t then nil
      unless first
        do (progn
             (render-aligned-string ",")
             (aligned-spacing 0.4))
      do (render-aligned () (clim:stream-add-output-record stream rec)))
    (aligned-spacing 0.5)
    (render-aligned () (render-formatted stream "]"))))

(defun render-mlist-multi-line (stream rec-list)
  "Render an mlist where each element goes on its own line.
Each element should be an output record."
  (let ((left-bracket (clim:with-output-to-output-record (stream)
                        (render-formatted stream "[")))
        (right-bracket (clim:with-output-to-output-record (stream)
                         (render-formatted stream "]"))))
    (clim:stream-add-output-record stream left-bracket)
    (dimension-bind (left-bracket :right lb-right )
      (loop
        with bracket-space = (/ (char-width stream) 2)
        with left-margin = (+ lb-right bracket-space)
        with vertical-space = (/ (char-height stream) 2)
        with prev-rec-baseline = 0
        with prev-bottom = nil
        with prev-right = 0
        for rec in rec-list
        do (dimension-bind (rec :y rec-y)
             (let ((new-y-offset (if prev-bottom
                                     (+ prev-bottom
                                        (- rec-y)
                                        vertical-space)
                                     0)))
               (set-rec-position rec left-margin (+ new-y-offset rec-y))
               (clim:stream-add-output-record stream rec)
               (setq prev-rec-baseline new-y-offset)
               (dimension-bind (rec :bottom rec-bottom :right rec-right)
                 (setq prev-bottom rec-bottom)
                 (setq prev-right rec-right))))
        finally (dimension-bind (right-bracket :y right-bracket-y)
                  (set-rec-position right-bracket (+ prev-right bracket-space) (+ prev-rec-baseline right-bracket-y))
                  (clim:stream-add-output-record stream right-bracket))))))

(defun render-mlist (stream exprs)
  ;; If the entire list can't be rendered on a single line, we render
  ;; each element on its own line. We call one of two different
  ;; functions to do this.
  (with-paren-op
    (let ((pane-width (clim:rectangle-width (clim:pane-viewport-region stream)))
          (rec-list (mapcar (lambda (v)
                              (clim:with-output-to-output-record (stream)
                                (render-maxima-expression stream v)))
                            exprs)))
      (let ((width (reduce #'+ (mapcar (lambda (rec)
                                         (dimension-bind (rec :width w)
                                           w))
                                       rec-list))))
        (if (or (= (length rec-list) 1)
                (<= width pane-width))
            ;; Single-element lists, or narrow lists are rendered on a single line
            (render-mlist-one-line stream rec-list)
            ;; ELSE: The line is too wide, call the multi line renderer
            (render-mlist-multi-line stream rec-list))))))

(defun render-string (stream string)
  (with-fix-text-style (stream)
    (render-formatted-with-replacement stream "~s" string)))

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
        (if (member 'maxima::array (cdr function-name))
            (render-array-reference stream (car function-name) function-args)
            (render-param-list stream function-args
                               :prefix-renderer (lambda (stream)
                                                  (render-symbol stream (car function-name) :roman-font t)))))
      (aligned-spacing 0.5)
      (render-aligned-string symbol)
      (aligned-spacing 0.5)
      (render-aligned () (render-maxima-expression stream definition)))))

(define-render-function (render-limit (maxima::%limit maxima::$limit) stream args)
  (destructuring-bind (expr &optional sym to direction)
      args
    (log:trace "rendering limit: expr=~s, sym=~s, to=~s, direction=~s" expr sym to direction)
    (multiple-value-bind (top top-ascent top-descent)
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
                (clim:stream-add-output-record stream rec)))))))))

(defun %render-matrix (stream args)
  (let ((rec (clim:with-output-to-output-record (stream)
               (clim:formatting-table (stream :x-spacing (char-width stream) :y-spacing (char-height stream))
                 (loop
                   for row in args
                   do (clim:formatting-row (stream)
                        (loop
                          for col in (maxima-list-to-list row)
                          do (clim:formatting-cell (stream :align-y :center :align-x :center)
                               (with-paren-op
                                 (render-maxima-expression stream col))))))))))
    (wrap-with-parens stream rec :style :square :left-spacing (char-width stream) :right-spacing (char-width stream))))

(define-render-function (render-matrix maxima::$matrix stream args)
  (let ((rec (clim:with-output-to-output-record (stream)
               (%render-matrix stream args))))
    (dimension-bind (rec :height height)
      (set-rec-position rec nil (- (+ (/ height 2) (/ (char-height stream) 4))))
      (clim:stream-add-output-record stream rec))))

(defun render-msetq (stream a b)
  (with-aligned-rendering (stream)
    (render-aligned () (render-maxima-expression stream a))
    (aligned-spacing 0.5)
    (render-aligned-string ":")
    (aligned-spacing 0.5)
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

(defun render-derivative (stream expr sym-list)
  (let ((varlist (loop
                   for (sym exp) on sym-list by #'cddr
                   collect (cons sym exp)))
        (compressed-form-p (symbolp expr)))
    (labels ((render-sym (sym exp)
               (if (eql exp 1)
                   (render-symbol stream sym)
                   (%render-expt stream
                                 (lambda (stream)
                                   (render-symbol stream sym))
                                 (lambda (stream)
                                   (render-maxima-expression stream exp))))))
      (with-aligned-rendering (stream)
        (render-aligned () (let ((*inhibit-presentations* t))
                             (%render-quotient stream
                                               (lambda (stream)
                                                 (with-aligned-rendering (stream)
                                                   (let ((exp (reduce #'+ varlist :key #'cdr)))
                                                     (render-aligned () (render-sym 'maxima::$d exp))
                                                     (when compressed-form-p
                                                       (render-aligned () (render-symbol stream expr))))))
                                               (lambda (stream)
                                                 (with-aligned-rendering (stream)
                                                   (loop
                                                     for (sym . exp) in varlist
                                                     for first = t then nil
                                                     unless first
                                                       do (aligned-spacing 0.4)
                                                     do (progn
                                                          (render-aligned () (render-symbol stream 'maxima::$d))
                                                          (render-aligned () (render-sym sym exp)))))))))
        (unless compressed-form-p
          (aligned-spacing 0.5)
          (with-wrapped-parens (stream)
            (render-aligned () (render-maxima-expression stream expr))))))))

(defun render-subscript (stream name-fn args-fn)
  (let ((rec (clim:with-output-to-output-record (stream)
               (funcall name-fn stream))))
    (clim:stream-add-output-record stream rec)
    (destructuring-bind (x1 y1 x2 y2)
        (output-record-margins rec)
      (declare (ignore x1))
      (let ((height (- y2 y1)))
        (with-font-size-change (stream 0.8)
          (let ((args-rec (clim:with-output-to-output-record (stream)
                            (funcall args-fn stream))))
            (dimension-bind (args-rec :x args-x :y args-y :bottom args-bottom :height args-height)
              (move-rec args-rec
                        (- x2 args-x)
                        (if (>= args-height (* height 3/4))
                            (- (+ y1 (/ height 2)) args-y)
                            (let ((args-centre (/ (+ args-y args-bottom) 2)))
                              (- y2 args-centre))))
              (clim:stream-add-output-record stream args-rec))))))))

(defun render-array-reference (stream expr args &key paren-p)
  (unless args
    (error "Arg list should have at least one element"))
  (render-subscript stream
                    (lambda (stream)
                      (with-wrapped-optional-parens (stream paren-p)
                        (render-maxima-expression stream expr)))
                    (lambda (stream)
                      (render-arg-list stream args :spacing 0))))

(defun render-function-or-array-ref (stream arrayref-p paren-p expr args)
  (if arrayref-p
      (render-array-reference stream expr args :paren-p paren-p)
      (render-function stream expr args :paren-p paren-p)))

(defun render-units (stream expr unit)
  (with-aligned-rendering (stream)
    (render-aligned () (let ((*rop* 'maxima::|$`|))
                         (render-maxima-expression stream expr)))
    (aligned-spacing 0.2)
    (render-aligned-string "`")
    (aligned-spacing 0.2)
    (render-aligned () (let ((*lop* 'maxima::|$`|))
                         (render-maxima-expression stream unit)))))

(defun render-mnot (stream expr)
  (with-aligned-rendering (stream)
    (render-aligned-string "~c" #\NOT_SIGN)
    (aligned-spacing 0.4)
    (let ((*lop* 'maxima::mnot))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-mncexcept (stream a b)
  (%render-expt stream
                (lambda (stream)
                  (let ((*rop* 'maxima::mncexpt))
                    (render-maxima-expression stream a)))
                (lambda (stream)
                  (with-aligned-rendering (stream)
                    (render-aligned-string "<")
                    (render-aligned () (let ((*lop* 'maxima::mncexpt))
                                         (render-maxima-expression stream b)))
                    (render-aligned-string ">")))))

(defun render-factorial (stream expr)
  (with-aligned-rendering (stream)
    (render-aligned () (let ((*rop* 'maxima::mfactorial))
                         (render-maxima-expression stream expr)))
    (aligned-spacing 0.4)
    (render-aligned-string "!")))

(defun %find-paren-font (stream size)
  (let ((height (char-height stream)))
    (cond ((<= size (* height 2))
           (list *font-roman* (* size 1) (* (char-width stream) 0.2) 0))
          ((< size (* height 3))
           (list *font-integrate-size1* (* size 0.8) (* (char-width stream) 0.2) 0))
          ((< size (* height 4))
           (list *font-integrate-size2* (* size 0.54) (* (char-width stream) 0.3) 0))
          ((< size (* height 5.6))
           (list *font-paren-size3* (* size 0.40) (* (char-width stream) 0.4) 0))
          (t
           (list *font-paren-size4* (* size 0.33) (* (char-width stream) 0.5) 0)))))

(defun find-paren-font (stream size)
  (let ((result (%find-paren-font stream size)))
    (log:trace "findPF[size=~s ch=~s] → ~s" size (char-height stream) (car result))
    result))

(defun render-size-test (stream)
  (flet ((draw (font y m)
           (loop
             for size from 12 to 80 by 4
             for x = 10 then (+ x (* size 2.5))
             do (with-font (stream font (* size m))
                  (clim:draw-text* stream (format nil "(~c~c" #\GREEK_CAPITAL_LETTER_SIGMA #\N-ARY_SUMMATION) x y))
             do (with-font (stream *font-roman* size)
                  (clim:draw-text* stream "A" (+ x (* 2 (char-width stream))) y)))))
    (draw *font-roman* 10 1)
    (draw *font-integrate-size1* 60 0.7)
    (draw *font-integrate-size2* 120 0.45)
    (draw *font-paren-size3* 180 0.4)
    (draw *font-paren-size4* 240 0.3)))

(defun render-paren-test (stream)
  (flet ((render-iterate (x style)
           (loop
             with y = 20
             for i from 1 to 8
             do (let ((rec (clim:with-output-to-output-record (stream)
                             (with-wrapped-parens (stream :style style)
                               (loop
                                 with h = (char-height stream)
                                 for n from 0 below i
                                 do (clim:draw-text* stream "a" 0 (* n h)))))))
                  (move-rec rec x y)
                  (clim:stream-add-output-record stream rec)
                  (dimension-bind (rec :height h)
                    (incf y h))))))
    (render-iterate 0 :paren)
    (render-iterate 80 :square)))

(defun render-sqrt-test (stream)
  (loop
    with y = 20
    for i from 1 to 8
    do (let ((rec (clim:with-output-to-output-record (stream)
                    (%render-sqrt stream (lambda (stream)
                                           (loop
                                             with h = (char-height stream)
                                             for n from 0 below i
                                             do (clim:draw-text* stream "a" 0 (* n h))))))))
         (move-rec rec 0 y)
         (clim:stream-add-output-record stream rec)
         (dimension-bind (rec :height h)
           (incf y h)))))

(defun %render-sqrt (stream fn)
  (let ((spacing (/ (char-height stream) 4)))
    (let ((exp (clim:with-output-to-output-record (stream)
                 (with-paren-op
                   (funcall fn stream)))))
      (dimension-bind (exp :y exp-y :width exp-width :height exp-height)
        (let ((top-line-y (- exp-y spacing)))
          (multiple-value-bind (sqrt-sym sqrt-sym-ascent sqrt-sym-descent sqrt-sym-width)
              (destructuring-bind (font size x-offset y-offset)
                  (find-paren-font stream (+ exp-height spacing))
                (declare (ignore x-offset y-offset))
                (with-font (stream font size)
                  (render-and-measure-string stream (format nil "~c" #\SQUARE_ROOT) 0 0)))
            (declare (ignore sqrt-sym-ascent sqrt-sym-descent))
            (dimension-bind (sqrt-sym :y sqrt-sym-y)
              ;; Move the sqrt symbol to align the top of the symbol with the top of the expr
              (move-rec sqrt-sym 0 (- top-line-y sqrt-sym-y) #+nil (- exp-y sqrt-sym-ascent))
              (clim:stream-add-output-record stream sqrt-sym)
              ;; Move the expr to the right of the sqrt symbol
              (dimension-bind (sqrt-sym :right sqrt-sym-right)
                (move-rec exp sqrt-sym-right 0)
                (clim:stream-add-output-record stream exp)
                ;; Draw line above the expr
                (clim:draw-line* stream
                                 sqrt-sym-width
                                 top-line-y
                                 (+ sqrt-sym-right exp-width (/ (char-width stream) 4))
                                 top-line-y)))))))))

(defun render-sqrt (stream expr)
  (%render-sqrt stream (lambda (stream)
                         (render-maxima-expression stream expr))))

(defun render-lsum (stream f var list)
  (render-intsum stream f nil
                 (lambda (stream)
                   (with-aligned-rendering (stream)
                     (render-aligned () (render-maxima-expression stream var))
                     (aligned-spacing 0.2)
                     (render-aligned-string "in")
                     (aligned-spacing 0.2)
                     (render-aligned () (render-maxima-expression stream list))))
                 nil #\N-ARY_SUMMATION nil (lambda (size)
                                             (list *font-integrate-size1*
                                                   (* (max size 40) *font-integrate-size1-scale*)))))

(defun render-bigfloat (stream value)
  (render-formatted stream "~a" (map 'string (lambda (v)
                                               (maxima::get-first-char v))
                                     (maxima::fpformat value))))

(defun render-lisp-array (stream value)
  (render-formatted stream "{Lisp Array: ~s}" value))

(defun single-mlist-element-or-nil (element)
  (let ((mlist-p (eq (caar element) 'maxima::mlist)))
    (cond ((and mlist-p
                (cdr element)
                (not (cddr element)))
           (second element))
          ((not mlist-p)
           element)
          (t
           nil))))

(defun render-at (stream expr values)
  (let ((vert-line-spacing (/ (char-width stream) 2))
        (height-adjustment (char-height stream))
        (rec (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream expr)))
        (values-rec (clim:with-output-to-output-record (stream)
                      (with-paren-op
                        (alexandria:if-let ((single-element (single-mlist-element-or-nil values)))
                          (render-maxima-expression stream single-element)
                          ;; ELSE: Multiple elements
                          (with-aligned-rendering (stream)
                            (loop
                              for element in (cdr values)
                              for first = t then nil
                              do (let ((rec (clim:with-output-to-output-record (stream)
                                              (render-maxima-expression stream element))))
                                   (unless first
                                     (aligned-spacing 0.5))
                                   (render-aligned () (wrap-with-parens stream rec))))))))))
    (clim:stream-add-output-record stream rec)
    (dimension-bind (rec :y y1 :bottom y2 :right right)
      (let ((line-y1 (- y1 height-adjustment))
            (line-y2 (+ y2 height-adjustment))
            (line-x (+ right vert-line-spacing)))
        (clim:draw-line* stream line-x line-y1 line-x line-y2)
        (dimension-bind (values-rec :height values-height)
          (set-rec-position values-rec (+ right (* vert-line-spacing 2)) (- line-y2 values-height))
          (clim:stream-add-output-record stream values-rec))))))

(defun render-mbox (stream expr)
  (let ((margin 2)
        (rec (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream expr))))
    (dimension-bind-new (stream rec :x x1 :y y1 :right x2 :bottom y2)
      (clim:stream-add-output-record stream rec)
      (clim:draw-rectangle* stream (- x1 margin) (- y1 margin) (+ x2 margin) (+ y2 margin) :filled nil))))

(defun render-inference-result (stream title exprs displayed-rows)
  (let* ((margin 2)
         (expr-list (maxima-list-to-list exprs))
         (disp-rows-list (maxima-list-to-list displayed-rows))
         (rec-list (loop
                     for i in disp-rows-list
                     collect (clim:with-output-to-output-record (stream)
                               (render-maxima-expression stream (nth (1- i) expr-list)))))
         (max-width (reduce #'max rec-list
                            :key (lambda (rec)
                                   (dimension-bind (rec :width width)
                                     width))))
         (lrec (clim:with-output-to-output-record (stream)
                 (clim:with-text-face (stream :bold)
                   (clim:draw-text* stream title 0 0))
                 (loop
                   with y = (* (char-height stream) 1/2)
                   for rec in rec-list
                   do (dimension-bind (rec :width width :height height)
                        (set-rec-position rec (/ (- max-width width) 2) y)
                        (clim:stream-add-output-record stream rec)
                        (incf y (+ height (/ (char-height stream) 2))))))))
    (move-rec lrec margin margin)
    (dimension-bind-new (stream lrec :x x1 :y y1 :right x2 :bottom y2)
      (clim:stream-add-output-record stream lrec)
      (clim:draw-rectangle* stream (- x1 margin) (- y1 margin) (+ x2 margin) (+ y2 margin) :filled nil))))

(defun render-pathname (stream pathname)
  (with-aligned-rendering (stream)
    (render-aligned-string "~a" (namestring pathname))))

(defun inhibit-presentation-p (fixed)
  (or *inhibit-presentations*
      (and (listp fixed)
           (alexandria:if-let ((handler-info (gethash (caar fixed) *render-functions*)))
             (second handler-info)
             nil))))

(defmacro with-maxima-presentation ((stream expr) &body body)
  (check-type stream symbol)
  (alexandria:once-only (expr)
    (alexandria:with-gensyms (render)
      `(flet ((,render ()
                ,@body))
         (if (inhibit-presentation-p expr)
             (,render)
             (clim:with-output-as-presentation (,stream (make-instance 'maxima-native-expr :expr ,expr)
                                                        'maxima-native-expr
                                                        :view (clim:stream-default-view stream)
                                                        :allow-sensitive-inferiors t
                                                        :single-box t
                                                        :record-type 'maxima-native-expr-record)
               (,render)))))))

(define-render-function (render-marrow maxima::marrow stream args)
  (destructuring-bind (expr1 expr2)
      args
    (with-aligned-rendering (stream)
      (render-aligned () (render-maxima-expression stream expr1))
      (aligned-spacing 0.4)
      (let ((*rop* 'maxima::marrow))
        (render-aligned-string "~c" #\RIGHTWARDS_ARROW))
      (aligned-spacing 0.4)
      (let ((*lop* 'maxima::marrow))
        (render-aligned () (render-maxima-expression stream expr2))))))

(defun render-labelled-table (stream &rest content)
  (clim:formatting-table (stream :x-spacing (char-width stream))
    (loop
      for (label value) on content by #'cddr
      do (clim:formatting-row (stream)
           (clim:formatting-cell (stream :align-x :left)
             (clim:with-text-face (stream :bold)
               (format stream "~a" label)))
           (clim:formatting-cell (stream :align-x :left)
             (format stream "~a" value))))))

(define-render-function (render-build-info maxima::%build_info stream args :inhibit-presentation t)
  (destructuring-bind (version build-date host impl-type impl-version)
      args
    (render-labelled-table stream
                           "Maxima version:" version
                           "Maxima build date:" build-date
                           "Host type:" host
                           "Lisp implementation type:" impl-type
                           "Lisp implementation version:" impl-version)))

(define-render-function (render-mcond maxima::mcond stream args)
  (flet ((render-with-label (label expr)
           (clim:with-output-to-output-record (stream)
             (let ((rec (clim:with-output-to-output-record (stream)
                          (render-maxima-expression stream expr)))
                   (label-rec (clim:with-output-to-output-record (stream)
                                (with-sans-serif-font (stream)
                                  (clim:draw-text* stream label 0 0)))))
               (dimension-bind (rec :height rec-height)
                 (dimension-bind (label-rec :y label-y :height label-height :right label-right)
                   (clim:stream-add-output-record stream label-rec)
                   (set-rec-position rec
                                     (+ label-right (char-width stream))
                                     (- (+ label-y (/ label-height 2))
                                        (/ rec-height 2)))
                   (clim:stream-add-output-record stream rec)))))))
    (loop
      with y = 0
      for (if-clause then-clause) on args by #'cddr
      for first = t then nil
      unless (eq if-clause t)
        do (let ((rec (render-with-label (if first "if" "elseif") if-clause)))
             (set-rec-position rec 0 y)
             (dimension-bind (rec :height height)
               (incf y (+ height (/ (char-height stream) 2))))
             (clim:stream-add-output-record stream rec))
      do (let ((rec (render-with-label (if (eq if-clause t) "else" "then") then-clause)))
           (set-rec-position rec (* (char-width stream) 1) y)
           (dimension-bind (rec :height height)
             (incf y (+ height (/ (char-height stream) 2))))
           (clim:stream-add-output-record stream rec)))))

(defun render-maxima-expression (stream expr)
  (labels ((render-inner (fixed)
             (alexandria:if-let ((handler-info (gethash (caar fixed) *render-functions*)))
               (funcall (first handler-info) stream (car fixed) (cdr fixed))
               ;; ELSE: Explicit check
               (case (caar fixed)
                 (maxima::mlist (render-mlist stream (cdr fixed)))
                 (maxima::mexpt (render-expt stream (second fixed) (third fixed)))
                 (maxima::mdefine (render-mdefine stream (second fixed) (third fixed) ":="))
                 (maxima::mdefmacro (render-mdefine stream (second fixed) (third fixed) "::="))
                 (maxima::%sqrt (render-sqrt stream (second fixed)))
                 (maxima::mprog (render-function stream '(maxima::$block) (cdr fixed)))
                 (maxima::msetq (render-msetq stream (second fixed) (third fixed)))
                 (maxima::mabs (render-mabs stream (second fixed)))
                 (maxima::%derivative (render-derivative stream (second fixed) (cddr fixed)))
                 (maxima::mqapply (render-function-or-array-ref stream (member 'maxima::array (car fixed)) t (second fixed) (cddr fixed)))
                 (maxima::mnctimes (render-op-list stream 'maxima::mnctimes (format nil "~c" #\DOT_OPERATOR) (cdr fixed)))
                 (maxima::mncexpt (render-mncexcept stream (second fixed) (third fixed)))
                 (maxima::|$`| (render-units stream (second fixed) (third fixed)))
                 (maxima::mnot (render-mnot stream (second fixed)))
                 (maxima::mfactorial (render-factorial stream (second fixed)))
                 (maxima::%lsum (render-lsum stream (second fixed) (third fixed) (fourth fixed)))
                 (maxima::bigfloat (render-bigfloat stream fixed))
                 ((maxima::%$at maxima::%at) (render-at stream (second fixed) (third fixed)))
                 (maxima::mbox (render-mbox stream (second fixed)))
                 (maxima::$inference_result (render-inference-result stream (second fixed) (third fixed) (fourth fixed)))
                 (t (render-function-or-array-ref stream (member 'maxima::array (car fixed)) nil (caar fixed) (cdr fixed)))))))
    (log:trace "before nformat-check: ~s" expr)
    (let ((fixed (maxima::nformat-check expr)))
      (log:trace "Calling render expression on: ~s (lop=~a rop=~a)" fixed *lop* *rop*)
      (etypecase fixed
        (number (with-maxima-presentation (stream fixed) (render-number stream fixed)))
        (symbol (with-maxima-presentation (stream fixed) (render-symbol stream fixed)))
        (string (with-maxima-presentation (stream fixed) (render-string stream fixed)))
        (pathname (render-pathname stream fixed))
        (list (if (or (<= (maxima::lbp (caar fixed)) (maxima::rbp *lop*))
                      (<= (maxima::rbp (caar fixed)) (maxima::lbp *rop*)))
                  (with-wrapped-parens (stream)
                    (with-maxima-presentation (stream fixed)
                      (render-inner fixed)))
                  (with-maxima-presentation (stream fixed)
                    (render-inner fixed))))
        (array (render-lisp-array stream fixed))))))

(defmacro make-rendered-output-record ((stream) &body body)
  (alexandria:with-gensyms (output-record)
    `(with-roman-text-style (stream)
       (let ((,output-record (clim:with-output-to-output-record (,stream)
                               (with-paren-op
                                 ,@body))))
         (setf (clim:output-record-position ,output-record) (values 0 0))
         ,output-record))))

(defun make-expression-output-record (stream expr)
  (log:trace "Making output record for expr: ~s" expr)
  (make-rendered-output-record (stream)
    (render-maxima-expression stream expr)))

(defun render-maxima-native-expr-toplevel (stream expr)
  (check-type expr maxima-native-expr)
  (with-roman-text-style (stream)
    (with-paren-op
      (render-maxima-expression stream (maxima-native-expr/expr expr)))))

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
    (clim:with-drawing-options (stream :ink clim:+red+ :text-style (clim:make-text-style :fix :roman size))
      (format stream "~a~%" message))))
