(in-package :infoparser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %process-single-line-command (string clauses)
    (alexandria:with-gensyms (match parts)
      (let ((clause (car clauses)))
        (if (eq (car clause) t)
            `(progn ,@(cdr clause))
            `(multiple-value-bind (,match ,parts)
                 (cl-ppcre:scan-to-strings ,(caar clause) ,string)
               (declare (ignorable ,parts))
               (if ,match
                   ,(if (cadar clause)
                        `(let ((,(cadar clause) ,parts))
                           ,@(cdr clause))
                        `(progn ,@(cdr clause)))
                   ,@(if (cdr clauses)
                         (list (%process-single-line-command string (cdr clauses)))
                         nil))))))))

(defmacro process-single-line-command (string &body clauses)
  (alexandria:once-only (string)
    (%process-single-line-command string clauses)))

(defun %skip-block (stream end-tag)
  (loop
    for s = (read-line stream)
    until (cl-ppcre:scan end-tag s)
    collect s))

(defmacro skip-block (stream end-tag &environment env)
  (alexandria:once-only (stream)
    `(%skip-block ,stream
                  ,(if (constantp end-tag env)
                       `(load-time-value (cl-ppcre:create-scanner ,end-tag))
                       `(cl-ppcre:create-scanner ,end-tag)))))

(defun %markup-from-regexp (regexp string callback &optional plain-string-markup-fn)
  (flet ((markup-string (s)
           (if plain-string-markup-fn
               (funcall plain-string-markup-fn s)
               (list s))))
    (loop
      with length = (length string)
      with start = 0
      while (< start length)
      append (multiple-value-bind (match-start match-end reg-starts reg-ends)
                 (cl-ppcre:scan regexp string :start start)
               (if match-start
                   ;; Some highlighted text was found
                   (let* ((highlight (funcall callback reg-starts reg-ends))
                          (old-start start))
                     (setq start match-end)
                     (if (> match-start old-start)
                         ;; There is some unmatched text before the match
                         (append (markup-string (subseq string old-start match-start))
                                 (list highlight))
                         ;; ELSE: The match is at the beginning of the string
                         (list highlight)))
                   ;; ELSE: No match, copy the last part of the text and finish the loop
                   (progn
                     (let ((old-start start))
                       (setq start length)
                       (markup-string (subseq string old-start)))))))))

(defmacro markup-from-regexp (regexp string callback &optional plain-string-markup-fn &environment env)
  `(%markup-from-regexp ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                        ,string ,callback ,@(if plain-string-markup-fn (list plain-string-markup-fn))))

(defun %iterate-regexps (regexp string callback)
  (loop
    with length = (length string)
    with start = 0
    while (< start length)
    do (multiple-value-bind (match-start match-end reg-starts reg-ends)
           (cl-ppcre:scan regexp string :start start)
         (cond (match-start
                (setq start match-end)
                (let ((vec (make-array (length reg-starts) :element-type 'string :initial-element "")))
                  (loop
                    for start across reg-starts
                    for end across reg-ends
                    for i from 0
                    do (setf (aref vec i) (subseq string start end)))
                  (funcall callback vec)))
               (t
                (setq start length))))))

(defmacro iterate-regexps ((sym regexp string) &body body &environment env)
  `(%iterate-regexps ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                     ,string
                     (lambda (,sym) ,@body)))

(defun process-menu (stream)
  (cons :menu
        ;; Only process lines matching * title ::
        (loop
          for s = (read-line stream)
          until (cl-ppcre:scan "^@end menu *$" s)
          append (multiple-value-bind (match strings)
                     (cl-ppcre:scan-to-strings "^ *\\* *([^ ]+(?: +[^ ]+)*) *:: *$" s)
                   (if match
                       (list (aref strings 0))
                       ;; ELSE: No match
                       nil)))))

(defun parse-paragraph (s)
  (cons :paragraph
        (markup-from-regexp "@([a-z]+){([^}]+)}" s
                            (lambda (reg-starts reg-ends)
                              (let ((cmd (subseq s (aref reg-starts 0) (aref reg-ends 0)))
                                    (content (subseq s (aref reg-starts 1) (aref reg-ends 1))))
                                (list (string-case:string-case (cmd)
                                        ("code" :code)
                                        ("var" :var)
                                        ("mref" :mref)
                                        ("emph" :italic)
                                        ("i" :italic)
                                        ("strong" :bold)
                                        ("b" :bold)
                                        ("anchor" :anchor)
                                        ("xref" :xref)
                                        ("fname" :fname)
                                        ("math" :math)
                                        ("mrefcomma" :mrefcomma)
                                        ("mrefdot" :mrefdot)
                                        ("ref" :ref)
                                        ("mxrefcomma" :mxrefcomma) ;; 2-arg
                                        ("mxref" :mxref) ;; 2-arg
                                        ("mxrefdot" :mxrefdot) ;; 2-arg
                                        ("file" :file)
                                        ("mrefparen" :mrefparen)
                                        ("footnote" :footnote)
                                        ("kbd" :kbd)
                                        ("key" :key) ;; Same as kbd?
                                        ("image" :image) ;; filename,size
                                        ("dfn" :dfn)
                                        ("url" :url)
                                        ("uref" :url)
                                        ("figure" :figure)
                                        ("verb" :verb)
                                        ("dotless" :dotless) ;; Only used to create a dotless i
                                        ("email" :email)
                                        ("pxref" :pxref)
                                        (t (error "Unknown cmd: ~s" cmd)))
                                      content)))
                            (lambda (s)
                              (list (cl-ppcre:regex-replace-all "@dots{}" s "…"))))))

(defun process-opencatbox (stream)
  (cons :catbox
        (loop
          for s = (read-line stream)
          until (cl-ppcre:scan "^@closecatbox *$" s)
          append (collectors:with-collector (coll)
                   (iterate-regexps (category "@category{([^}]+)}" s)
                     (coll (aref category 0)))
                   (coll)))))

(defun parse-node (args)
  (labels ((nil-or-trim (s)
             (if s
                 (let ((trimmed (string-trim " " s)))
                   (if (zerop (length trimmed)) nil trimmed))
                 nil)))
    (let* ((category-names (split-sequence:split-sequence #\, args))
           (name (string-trim " " (nth 0 category-names)))
           (next (nil-or-trim (nth 1 category-names)))
           (prev (nil-or-trim (nth 2 category-names)))
           (up (nil-or-trim (nth 3 category-names))))
      (list name next prev up))))

(alexandria:define-constant +newline-str+ (format nil "~c" #\Newline) :test #'equal)

(defun process-demo-code (stream)
  (multiple-value-bind (code input)
      (collectors:with-collectors (coll input-coll)
        (loop
          with curr = ""
          for s = (read-line stream)
          until (cl-ppcre:scan "^@c ===end===" s)
          unless (zerop (length s))
            do (progn
                 (process-single-line-command s
                   (("@c +input:(.*[^ ]) *$" strings)
                    (input-coll (aref strings 0)))
                   (("@c +(.*[^ ]) *$" strings)
                    (setq curr (format nil "~a~a~a" curr (if (zerop (length curr)) "" +newline-str+) (aref strings 0))))
                   (("^[^@]")
                    (setq curr (format nil "~a~c~a" curr #\Newline s)))
                   (t
                    (error "Demo code block does not have the expected format: ~s" s)))
                 (when (cl-ppcre:scan "(?:;|\\$) *$" curr)
                   (coll curr)
                   (setq curr ""))))
        (values (coll) (input-coll)))
    ;; After the example code, the following example group needs to be skipped
    (loop
      for s = (read-line stream)
      until (cl-ppcre:scan "^@example *$" s)
      unless (cl-ppcre:scan "^ *$" s)
        do (error "No example block following demo code"))
    (let ((example (skip-block stream "^@end example$")))
      `(:demo-code (:demo-source . ,code)
                   ,@(if input `((:input . ,input)) nil)
                   (:example-info . ,example)))))

(defun parse-deffn (stream type name args)
  (let ((parsed-arglist (with-input-from-string (s args)
                          (parse-stream s nil))))
    (unless (and (alexandria:sequence-of-length-p parsed-arglist 1)
                 (eq (caar parsed-arglist) :paragraph))
      (error "Unexpected arglist format: ~s" args))
    (list* :deffn (list type name (cdar parsed-arglist))
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

(defun parse-defvr (stream type name)
  (list* :defvr (list type name)
         (parse-stream stream (cl-ppcre:create-scanner "^@end +defvr"))))

(defun parse-stream (stream end-tag)
  (collectors:with-collector (info-collector)
    (let ((current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
      (labels ((collect-paragraph ()
                 (when (plusp (length current-paragraph))
                   (info-collector (parse-paragraph current-paragraph))
                   (setq current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))))
        (loop
          for s = (read-line stream nil nil)
          until (or (null s)
                    (and end-tag
                         (cl-ppcre:scan end-tag s)))
          do (cond ((cl-ppcre:scan "^@[a-z]+(?: |$)" s)
                    (process-single-line-command s
                      (("^@c ===beg===") (collect-paragraph) (info-collector (process-demo-code stream)))
                      (("^@menu *$") (collect-paragraph) (info-collector (process-menu stream)))
                      (("^@opencatbox *$") (collect-paragraph) (info-collector (process-opencatbox stream)))

                      (("^@node +(.*)$" args)
                       (collect-paragraph)
                       (info-collector (cons :node (parse-node (aref args 0)))))

                      (("@section +(.*[^ ]) *$" name)
                       (collect-paragraph)
                       (info-collector (list :section (aref name 0))))

                      (("@subsection +(.*[^ ]) *$" name)
                       (collect-paragraph)
                       (info-collector (list :subsection (aref name 0))))

                      (("^@c ") nil)
                      (("^@ifhtml *$") (skip-block stream "^@end ifhtml"))
                      (("^@iftex *$") (skip-block stream "^@end iftex"))

                      (("^@deffn +{([^}]+)} +([^ ]+) +(.*[^ ]) *$" args)
                       (collect-paragraph)
                       (info-collector (parse-deffn stream (aref args 0) (aref args 1) (aref args 2))))

                      (("^@defvr +{([^}]+)} +(.*[^ ]) *$" args)
                       (collect-paragraph)
                       (info-collector (parse-defvr stream (aref args 0) (aref args 1))))

                      (("^@example *$")
                       (collect-paragraph)
                       (info-collector (cons :pre (skip-block stream "^@end example"))))))
                   ((cl-ppcre:scan "^ *$" s)
                    (collect-paragraph))
                   (t
                    ;; This is normal content, so we'll collect it into the output string
                    (when (plusp (length current-paragraph))
                      (vector-push-extend #\Space current-paragraph))
                    (loop for ch across s do (vector-push-extend ch current-paragraph)))))
        (collect-paragraph)
        (info-collector)))))

(defun parse-file (file)
  (log:trace "Parsing file: ~s" file)
  (with-open-file (stream file :direction :input :external-format :utf-8)
    (parse-stream stream nil)))

(defun evaluate-one-demo-src-line (src)
  (when (cl-ppcre:scan "^ *:" src)
    (return-from evaluate-one-demo-src-line nil))
  (when (or (not (maxima::checklabel maxima::$inchar))
	    (not (maxima::checklabel maxima::$outchar)))
    (incf maxima::$linenum))
  (let* ((expr (string-to-maxima-expr src))
         (c-tag (maxima::makelabel maxima::$inchar))
         (out (make-string-output-stream)))
    (setf (symbol-value c-tag) expr)
    (let ((eval-ret (catch 'maxima::macsyma-quit
                      (catch 'eval-expr-error
                        (let ((result (let ((*use-clim-retrieve* nil)
                                            (*standard-output* out))
                                        (handler-bind ((error (lambda (condition)
                                                                (throw 'eval-expr-error (cons :lisp-error condition)))))
                                          (eval-maxima-expression expr)))))
                          (let ((d-tag (maxima::makelabel maxima::$outchar)))
                            (setq maxima::$% result)
                            (setf (symbol-value d-tag) result)
                            (cons :result result)))))))
      (unless (and (consp eval-ret)
                   (eq (car eval-ret) :result))
        (error "Error when evaluating command: ~s: ~s" src eval-ret))
      (cdr eval-ret))))

(defun evaluate-demo-src (src standard-input-content)
  (uiop:with-temporary-file (:stream input)
    (with-standard-io-syntax
      (print (list src standard-input-content) input))
    (close input)
    (uiop:with-temporary-file (:stream output)
      (uiop:with-temporary-file (:stream error-out)
        (handler-case
            (let* ((dir (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/"))
                   (exec-file (merge-pathnames "maxima-parser.bin" dir))
                   (exec-name (namestring exec-file)))
              (uiop:run-program (list exec-name "--dynamic-space-size" "3000")
                                :input (pathname input)
                                :output (pathname output)
                                :error-output (pathname error-out))
              (close output)
              (with-open-file (s (pathname output))
                (read s)))
          (uiop:subprocess-error (condition)
            (close error-out)
            (format t "Error when calling external program: ~a~%Error output:~%~a"
                    condition
                    (uiop:read-file-string (pathname error-out)))
            (list :error "Error when calling external program"))
          (error (condition)
            (format t "Error while evaluating Maxima expression: ~a" condition)
            (list :error "Error evaluating expression")))))))

(defun resolve-example-code (info-content)
  (labels ((parse-branch (nodes)
             (loop
               for v in nodes
               if (listp v)
                 collect (if (eq (car v) :demo-code)
                             (let ((src (cdr (assoc :demo-source (cdr v))))
                                   (input (cdr (assoc :input (cdr v))))
                                   (example-info (cdr (assoc :example-info (cdr v)))))
                               `(:demo-code (:demo-source . ,src)
                                            (:example-info . ,example-info)
                                            (:demo-result . ,(evaluate-demo-src src input))))
                             (parse-branch v))
               else
                 collect v)))
    (parse-branch info-content)))

(defun parse-doc-directory (info-directory destination-directory)
  "Parse all texinfo files in the given directory and output
corresponding lisp files to the output directory."
  (let ((dir (merge-pathnames "*.texi" info-directory)))
    (loop
      for file in (directory dir)
      for content = (parse-file file)
      for processed = (resolve-example-code content)
      do (with-open-file (s (make-pathname :name (pathname-name file)
                                           :type "lisp"
                                           :defaults destination-directory)
                            :direction :output
                            :if-exists :supersede)
           (with-standard-io-syntax
             (let ((*print-readably* t))
               (print processed s)))))))

(defun make-multiline-string (lines)
  (with-output-to-string (s)
    (loop
      for line in lines
      do (format s "~a~%" line))))

(defun resolve-example-code-external ()
  "This function is called from the standalone maxima expression evaluator."
  (with-maxima-package
    (maxima::initialize-runtime-globals))
  (setq *debugger-hook* nil)
  (let ((src-data (with-standard-io-syntax
                    (read))))
    (with-input-from-string (in (make-multiline-string (second src-data)))
      (let ((*standard-input* in))
        (let ((result (loop
                        for v in (first src-data)
                        collect (let ((res (evaluate-one-demo-src-line v)))
                                  (if (cl-ppcre:scan "\\$ *$" v)
                                      ;; If the command ended with a $, don't save the result
                                      nil
                                      ;; ELSE: Normal command, the result needs to be saved
                                      res)))))
          (with-standard-io-syntax
            (let ((*print-readably* t))
              (print result))))))))

(defun generate-doc-directory ()
  "Generate lisp files for all the texinfo files in the maxima distribution."
  (let ((destination-directory (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/")))
    (ensure-directories-exist destination-directory)
    (parse-doc-directory (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/")
                         destination-directory)))
