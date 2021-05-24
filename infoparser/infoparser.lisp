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
    with r = (make-macro-reader stream)
    for s = (funcall r)
    until (cl-ppcre:scan end-tag s)
    collect s))

(defmacro skip-block (stream end-tag &environment env)
  (alexandria:once-only (stream)
    `(%skip-block ,stream
                  ,(if (constantp end-tag env)
                       `(load-time-value (cl-ppcre:create-scanner ,end-tag))
                       `(cl-ppcre:create-scanner ,end-tag)))))

(defvar *macro-handlers* (make-hash-table :test 'equal))

(defmacro define-macro-handler ((tag sym) &body body)
  (check-type tag string)
  (check-type sym symbol)
  `(setf (gethash ,tag *macro-handlers*)
         (lambda (,sym)
           (declare (ignorable ,sym))
           ,@body)))

(define-macro-handler ("ifhtml" in)
  (log:info "Found ifhtml")
  (skip-block in "^@end ifhtml"))

(defun make-macro-reader (in)
  (let ((in-function (etypecase in
                       (function in)
                       (stream (lambda () (read-line in))))))
    (lambda ()
      (loop
        for s = (funcall in-function)
        do (let ((found t))
             (multiple-value-bind (match strings)
                 (cl-ppcre:scan-to-strings "^@([a-z]+)(: |$)" s)
               (when match
                 (alexandria:when-let ((handler (gethash (aref strings 0) *macro-handlers*)))
                   (funcall handler in-function)
                   (setq found nil))))
             (when found
               (return s)))))))

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
                   (let ((old-start start))
                     (setq start length)
                     (markup-string (subseq string old-start))))))))

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
  (let ((content (collectors:with-collector (coll)
                   (let ((prefix nil))
                     (labels ((collect-prefix ()
                                (when prefix
                                  (let ((parsed (with-input-from-string (s (make-multiline-string (reverse prefix)))
                                                  (parse-stream s))))
                                    (when parsed
                                      (coll (list :menu-text parsed))))
                                  (setq prefix nil))))
                       (loop
                         with in = (make-macro-reader stream)
                         for s = (funcall in)
                         until (cl-ppcre:scan "^@end menu *$" s)
                         append (multiple-value-bind (match strings)
                                    (cl-ppcre:scan-to-strings "^ *\\* *([^ ]+(?: +[^ ]+)*) *:: *(.*[^ ])? *$" s)
                                  (if match
                                      (progn
                                        (collect-prefix)
                                        (coll (list :menu-entry (aref strings 0) (aref strings 1))))
                                      ;; ELSE: No match
                                      (push s prefix))))
                       (collect-prefix)
                       (coll))))))
    (cons :menu content)))

(defparameter *simple-tags*
  '(("mref" :mref)
    ("emph" :italic)
    ("i" :italic)
    ("strong" :bold)
    ("b" :bold)
    ("anchor" :anchor)
    ("xref" :xref)
    ("math" :math)
    ("ref" :ref)
    ("file" :file)
    ("footnote" :footnote)
    ("kbd" :key)
    ("key" :key)
    ("dfn" :dfn)
    ("url" :url)
    ("uref" :url)
    ("verb" :verb)
    ("dotless" :dotless)
    ("email" :email)
    ("pxref" :pxref)
    ("var" :var)
    ("env" :code)))

(defun parse-image (args)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^([^,]+)(?:,(.*))?$" args)
    (unless match
      (error "Unexpected format in image tag: ~s" args))
    (let ((flags (aref strings 1)))
      `(:image ,(aref strings 0) ,@(if flags (list flags) nil)))))

(defun process-markup-tag (name args)
  (labels ((ensure-string-arg (s)
             (unless (and (listp s)
                          (= (length s) 1)
                          (stringp (car s)))
               (error "Single argument expected"))
             (car s))
           (parse-mxref-arg (args)
             (let* ((string (ensure-string-arg args))
                    (pos (position #\, string)))
               (unless pos
                 (error "mxref tag without a separator: ~s" args))
               (list (string-trim " " (subseq string 0 pos))
                     (string-trim " " (subseq string (1+ pos)))))))
    (let ((tag (find name *simple-tags* :key #'car :test #'equal)))
      (cond (tag
             (list (cons (cadr tag) args)))
            (t
             (string-case:string-case (name)
               ("code" (list (cons :code args)))
               ("mrefcomma" `((:mref ,(ensure-string-arg args)) ","))
               ("mrefdot" `((:mref ,(ensure-string-arg args)) "."))
               ("mrefparen" `((:mref ,(ensure-string-arg args)) ")"))
               ("mxref" `((:mxref ,@(parse-mxref-arg args))))
               ("mxrefcomma" `((:mxref ,@(parse-mxref-arg args)) ","))
               ("mxrefdot" `((:mxref ,@(parse-mxref-arg args)) "."))
               ("mxrefparen" `((:mxref ,@(parse-mxref-arg args)) ")"))
               ("image" (list (parse-image (ensure-string-arg args))))
               ("figure" `((:image ,(format nil "figures/~a" (ensure-string-arg args)))))
               ("w" nil)
               ("dots" '("…"))
               ("TeX" '("LaTeX"))
               (t (error "Undefined tag: ~s" name))))))))

(defun compress-strings (list)
  (collectors:with-collector (coll)
    (loop
      with curr-string = nil
      for e in list
      if (stringp e)
        do (setq curr-string (if curr-string (concatenate 'string curr-string e) e))
      else
        do (progn
             (when curr-string
               (coll curr-string)
               (setq curr-string nil))
             (coll e))
      finally (when curr-string
                (coll curr-string)))
    (coll)))

(defun no-recurse-tag-p (name)
  (member name '("anchor") :test #'equal))

(defun no-latex-tag-p (name)
  (member name '("anchor" "code" "kbd" "mrefdot") :test #'equal))

(defun parse-paragraph-content (s start recursive-p no-recurse no-latex)
  (collectors:with-collector (coll)
    (let ((len (length s))
          (beg start)
          (i start))
      (labels ((collect-str ()
                 (when (> i beg)
                   (coll (subseq s beg i)))))
        (loop
          while (< i len)
          do (let ((ch (aref s i)))
               (cond ((eql ch #\})
                      (if recursive-p
                          (progn
                            (collect-str)
                            (return (values (compress-strings (coll)) (1+ i))))
                          (error "Unexpected end-of-arglist character")))
                     ((eql ch #\{)
                      (when no-recurse
                        (error "Found opening brace when recursion is disabled"))
                      (collect-str)
                      (multiple-value-bind (result next-pos)
                          (parse-paragraph-content s (1+ i) t nil no-latex)
                        (dolist (entry result)
                          (coll entry))
                        (setq beg next-pos)
                        (setq i next-pos)))
                     ((eql ch #\@)
                      (cond ((and (< i (1- len))
                                  (let ((ch (aref s (1+ i))))
                                    (not (cl-ppcre:scan "^[a-zA-Z]$" (format nil "~c" ch)))))
                             ;; This is an escaped @-sign
                             (collect-str)
                             (setq beg (1+ i))
                             (incf i 2))
                            ((and (< i (1- len))
                                  (eql (aref s (1+ i)) #\-))
                             (collect-str)
                             (setq i (+ i 2))
                             (setq beg i))
                            ((and (< i (1- len))
                                  (eql (aref s (1+ i)) #\*))
                             (collect-str)
                             (coll '(:newline))
                             (setq i (+ i 2))
                             (setq beg i))
                            (no-recurse
                             (incf i))
                            (t
                             (let ((pos (position #\{ s :start (1+ i))))
                               (unless pos
                                 (error "No arglist found"))
                               (collect-str)
                               (let ((name (subseq s (1+ i) pos)))
                                 (multiple-value-bind (result next-pos)
                                     (parse-paragraph-content s (1+ pos) t (no-recurse-tag-p name) (no-latex-tag-p name))
                                   (let ((processed (process-markup-tag name result)))
                                     (dolist (val processed)
                                       (coll val)))
                                   (setq beg next-pos)
                                   (setq i next-pos)))))))
                     ((and (not no-latex)
                           (eql ch #\$)
                           (< i (1- len))
                           (eql (aref s (1+ i)) #\$))
                      ;; A LaTeX block should be appended as :CODE
                      (collect-str)
                      (let ((pos (search "$$" s :start2 (+ i 2))))
                        (unless pos
                          (error "No end of LaTeX section block found"))
                        (coll (list :code (format nil "$$~a$$"  (subseq s (+ i 2) pos))))
                        (setq i (+ pos 2))
                        (setq beg i)))
                     ((and (not no-latex)
                           (eql ch #\$))
                      (collect-str)
                      (let ((pos (position #\$ s :start (1+ i))))
                        (unless pos
                          (error "No end of inline LaTeX maths found"))
                        (coll (list :code (format nil "$~a$" (subseq s (1+ i) pos))))
                        (setq i (1+ pos))
                        (setq beg i)))
                     (t
                      ;; ELSE: Check next character
                      (incf i))))
          finally (progn
                    (when recursive-p
                      (error "End of string while scanning for end-of-arglist character"))
                    (collect-str)
                    (return (values (compress-strings (coll)) len))))))))

(defun parse-paragraph (s)
  (cons :paragraph (parse-paragraph-content s 0 nil nil nil)))

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

(defun process-demo-code (stream extended)
  (multiple-value-bind (code input)
      (collectors:with-collectors (coll input-coll)
        (loop
          with curr = ""
          for s = (read-line stream)
          until (if extended
                    (cl-ppcre:scan "^@c ===endx===" s)
                    (cl-ppcre:scan "^@c ===end===" s))
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
                   (:example-info . ,(remove-group-entries example))))))

(defun parse-arglist (args-string)
  (if (and args-string (plusp (length args-string)))
      (let ((parsed-arglist (with-input-from-string (s args-string)
                              (parse-stream s nil))))
        (unless (and (alexandria:sequence-of-length-p parsed-arglist 1)
                     (eq (caar parsed-arglist) :paragraph))
          (error "Unexpected arglist format: ~s" args-string))
        (cdar parsed-arglist))
      ;; ELSE: no arglist, just return nil
      nil))

(defun parse-fnames (stream)
  (loop
    with completed-p = nil
    for line = (read-line stream)
    collect (multiple-value-bind (match strings)
                (cl-ppcre:scan-to-strings "^@fname{([^ }]+)} +(.*[^ ]) *$" line)
              (unless match
                (error "Unexpected format for @fname row: ~s" line))
              (let* ((fname-name (aref strings 0))
                     (fname-args (aref strings 1))
                     (continuation-marker (cl-ppcre:scan " +@$" fname-args)))
                (if continuation-marker
                    ;; If the line ends with a @, then trim off the last character
                    (setq fname-args (subseq fname-args 0 continuation-marker))
                    ;; ELSE: We're done
                    (setq completed-p t))
                (let ((parsed-arglist (parse-arglist fname-args)))
                  (list fname-name parsed-arglist))))
    until completed-p))

(defun parse-fname-entry (string)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^@fname{([^ }]+)} +(.*[^ ]) *$" string)
    (unless match
      (error "Unexpected format for @fname row: ~s" string))
    (let* ((fname-name (aref strings 0))
           (fname-args (aref strings 1))
           (parsed-arglist (parse-arglist fname-args)))
      (list fname-name parsed-arglist))))

(defun parse-fname-list (string)
  (let ((indexes (loop
                   with index = 0
                   for pos = (multiple-value-bind (start end)
                                 (cl-ppcre:scan "@fname" string :start index)
                               (when start
                                 (setq index end)
                                 start))
                   while pos
                   collect pos)))
    (list (parse-arglist (if indexes
                             (subseq string 0 (car indexes))
                             string))
          (loop
            for (first . rest) on indexes
            collect (parse-fname-entry (subseq string first (or (car rest) (length string))))))))

#+nil
(defun parse-deffn (stream type name args anchors)
  (let ((parsed-arglist (parse-arglist args)))
    (list* :deffn `(,type
                    ,name
                    :arglist ,parsed-arglist
                    ,@(if anchors (list :anchors anchors)))
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

(defun parse-deffn (stream type name args anchors)
  (destructuring-bind (parsed-args fname-list)
      (parse-fname-list args)
    (list* :deffn `(,type
                    ,name
                    :arglist ,parsed-args
                    ,@(if fname-list (list :definitions fname-list) nil)
                    ,@(if anchors (list :anchors anchors) nil))
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

#+nil
(defun parse-multiple-deffn (stream type name args anchors)
  (let ((definitions (parse-fnames stream)))
    (list* :deffn `(,type
                    ,name
                    :arglist ,(parse-arglist args)
                    :definitions ,definitions
                    ,@(if anchors (list :anchors anchors)))
           (parse-stream stream (cl-ppcre:create-scanner "^@end +deffn")))))

(defun parse-defvr (stream type name args anchors)
  (list* :defvr `(,type
                  ,name
                  :arglist ,(parse-arglist args)
                  ,@(if anchors (list :anchors anchors)))
         (parse-stream stream (cl-ppcre:create-scanner "^@end +defvr"))))

#+nil
(defun parse-multiple-defvr (stream type name args anchors)
  (let ((definitions (parse-fnames stream)))
    (list* :deffn `(,type
                    ,name
                    :arglist ,(parse-arglist args)
                    :definitions ,definitions
                    ,@(if anchors (list :anchors anchors)))
           (parse-stream stream (cl-ppcre:create-scanner "^@end +defvr")))))

(defun collect-items (stream item-scanner end-tag-scanner)
  (collectors:with-collector (coll)
    (loop
      with curr-item = nil
      with curr-item-set = nil
      while (multiple-value-bind (content terminating-string)
                (parse-stream stream
                              item-scanner
                              (if curr-item (list curr-item) nil))
              (multiple-value-bind (item-match item-strings)
                  (cl-ppcre:scan-to-strings "^@item(?: +(.*[^ ]))? *$" terminating-string)
                (labels ((collect-item ()
                           (when curr-item-set
                             (coll content))))
                  (if item-match
                      (progn
                        (collect-item)
                        (setq curr-item (aref item-strings 0))
                        (setq curr-item-set t)
                        t)
                      ;; ELSE: Must be end of table, but let's ensure that's the case
                      (if (cl-ppcre:scan end-tag-scanner terminating-string)
                          ;; Found end of table, return nil to stop iterating
                          (progn
                            (collect-item)
                            nil)
                          ;; ELSE: The first regex should not allow anything through to this point
                          (error "When parsing table, first regex suceeded, but the second failed: ~s" terminating-string)))))))
    (coll)))

(defun parse-itemize (stream args)
  (let ((flags (if (cl-ppcre:scan "@bullet" args)
                   '(:bullet)
                   nil))
        (items (collect-items stream
                              (load-time-value (cl-ppcre:create-scanner "^@(?:(?:item(?: .*[^ ])?)|(?:end itemize)) *$"))
                              (load-time-value (cl-ppcre:create-scanner "^@end itemize *$")))))
    (list* :itemize flags items)))

(defun parse-enumerate (stream)
  (let ((items (collect-items stream
                              (load-time-value (cl-ppcre:create-scanner "^@(?:(?:item(?: .*[^ ])?)|(?:end enumerate)) *$"))
                              (load-time-value (cl-ppcre:create-scanner "^@end enumerate *$")))))
    (list* :enumerate nil items)))

(defun parse-table (stream args)
  (let ((flags (if (cl-ppcre:scan "@code" args)
                   '(:code)
                   nil)))
    (collectors:with-collector (coll)
      (loop
        with curr-item = nil
        with curr-item-set = nil
        while (multiple-value-bind (content terminating-string)
                  (parse-stream stream (load-time-value (cl-ppcre:create-scanner "^@(?:(?:item(?: .*[^ ])?)|(?:end table)) *$")))
                (multiple-value-bind (item-match item-strings)
                    (cl-ppcre:scan-to-strings "^@item(?: +(.*[^ ]))? *$" terminating-string)
                  (labels ((collect-item ()
                             (when curr-item-set
                               (coll (list* :item curr-item content)))))
                    (if item-match
                        (progn
                          (collect-item)
                          (setq curr-item (aref item-strings 0))
                          (setq curr-item-set t)
                          t)
                        ;; ELSE: Must be end of table, but let's ensure that's the case
                        (if (cl-ppcre:scan "^@end table *$" terminating-string)
                            ;; Found end of table, return nil to stop iterating
                            (progn
                              (collect-item)
                              nil)
                            ;; ELSE: The first regex should not allow anything through to this point
                            (error "When parsing table, first regex suceeded, but the second failed: ~s" terminating-string)))))))
      (list* :table flags (coll)))))

(defun parse-ifhtml (stream)
  (let ((content (skip-block stream "^@end ifhtml")))
    (loop
      for row in content
      append (multiple-value-bind (match strings)
                 (cl-ppcre:scan-to-strings "@image{([^}]+)}" row)
               (if match
                   (list (list :image (aref strings 0)))
                   nil)))))

(defun trim-comment (s)
  (if s
      (string-trim " "
                   (let ((pos (search "@c " s)))
                     ;; Lines starting with @c might need special treatment
                     (if (and pos (plusp pos))
                         (subseq s 0 pos)
                         s)))
      nil))

(defun remove-group-entries (rows)
  (remove-if (lambda (row)
               (or (alexandria:starts-with-subseq "@group" row)
                   (alexandria:starts-with-subseq "@end group" row)))
             rows))

(defun parse-stream (stream &optional end-tag initial-content)
  (collectors:with-collector (info-collector)
    (let ((current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
          (terminating-string nil)
          (current-anchors nil))
      (labels ((collect-paragraph ()
                 (when (plusp (length current-paragraph))
                   (info-collector (parse-paragraph current-paragraph))
                   (setq current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
                 (prog1
                     (sort current-anchors #'string<)
                   (setq current-anchors nil))))
        (loop
          with injected-s = initial-content
          for line = (if injected-s
                         (prog1 (car injected-s) (setq injected-s (cdr injected-s)))
                         (read-line-with-continue stream))
          for s = (trim-comment line)
          do (setq terminating-string s)
          until (or (null s)
                    (and end-tag
                         (cl-ppcre:scan end-tag s)))
          do (process-single-line-command s
               (("^@c ===beg(x?)===" args)
                (collect-paragraph)
                (info-collector (process-demo-code stream (not (equal (aref args 0) "")))))

               (("^@menu *$")
                (collect-paragraph)
                (info-collector (process-menu stream)))

               (("^@opencatbox *$")
                (collect-paragraph)
                (info-collector (process-opencatbox stream)))

               (("^@node +(.*)$" args)
                (collect-paragraph)
                (info-collector (cons :node (parse-node (aref args 0)))))

               (("@chapter +(.*[^ ]) *$" name)
                (collect-paragraph)
                (info-collector (list :chapter (aref name 0))))

               (("@section +(.*[^ ]) *$" name)
                (collect-paragraph)
                (info-collector (list :section (aref name 0))))

               (("@subsection +(.*[^ ]) *$" name)
                (collect-paragraph)
                (info-collector (list :subsection (aref name 0))))

               (("@subheading +(.*[^ ]) *$" name)
                (collect-paragraph)
                (info-collector (list :subheading (aref name 0))))

               (("^@c(?: |$)") nil)

               (("^@ifhtml *$")
                (dolist (image (parse-ifhtml stream))
                  (info-collector image)))

               (("^@iftex *$")
                (skip-block stream "^@end iftex"))

               #+nil
               (("^@deffn *{([^}]+)} +([^ ]+)(?: *(.*[^ ]))? +@ *$" args)
                (let ((anchors (collect-paragraph)))
                  (info-collector (parse-multiple-deffn stream (aref args 0) (aref args 1) (aref args 2) anchors))))

               (("^@deffn *{([^}]+)} +([^ ]+)(?: +(.*[^ @]))? *$" args)
                (let ((anchors (collect-paragraph)))
                  (info-collector (parse-deffn stream (aref args 0) (aref args 1) (aref args 2) anchors))))

               #+nil
               (("^@defvr +{([^}]+)} +(.*[^ ])(?: *(.*[^ ]))? +@ *$" args)
                (let ((anchors (collect-paragraph)))
                  (info-collector (parse-multiple-defvr stream (aref args 0) (aref args 1) (aref args 2) anchors))))

               (("^@defvr +{([^}]+)} +(.*[^ ])(?: *(.*[^ ]))? *$" args)
                (let ((anchors (collect-paragraph)))
                  (info-collector (parse-defvr stream (aref args 0) (aref args 1) (aref args 2) anchors))))

               (("^@deffnx")
                ;; TODO: Currently this is being ignored, but it
                ;; should render a function definition that is
                ;; dependent on a variable.
                nil)

               (("^@defvrx")
                ;; TODO: Currently this is being ignored, but it
                ;; should render a function definition that is
                ;; dependent on a variable.
                nil)

               (("^@vrindex +(.*[^ ]+) *$")
                ;; TODO: Add vrindex to the output
                nil)

               (("^@example *$")
                (collect-paragraph)
                (info-collector (cons :pre (remove-group-entries (skip-block stream "^@end example")))))

               (("^@itemize(?: +(.*))?$" args)
                (collect-paragraph)
                (info-collector (parse-itemize stream (aref args 0))))

               (("^@table(?: +(.*))?$" args)
                (collect-paragraph)
                (info-collector (parse-table stream (aref args 0))))

               (("^@enumerate(?: |$)")
                (collect-paragraph)
                (info-collector (parse-enumerate stream)))

               (("^@anchor{([^}]+)} *$" args)
                (push (aref args 0) current-anchors))

               (("^@verbatim *$")
                (collect-paragraph)
                (info-collector (cons :verbatim (skip-block stream "^@end verbatim"))))

               (("^ *$")
                (collect-paragraph))

               (("^@ifinfo *$")
                (let ((content (parse-stream stream "^@end ifinfo")))
                  (dolist (entry content)
                    (info-collector entry))))

               (("^@need +[0-9]+ *$")
                nil)

               (("^@iftex *$")
                (skip-block stream "^@end iftex"))

               (("^@tex *$")
                (skip-block stream "^@end tex"))

               (("^@noindent *$") nil)
               (("^@ifnottex *$") nil)
               (("^@end ifnottex *$"))

               (("^@([a-z]+)(?: +(.*[^ ]))? *$" args)
                (log:warn "Unknown tag: ~s, args: ~s" (aref args 0) (aref args 1)))

               (t
                ;; This is normal content, so we'll collect it into the output string
                (when (plusp (length current-paragraph))
                  (vector-push-extend #\Space current-paragraph))
                (loop for ch across s do (vector-push-extend ch current-paragraph)))))
        (collect-paragraph)
        (values (info-collector) terminating-string)))))

(defun parse-file (file initial-content)
  (log:trace "Parsing file: ~s" file)
  (with-open-file (stream file :direction :input :external-format :utf-8)
    (parse-stream stream nil initial-content)))

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

(defun %evaluate-demo-src (src standard-input-content)
  (uiop:with-temporary-file (:stream input)
    (with-standard-io-syntax
      (let ((*print-circle* t))
        (print (list src standard-input-content) input)))
    (close input)
    (uiop:with-temporary-file (:stream output)
      (uiop:with-temporary-file (:stream error-out)
        (handler-case
            (let* ((dir (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/"))
                   (exec-file (merge-pathnames "maxima-parser.bin" dir))
                   (exec-name (namestring exec-file)))
              (uiop:run-program (list exec-name "--dynamic-space-size" "3000" (namestring (pathname output)))
                                :input (pathname input)
                                :error-output (pathname error-out))
              (close output)
              (with-open-file (s (pathname output) :external-format :utf-8)
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

(defun evaluate-demo-src (src standard-input-content)
  (when (or (equal (car src) "w[i,j] := random (1.0) + %i * random (1.0);")
            (loop
              for v in src
              when (member-if (lambda (p)
                                (alexandria:starts-with-subseq p v))
                              '("plot2d" "plot3d" "implicit_plot" "contour_plot" "mandelbrot" "julia"
                                "geomview_command" "gnuplot_command" "draw_graph" "barsplot"
                                "histogram" "piechart" "plotdf" "ploteq" "scene"))
                return t))
    (log:warn "Skipping example: ~s" src)
    (return-from evaluate-demo-src (list :error "Skip example")))
  (log:trace "Evaluating code: ~s. input: ~s" src standard-input-content)
  (let ((res (%evaluate-demo-src src standard-input-content)))
    (log:trace "Result: ~s" res)
    res))

(defun resolve-example-code (info-content)
  (labels ((parse-branch (nodes)
             (loop
               for v in nodes
               if (listp v)
                 collect (if (eq (car v) :demo-code)
                             (let* ((src (cdr (assoc :demo-source (cdr v))))
                                    (input (cdr (assoc :input (cdr v))))
                                    (example-info (cdr (assoc :example-info (cdr v))))
                                    (example-output (evaluate-demo-src src input)))
                               `(:demo-code (:demo-source . ,src)
                                            (:example-info . ,example-info)
                                            ,@(if (eq (car example-output) :success)
                                                  (list (cons :demo-result (second example-output)))
                                                  nil)))
                             (parse-branch v))
               else
                 collect v)))
    (parse-branch info-content)))

(defun parse-and-write-file (file destination-directory &key skip-example initial-content)
  (log:info "Parsing file: ~s" file)
  (let* ((content (parse-file file initial-content))
         (processed (if skip-example
                        content
                        (resolve-example-code content))))
    (write-parsed-content-to-file processed
                                  (make-pathname :name (pathname-name file)
                                                 :type "lisp"
                                                 :defaults destination-directory))))

(defun write-parsed-content-to-file (processed file)
  (with-open-file (s file
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-circle* t)
            (*print-pretty* t))
        (print processed s))))
  nil)

(defun parse-doc-directory (info-directory destination-directory &key skip-example)
  "Parse all texinfo files in the given directory and output
corresponding lisp files to the output directory."
  (let ((dir (merge-pathnames "*.texi" info-directory)))
    (dolist (file (directory dir))
      (unless (member (pathname-name file) '("category-macros") :test #'equal)
        (parse-and-write-file file destination-directory :skip-example skip-example)))))

(defun make-multiline-string (lines)
  (with-output-to-string (s)
    (loop
      for line in lines
      do (format s "~a~%" line))))

(defun scan-categories (definition-type type name hash content)
  (loop
    for v in content
    when (eq (car v) :catbox)
      do (loop for category-name in (cdr v)
               do (let ((refs (gethash category-name hash)))
                    (setf (gethash category-name hash)
                          (cons (list definition-type type name) refs))))))

(defun extract-functions (content functions-hash nodes-hash categories file)
  (loop
    for v in content
    when (and (listp v)
              (or (eq (car v) :deffn)
                  (eq (car v) :defvr)))
      do (destructuring-bind (type fn &key anchors arglist definitions)
             (second v)
           (declare (ignore arglist definitions))
           (let ((prev (gethash fn functions-hash)))
             (setf (gethash fn functions-hash) (cons (list (car v) file type) prev))
             (dolist (name anchors)
               (alexandria:if-let ((v (gethash name functions-hash)))
                 (when (and (eq (car v) :ref)
                            (not (equal (cadr v) fn)))
                   (error "Incompatible refererences from ~s" name))
                 ;; ELSE: No definition, add the reference
                 (setf (gethash name functions-hash) (list :ref fn))))
             (scan-categories (car v) type fn categories (cddr v))))
    when (and (listp v)
              (eq (car v) :node))
      do (let ((name (second v)))
           (setf (gethash name nodes-hash) (list (cdr v) file)))))

(defun resolve-destination-dir ()
  (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/"))

(defun generate-index ()
  (let ((destination-dir (resolve-destination-dir))
        (index-filename "index")
        (functions (make-hash-table :test 'equal))
        (nodes (make-hash-table :test 'equal))
        (categories (make-hash-table :test 'equal)))
    (loop
      for file in (directory (merge-pathnames #p"*.lisp" destination-dir))
      for name = (pathname-name file)
      unless (equal name index-filename)
        do (let  ((content (with-open-file (s file :external-format :utf-8)
                             (with-standard-io-syntax
                               (read s)))))
             (extract-functions content functions nodes categories name)))
    (let* ((content-list (loop
                           for key being each hash-key in functions using (hash-value value)
                           collect (cons key value)))
           (symbols-sorted (sort content-list (lambda (o1 o2)
                                                (string< (string-downcase o1) (string-downcase o2)))
                                 :key #'car))
           (nodes-sorted (sort (loop for v being each hash-value in nodes collect v)
                               (lambda (o1 o2)
                                 (string< (string-downcase o1) (string-downcase o2)))
                               :key #'caar))
           (categories-sorted (sort (collectors:with-collector (categories-collector)
                                      (maphash (lambda (key value)
                                                 (categories-collector (cons key value)))
                                               categories)
                                      (categories-collector))
                                    #'string<
                                    :key #'car)))
      (with-open-file (s (merge-pathnames (format nil "~a.lisp" index-filename) destination-dir)
                         :direction :output
                         :if-exists :supersede
                         :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (print `((:symbols . ,symbols-sorted)
                     (:nodes . ,nodes-sorted)
                     (:categories . ,categories-sorted))
                   s))))
      nil)))

(defun resolve-example-code-external ()
  "This function is called from the standalone maxima expression evaluator."
  (let ((args (unix-opts:argv)))
    (unless (alexandria:sequence-of-length-p args 2)
      (error "Missing argument"))
    (let ((result (handler-case
                      (progn
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
                                                            (cons :no-result nil)
                                                            ;; ELSE: Normal command, the result needs to be saved
                                                            (cons :result res))))))
                                (list :success result))))))
                    (error (condition)
                      (log:error "Error processing example code: ~a" condition)
                      (list :error (format nil "~a" condition))))))
      (with-open-file (s (second args) :direction :output :if-does-not-exist :create :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-circle* t))
            (print result s)))))))

(defun copy-file-to-dir (file dir)
  (let ((destination-name (merge-pathnames (format nil "~a.~a" (pathname-name file) (pathname-type file)) dir)))
    (uiop:copy-file file destination-name)))

(defun convert-pdf-to-png (file destination-name)
  #+use-gs
  (uiop:run-program (list "gs" "-sDEVICE=png16m" (format nil "-sOutputFile=~a" (namestring destination-name))
                          "-dNOPAUSE" "-dBATCH" "-dQUIET" "-r85" (namestring file)))
  #-use-gs
  (uiop:run-program (list "mutool" "draw" "-w" "800" "-o" (namestring destination-name) (namestring file))))

(defun convert-figures ()
  (let ((destination-dir (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/figures/")))
    (ensure-directories-exist destination-dir)
    (dolist (file (directory (merge-pathnames #p"*.*" (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/figures/"))))
      (multiple-value-bind (match strings)
          (cl-ppcre:scan-to-strings "\\.(pdf|png)" (namestring file))
        (when match
          (log:trace "Converting file: ~s" file)
          (let ((destination-name (merge-pathnames (format nil "~a.png" (pathname-name file))
                                                   destination-dir)))
            (string-case:string-case ((aref strings 0))
              ("png" (copy-file-to-dir file destination-dir))
              ("pdf" (convert-pdf-to-png file destination-name)))))))))

(defun read-line-with-continue (stream)
  (let ((first-line (read-line stream nil nil)))
    (if (null first-line)
        nil
        (with-output-to-string (out)
          (loop
            with line = first-line
            for first = t then nil
            unless first
              do (format out " ")
            do (multiple-value-bind (matched strings)
                   (cl-ppcre:scan-to-strings "^(.*)@ *$" line)
                 (cond (matched
                        (format out "~a" (aref strings 0)))
                       (t
                        (format out "~a" line)
                        (return nil)))
                 (setq line (read-line stream nil nil))
                 (unless line
                   (error "Last line is a continuation line"))))))))

(defun parse-toplevel-file (file destination-directory &key skip-example)
  (with-open-file (stream file :direction :input)
    (skip-block stream "^@end titlepage *$")
    (let ((main-content (with-output-to-string (out-stream)
                          (loop
                            for s = (read-line stream)
                            do (format out-stream "~a~%" s)
                            until (cl-ppcre:scan "^@end menu *" s)))))
      ;; At this point, we want to find all files specified using
      ;; @include directives. If the directive has a @node and
      ;; @chapter section before the @include, it should be included
      ;; before the file content.
      (loop
        with current-prefix = nil
        for s = (read-line-with-continue stream)
        while s
        do (multiple-value-bind (match strings)
               (cl-ppcre:scan-to-strings "^@include +([^ ]+) *$" s)
             (if match
                 (progn
                   (parse-and-write-file (merge-pathnames (aref strings 0)
                                                          (make-pathname :directory (pathname-directory file)))
                                         destination-directory
                                         :initial-content (reverse current-prefix)
                                         :skip-example skip-example)
                   (setq current-prefix nil))
                 (push s current-prefix))))
      (let ((parsed (with-input-from-string (instring main-content)
                      (parse-stream instring))))
        (write-parsed-content-to-file parsed (merge-pathnames maxima-client.doc-new:*maxima-toplevel-filename*
                                                              destination-directory))))))

(defun generate-doc-directory (&key skip-example skip-figures)
  "Generate lisp files for all the texinfo files in the maxima distribution."
  (let ((destination-directory (resolve-destination-dir)))
    (ensure-directories-exist destination-directory)
    #+nil
    (parse-doc-directory (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/")
                         destination-directory
                         :skip-example skip-example)
    (parse-toplevel-file (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/include-maxima.texi")
                         destination-directory
                         :skip-example skip-example)
    (parse-doc-directory (asdf:system-relative-pathname (asdf:find-system :maxima-client) "info/")
                         destination-directory
                         :skip-example skip-example)
    (generate-index)
    (unless skip-figures
      (convert-figures))))

(defun generate-one-file (name)
  (let ((file (merge-pathnames (format nil "~a.texi" name)
                               (asdf:system-relative-pathname (asdf:find-system :maxima) "../doc/info/"))))
    (parse-and-write-file file (resolve-destination-dir))))
