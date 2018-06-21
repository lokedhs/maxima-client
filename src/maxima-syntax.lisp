(in-package :maxima-client)

(drei-syntax:define-syntax-command-table maxima-table
  :errorp nil)

(drei::define-syntax maxima-syntax (drei-fundamental-syntax:fundamental-syntax)
  ()
  (:name "Maxima")
  (:pathname-types "mac")
  (:command-table maxima-table))

(defun find-maxima-completions (prefix &key (verbs t) (nouns t))
  (unless (or verbs nouns)
    (error "At least one of :VERBS or :NOUNS must be provided"))
  (let ((syms (loop
                with verb-prefix = (and verbs (concatenate 'string "$" prefix))
                with noun-prefix = (and nouns (concatenate 'string "%" prefix))
                with package = (find-package "MAXIMA")
                for sym being each symbol in package
                for sym-name-fixed = (format-sym-name sym)
                when (and (cl-ppcre:scan "^[$%][a-zA-Z0-9_]+$" sym-name-fixed)
                          (or (and verb-prefix (alexandria:starts-with-subseq verb-prefix sym-name-fixed))
                              (and noun-prefix (alexandria:starts-with-subseq noun-prefix sym-name-fixed))))
                  collect (let ((s (subseq sym-name-fixed 1)))
                            (list s (list s (function-signature sym)))))))
    #+nil(remove-duplicates (sort syms #'string<) :test #'equal)
    syms))

(defun symbol-constituent-p (ch)
  (let ((code (char-code ch)))
    (or (<= (char-code #\a) code (char-code #\z))
        (<= (char-code #\A) code (char-code #\Z))
        (<= (char-code #\0) code (char-code #\9))
        (eql ch #\_))))

(defun insert-completed-symbol (mark string)
  (loop
    until (drei::beginning-of-buffer-p mark)
    for ch = (drei::object-before mark)
    while (symbol-constituent-p ch)
    do (drei-buffer:backward-object mark))
  (loop
    until (drei::end-of-buffer-p mark)
    for ch = (drei::object-after mark)
    while (symbol-constituent-p ch)
    do (drei-editing::forward-delete-object mark))
  (drei-buffer:insert-sequence mark string))

(clim:define-command (complete-maxima-function :name "Complete function" :command-table maxima-table)
    ()
  "Complete the name of the function at the curstor position."
  (let* ((point (drei:point))
         (offset (drei::offset point))
         (string (drei-buffer:buffer-substring (drei:buffer point) 0 offset)))
    (multiple-value-bind (match strings)
        (cl-ppcre:scan-to-strings "([a-zA-Z_][a-zA-Z0-9_]*)$" string)
      (when match
        (let ((matches (find-maxima-completions (aref strings 0))))
          (cond ((null matches)
                 (esa:display-message "No completions"))
                ((alexandria:sequence-of-length-p matches 1)
                 (insert-completed-symbol point (caar matches)))
                (t
                 (let ((result (select-completion-match matches)))
                   (when result
                     (insert-completed-symbol point result)))))))))
  nil)

(drei::set-key 'complete-maxima-function
               'maxima-table
               '((#\Tab)))

