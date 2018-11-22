(in-package :infoparser)

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
        (markup-from-regexp "@(code|var|mref){([^}]+)}" s
                            (lambda (reg-starts reg-ends)
                              (let ((cmd (subseq s (aref reg-starts 0) (aref reg-ends 0)))
                                    (content (subseq s (aref reg-starts 1) (aref reg-ends 1))))
                                (list (string-case:string-case (cmd)
                                        ("code" :code)
                                        ("var" :var)
                                        ("mref" :mref))
                                      content))))))

(defun parse-file (file)
  (collectors:with-collector (info-collector)
    (let ((current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
      (labels ((collect-paragraph ()
                 (when (plusp (length current-paragraph))
                   (info-collector (parse-paragraph current-paragraph))
                   (setq current-paragraph (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))))
        (with-open-file (stream file :direction :input :external-format :utf-8)
          (loop
            for s = (read-line stream nil nil)
            while s
            do (cond ((alexandria:starts-with-subseq "@" s)
                      (cond ((cl-ppcre:scan "^@menu *$" s)
                             (info-collector (process-menu stream)))
                            ((cl-ppcre:scan "^@c +" s)
                             ;; Comment
                             nil)
                            (t
                             (log:warn "Unexpected command: ~s" s)
                             nil)))
                     ((cl-ppcre:scan "^ *$" s)
                      (collect-paragraph))
                     (t
                      ;; This is normal content, so we'll collect it into the output string
                      (when (plusp (length current-paragraph))
                        (vector-push-extend #\Space current-paragraph))
                      (loop for ch across s do (vector-push-extend ch current-paragraph))))))
        (collect-paragraph)
        (info-collector)))))
