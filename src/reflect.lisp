(in-package :maxima-client)

(defun arglist-from-function (name)
  (with-output-to-string (out)
    (format out "(")
    (loop
      with arg-type-state = nil
      for arg in (trivial-arguments:arglist name)
      do (cond ((eq arg '&optional)
                (setq arg-type-state :optional))
               ((eq arg '&aux)
                (setq arg-type-state :aux))
               ((eq arg '&rest)
                (setq arg-type-state :rest))
               ((eq arg '&key)
                (setq arg-type-state :key))
               (t
                (format out "a"))))))

(defun arglist-from-maxima-function (name)
  (labels ((format-list-entry (v)
             (etypecase v
               (symbol (format-sym-as-string v))
               (list (progn
                       (assert (eq (caar v) 'maxima::mlist))
                       (format nil "[~{~a~^, ~}]" (mapcar #'format-list-entry (cdr v))))))))
    (let ((mexpr (or (maxima::mget name 'maxima::mexpr)
                     (maxima::mget name 'maxima::mmacro))))
      (cond (mexpr
             ;; I'm not entirely certain about how the content of the
             ;; function definition can vary, so let's add some assertions
             ;; that encodes the current understanding of the situation.
             (assert (eq (caar mexpr) 'lambda))
             (let ((arglist (second mexpr)))
               (assert (eq (caar arglist) 'maxima::mlist))
               (format nil "M:(~{~a~^, ~})" (mapcar #'format-list-entry (cdr arglist)))))
            (t
             "variable")))))

(defun function-signature (name)
  (check-type name symbol)
  (if (fboundp name)
    (arglist-from-function name)
    ;; ELSE: Possibly a Maxima function?
    (arglist-from-maxima-function name)))
