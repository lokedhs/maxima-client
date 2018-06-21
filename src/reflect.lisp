(in-package :maxima-client)

(defun arglist-from-function (name)
  (with-output-to-string (out)
    (format out "(")
    (let ((arg-type-state :initial)
          (first t))
      (labels ((print-arg (arg)
                 (if first
                     (setq first nil)
                     (format out ", "))
                 (let ((arg-fixed (format-sym-name arg)))
                   (ecase arg-type-state
                     (:initial (format out "~a" arg-fixed))
                     (:optional (format out "(~a)" arg-fixed))
                     (:rest (format out "[~a]" arg-fixed))
                     (:aux nil)))))
        (loop
          for arg in (trivial-arguments:arglist name)
          do (cond ((eq arg '&optional)
                    (setq arg-type-state :optional))
                   ((eq arg '&aux)
                    (setq arg-type-state :aux))
                   ((eq arg '&rest)
                    (setq arg-type-state :rest))
                   ((eq arg '&key)
                    (setq arg-type-state :key))
                   ((listp arg)
                    (when (eq arg-type-state :initial)
                      (error "Default argument provided in initial params: ~s" name))
                    (print-arg (if (listp (car arg)) (caar arg) (car arg))))
                   ((symbolp arg)
                    (print-arg arg))))))
    (format out ")")))

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
