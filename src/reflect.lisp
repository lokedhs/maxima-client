(in-package :maxima-client)

(defun arglist-from-function (name)
  (let ((arg-list (trivial-arguments:arglist name)))
    ;; In some cases, TRIVIAL-ARGUMENTS:ARGLIST returns an improper
    ;; list. I suspect this is a problem with trivial-arguments. Until
    ;; it has been investigated, we'll just skip that case.
    (unless (listp (cdr (last arg-list)))
      (return-from arglist-from-function nil))
    (with-output-to-string (out)
      (format out "(")
      (when (listp arg-list)
        (let ((arg-type-state :initial)
              (optionals nil)
              (first t))
          (labels ((print-arg (arg)
                     (if first
                         (setq first nil)
                         (format out " "))
                     (let ((arg-fixed (format-sym-name arg :any-sym t)))
                       (ecase arg-type-state
                         (:initial (format out "~a" arg-fixed))
                         (:optional (push arg-fixed optionals))
                         (:rest (format out "...~a" arg-fixed))
                         (:aux nil))))
                   (collect-optional ()
                     (when optionals
                       (format out "[~{~a~^ ~}]" (reverse optionals))
                       (setf optionals nil))))
            (loop
              for arg in arg-list
              do (cond ((eq arg '&optional)
                        (setq arg-type-state :optional))
                       ((eq arg '&aux)
                        (collect-optional)
                        (setq arg-type-state :aux))
                       ((eq arg '&rest)
                        (collect-optional)
                        (setq arg-type-state :rest))
                       ((eq arg '&key)
                        (collect-optional)
                        (setq arg-type-state :key))
                       ((listp arg)
                        (when (eq arg-type-state :initial)
                          (error "Default argument provided in initial params: ~s" name))
                        (print-arg (if (listp (car arg)) (caar arg) (car arg))))
                       ((symbolp arg)
                        (print-arg arg)))
              finally (collect-optional)))))
      (format out ")"))))

(defun arglist-from-maxima-function (name)
  (labels ((format-list-entry (v)
             (etypecase v
               (symbol (format-sym-name v :any-sym t))
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
               (format nil "(~{~a~^, ~})" (mapcar #'format-list-entry (cdr arglist)))))
            (t
             "variable")))))

(defun function-signature (name)
  (check-type name symbol)
  (if (fboundp name)
      (arglist-from-function name)
      ;; ELSE: Possibly a Maxima function?
      (arglist-from-maxima-function name)))
