(in-package :maxima-client.markup)

(clim:define-command-table text-commands)

(defclass text-link ()
  ((src :initarg :src
        :reader text-link/src)
   (description :initarg :description
                :initform nil)))

(defgeneric text-link/description (obj)
  (:method ((obj text-link))
    (or (slot-value obj 'description)
        (text-link/src obj))))

(defun make-text-link-from-markup (args)
  (cond ((alexandria:sequence-of-length-p args 1)
         (make-instance 'text-link :src (first args)))
        ((alexandria:sequence-of-length-p args 2)
         (make-instance 'text-link :src (first args) :description (second args)))
        (t
         (error "Illegally formatted text-link entry: ~s" args))))

(clim:define-presentation-method clim:present (obj (type text-link) stream (view t) &key)
  (clim:with-drawing-options (stream :ink clim:+blue+)
    (format stream "~a" (text-link/description obj))))

(clim:define-presentation-translator text-to-text-link (string text-link text-commands)
    (object)
  (make-instance 'text-link :src object))

(clim:define-presentation-to-command-translator select-url
    (text-link open-url text-commands)
    (obj)
  (list obj))

(clim:define-command (open-url :name "Open URL" :menu t :command-table text-commands)
    ((url 'text-link :prompt "URL"))
  (let ((s (text-link/src url)))
    (format t "Opening URL: ~a" s)
    (bordeaux-threads:make-thread (lambda ()
                                    (uiop/run-program:run-program (list "xdg-open" s))))))

(defun display-markup-list (stream content)
  (log:info "displaying markup list: ~s" content)
  (loop
    for v in content
    do (display-markup stream v)))

(defun display-possibly-tagged-list (stream content)
  (labels ((display (v)
             (display-markup-list stream v)))
   (when (null content)
     (return-from display-possibly-tagged-list nil))
    (case (car content)
      (:heading (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :large)) (display (cdr content))))
      (:bold (clim:with-text-face (stream :bold) (display (cdr content))))
      (:code (clim:with-text-family (stream :fix) (display (cdr content))))
      (:link (maxima-client::present-to-stream (make-text-link-from-markup (cdr content)) stream))
      (:p (format stream "~&") (display (cdr content)))
      (:newline (format stream "~%"))
      (t (display-markup-list stream content)))))

(defun display-markup (stream content)
  (etypecase content
    (string (present-multiline-with-wordwrap stream content))
    (list (display-possibly-tagged-list stream content))))

