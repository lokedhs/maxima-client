(in-package :maxima-client.doc-new)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type node ()
    :description "Node in the documentation"))

(clim:define-command-table info-commands
  :inherit-from (maxima-client.markup:text-commands))

(defclass info-content-panel-view (maxima-client::maxima-renderer-view maxima-client.markup:markup-text-view)
  ())

(defvar *doc-frame-lock* (bordeaux-threads:make-lock "doc-frame-lock"))
(defvar *doc-frame* nil)

(defvar *index-symbols* nil)
(defvar *index-nodes* nil)
(defvar *index-categories* nil)

(defparameter +info-content-panel-view+ (make-instance 'info-content-panel-view))

(defclass info-content-panel (clim:application-pane)
  ((content :initform nil
            :accessor info-content-panel/content)))

(clim:define-application-frame documentation-frame ()
  ((initial-request :initform nil
                    :initarg :initial-request
                    :accessor documentation-frame/initial-request))
  (:panes (info-content (clim:make-pane 'info-content-panel
                                        :display-function 'display-text-content
                                        :default-view +info-content-panel-view+))
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       (4/5 (clim:scrolling ()
                              info-content))
                       (1/5 interaction-pane))))
  (:command-table (documentation-frame :inherit-from (info-commands))))

(defun process-documentation-request (frame command)
  (destructuring-bind (type name)
      command
    (let ((content (ecase type
                     (:function (load-function name))
                     (:file (load-doc-file name)))))
      (unless content
        (error "Documentation not found: ~s" command))
      (let ((info-content-panel (clim:find-pane-named frame 'info-content)))
        (setf (info-content-panel/content info-content-panel) content)))))

(defun display-function-help (name)
  (open-documentation-frame (list :function name)))

(defmethod clim:note-frame-enabled :after (fm (frame documentation-frame))
  (alexandria:when-let ((command (documentation-frame/initial-request frame)))
    (setf (documentation-frame/initial-request frame) nil)
    (process-documentation-request frame command)))

(defun display-documentation-frame (&optional command)
  (let ((frame (clim:make-application-frame 'documentation-frame
                                            :width 900
                                            :height 800
                                            :initial-request command)))
    (clim:run-frame-top-level frame)))

(defun open-documentation-frame (command)
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (if (null *doc-frame*)
        (let ((frame (clim:make-application-frame 'documentation-frame
                                                  :width 900
                                                  :height 800)))
          (setq *doc-frame* frame)
          (setf (documentation-frame/initial-request frame) command)
          (bordeaux-threads:make-thread (lambda ()
                                          (clim:run-frame-top-level frame))))
        ;; ELSE: Frame was already created, just run the command
        (when command
          (process-documentation-request *doc-frame* command)))))

(defmethod clim:frame-exit ((frame documentation-frame))
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (setq *doc-frame* nil))
  (call-next-method))

(defun display-text-content (frame panel)
  (declare (ignore frame))
  (let ((content (info-content-panel/content panel)))
    (when content
      (clim:with-room-for-graphics (panel :first-quadrant nil)
        (maxima-client.markup:display-markup panel content)))))

(defvar *doc-file-cache* nil)

(defmacro with-doc-file-cache (&body body)
  `(let ((*doc-file-cache* (make-hash-table :test 'equal)))
     ,@body))

(defun load-doc-file (name)
  (labels ((load ()
             (let* ((info-root-path (find-info-root-path))
                    (file (merge-pathnames (format nil "docs/~a.lisp" name) info-root-path))
                    (content (with-open-file (in file :external-format :utf-8)
                               (read in))))
               content))
           (load-from-cache ()
             (alexandria:ensure-gethash name *doc-file-cache* (load))))
    (if *doc-file-cache*
        (load-from-cache)
        (with-doc-file-cache
          (load-from-cache)))))

(defun load-index ()
  (when (or (null *index-symbols*)
            (null *index-nodes*)
            (null *index-categories*))
    (let* ((info-root-path (find-info-root-path))
           (file (merge-pathnames #p"docs/index.lisp" info-root-path))
           (content (with-open-file (in file :external-format :utf-8)
                      (read in))))
      (setq *index-symbols* (cdr (assoc :symbols content)))
      (setq *index-nodes* (cdr (assoc :nodes content)))
      (setq *index-categories* (cdr (assoc :categories content))))))

(defun load-node (name)
  (load-index)
  (let ((entry (find name *index-nodes* :test #'equal :key (lambda (v)
                                                             ;; Each entry consists of a node descriptor
                                                             ;; Each node descriptor consists of 4 elements
                                                             ;; The first element of each node descriptor is its name
                                                             (first (first v))))))
    (when entry
      (let ((file-content (load-doc-file (second entry))))
        ;; Find the content from the given node, until the next node
        (let ((start (member-if (lambda (v)
                                  (and (listp v)
                                       (eq (car v) :node)
                                       (equal (second v) name)))
                                file-content)))
          ;; Copy everything up until the next node
          (loop
            for v in (cdr start)
            until (and (listp v)
                       (eq (car v) :node))
            collect v))))))

(defun load-function (name)
  (load-index)
  (let ((entry (find name *index-symbols* :key #'car :test #'equal)))
    (unless entry
      (return-from load-function nil))
    (with-doc-file-cache
      (loop
        for (type file description) in (cdr entry)
        collect (let ((file-content (load-doc-file file)))
                  (let ((found (find-if (lambda (definition)
                                          (and (eq (car definition) type)
                                               (equal (second (second definition)) name)))
                                        file-content)))
                    (unless found
                      (error "symbol found in index but not in file: ~s type: ~s" name type))
                    (list :paragraph found)))))))

(define-documentation-frame-command (datatypes-command :name "datatypes")
    ()
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel) (load-doc-file "DataTypes"))))

(define-documentation-frame-command (file-command :name "file")
    ((file 'string :prompt "Filename"))
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel) (load-doc-file file))))

(define-documentation-frame-command (add-test-doc-command :name "testdoc")
    ()
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel)
          '((:MENU "Numbers" "Strings" "Constants" "Lists" "Arrays" "Structures")
            (:NODE "Numbers" "Strings" "Data Types and Structures" "Data Types and Structures")
            (:p (:url "http://www.reddit.com/"))
            (:p (:mref "diff"))
            (:SECTION "Numbers")
            (:pre "This" "is some text" "third line")
            (:MENU "Introduction to Numbers" "Functions and Variables for Numbers")
            (:NODE "Introduction to Numbers" "Functions and Variables for Numbers" "Numbers" "Numbers")
            (:SUBSECTION "Introduction to Numbers")
            (:PARAGRAPH "A complex expression is specified in Maxima by adding the real part of the expression to "
             (:CODE "%i") " times the imaginary part.  Thus the roots of the equation " (:CODE "x^2 - 4*x + 13 = 0")
             " are " (:CODE "2 + 3*%i") " and " (:CODE "2 - 3*%i")
             ". Note that simplification of products of complex expressions can be effected by expanding the product.  Simplification of quotients, roots, and other functions of complex expressions can usually be accomplished by using the "
             (:CODE "realpart") ", " (:CODE "imagpart") ", " (:CODE "rectform") ", " (:CODE "polarform")
             ", " (:CODE "abs") ", " (:CODE "carg") " functions.")
            (:CATBOX "Complex variables")
            (:NODE "Functions and Variables for Numbers" NIL "Introduction to Numbers" "Numbers")
            (:SUBSECTION "Functions and Variables for Numbers")
            (:PARAGRAPH (:ANCHOR "bfloat"))
            (:DEFFN ("Function" "bfloat" ("(" (:VAR "expr") ")"))
             (:PARAGRAPH "Converts all numbers and functions of numbers in " (:VAR "expr")
              " to bigfloat numbers. The number of significant digits in the resulting bigfloats is specified by the global variable "
              (:MREFDOT "fpprec"))
             (:PARAGRAPH "When " (:MREF "float2bf") " is " (:CODE "false")
              " a warning message is printed when a floating point number is converted into a bigfloat number (since this may lead to loss of precision).")
             (:CATBOX "Numerical evaluation"))
            (:PARAGRAPH (:ANCHOR "bfloatp"))
            (:DEFFN ("Function" "bfloatp" ("(" (:VAR "expr") ")"))
             (:PARAGRAPH "Returns " (:CODE "true") " if " (:VAR "expr") " is a bigfloat number, otherwise " (:CODE "false") ".")
             (:paragraph (:DEMO-CODE (:DEMO-SOURCE #A((33) BASE-CHAR . "[sqrt(2), sin(1), 1/(1+sqrt(3))];") #A((39) BASE-CHAR . "[sqrt(2), sin(1), 1/(1+sqrt(3))],numer;")) (:EXAMPLE-INFO "@group" "(%i1) [sqrt(2), sin(1), 1/(1+sqrt(3))];" "                                        1" "(%o1)            [sqrt(2), sin(1), -----------]" "                                   sqrt(3) + 1" "@end group" "@group" "(%i2) [sqrt(2), sin(1), 1/(1+sqrt(3))],numer;" "(%o2) [1.414213562373095, 0.8414709848078965, 0.3660254037844387]" "@end group") (:DEMO-RESULT ((MAXIMA::MLIST MAXIMA::SIMP) ((MAXIMA::MEXPT MAXIMA::SIMP) 2 ((MAXIMA::RAT MAXIMA::SIMP) 1 2)) ((MAXIMA::%SIN MAXIMA::SIMP) 1) ((MAXIMA::MEXPT MAXIMA::SIMP) ((MAXIMA::MPLUS MAXIMA::SIMP) 1 ((MAXIMA::MEXPT MAXIMA::SIMP) 3 ((MAXIMA::RAT MAXIMA::SIMP) 1 2))) -1)) ((MAXIMA::MLIST MAXIMA::SIMP) 1.4142135623730951d0 0.8414709848078965d0 0.36602540378443865d0))))
             (:paragraph "After example code")
             (:CATBOX "Numerical evaluation" "Predicate functions"))
            (:p "test")))))

(define-documentation-frame-command (add-code-command :name "code")
    ()
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel)
          '((:pre "some" "test" "line")))))

(defun find-interaction-pane ()
  (clim:find-pane-named clim:*application-frame* 'interaction-pane))

(define-documentation-frame-command (open-help-node :name "Node")
    ((name '(or string maxima-client.markup:node-reference) :prompt "Node"))
  (let* ((node-name (etypecase name
                               (string name)
                               (maxima-client.markup:node-reference (maxima-client.markup:node-reference/name name))))
         (info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content))
         (content (load-node node-name)))
    (if content
        (setf (info-content-panel/content info-content-panel) content)
        (format (find-interaction-pane) "Node not found: ~s" node-name))))

(clim:define-command (open-help-function :name "Function" :menu t :command-table info-commands)
    ((function '(or string maxima-client.markup:maxima-function-reference) :prompt "Name"))
  (let* ((name (etypecase function
                 (string function)
                 (maxima-client.markup:maxima-function-reference (maxima-client.markup:maxima-function-reference/name function))))
         (info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content))
         (content (load-function name)))
    (cond (content
           (setf (info-content-panel/content info-content-panel) content)
           (clim:redisplay-frame-pane clim:*application-frame* info-content-panel))
          (t
           (format (find-interaction-pane) "No documentation for: ~s" name)))))

(clim:define-presentation-translator text-to-maxima-function-reference (string maxima-client.markup:maxima-function-reference info-commands)
    (object)
  (make-instance 'maxima-client.markup:maxima-function-reference :name object))

(clim:define-presentation-to-command-translator select-maxima-function
    (maxima-client.markup:maxima-function-reference open-help-function info-commands)
    (obj)
  (list obj))

(clim:define-presentation-to-command-translator select-node
    (maxima-client.markup:node-reference open-help-node info-commands)
    (obj)
  (list obj))
