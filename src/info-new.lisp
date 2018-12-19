(in-package :maxima-client.doc-new)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type node ()
    :description "Node in the documentation"))

(defclass info-content-panel-view (maxima-client::maxima-renderer-view)
  ())

(defvar *doc-frame-lock* (bordeaux-threads:make-lock "doc-frame-lock"))
(defvar *doc-frame* nil)
(defvar *index-symbols* nil)
(defvar *index-nodes* nil)

(defparameter +info-content-panel-view+ (make-instance 'info-content-panel-view))

(defclass info-content-panel (clim:application-pane)
  ((content :initform nil
            :accessor info-content-panel/content)))

(clim:define-application-frame documentation-frame ()
  ()
  (:panes (info-content (clim:make-pane 'info-content-panel
                                        :display-function 'display-text-content
                                        :default-view +info-content-panel-view+))
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       (4/5 (clim:scrolling ()
                              info-content))
                       (1/5 interaction-pane)))))

(defun display-text-content (frame panel)
  (declare (ignore frame))
  (let ((content (info-content-panel/content panel)))
    (when content
      (clim:with-room-for-graphics (panel :first-quadrant nil)
        (maxima-client.markup:display-markup panel content)))))

(defun load-doc-file (name)
  (let* ((info-root-path (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/"))
         (file (merge-pathnames (concatenate 'string name ".lisp") info-root-path))
         (content (with-open-file (in file :external-format :utf-8)
                    (read in))))
    content))

(defun load-index ()
  (when (or (null *index-symbols*)
            (null *index-nodes*))
    (let* ((info-root-path (asdf:system-relative-pathname (asdf:find-system :maxima-client) #p"infoparser/docs/"))
           (file (merge-pathnames #p"index.lisp" info-root-path))
           (content (with-open-file (in file :external-format :utf-8)
                      (read in))))
      (setq *index-symbols* (cdr (assoc :symbols content)))
      (setq *index-nodes* (cdr (assoc :nodes content))))))

(defun load-node (name)
  (load-index)
  (let ((entry (find name *index-nodes* :test #'equal :key (lambda (v)
                                                             ;; Each entry consists of a node descriptor
                                                             ;; Each node descriptor consists of 4 elements
                                                             ;; The first element of each node descriptor is its name
                                                             (first (first v))))))
    (when entry
      (let* ((file (second entry))
             (file-content (load-doc-file file)))
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

(defun display-documentation-frame ()
  (let ((frame (clim:make-application-frame 'documentation-frame
                                            :width 900
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(defun open-documentation-frame ()
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (unless *doc-frame*
      (let ((frame (clim:make-application-frame 'documentation-frame
                                                :width 900
                                                :height 800)))
        (setq *doc-frame* frame)
        (bordeaux-threads:make-thread (lambda ()
                                        (clim:run-frame-top-level frame)))))))

(defmethod clim:frame-exit ((frame documentation-frame))
  (bordeaux-threads:with-lock-held (*doc-frame-lock*)
    (setq *doc-frame* nil))
  (call-next-method))

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

(define-documentation-frame-command (node-command :name "Node")
    ((name 'string :prompt "Node"))
  (let ((info-content-panel (clim:find-pane-named clim:*application-frame* 'info-content)))
    (setf (info-content-panel/content info-content-panel) (load-node name))))
