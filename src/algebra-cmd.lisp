(in-package :maxima-client)

(clim:define-command (solve-command :name "Solve" :menu t :command-table maxima-commands)
    ((equations maxima-native-expr :prompt "Equations")
     (variable maxima-native-expr :prompt "Variable"))
  "Solve a set of equations for a given variable"
  (let ((equation-expr (maxima-native-expr/expr equations))
        (var-expr (maxima-native-expr/expr variable)))
    (maxima-eval-lisp-expr `((maxima::$solve) ,equation-expr ,var-expr))))

(clim:make-command-table 'maxima-algebra-command-table
                         :errorp nil
                         :menu '(("Solve" :command solve-command)))
