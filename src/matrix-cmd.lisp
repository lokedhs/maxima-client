(in-package :maxima-client)

(clim:define-command (characteristic-polynomial-command :name "Characteristic polynomial" :menu t :command-table maxima-commands)
    ((matrix maxima-native-expr :prompt "Matrix")
     (variable maxima-native-expr :prompt "Variable"))
  "Compute the characteristic polynomial for a matrix"
  (let ((m (maxima-native-expr/expr matrix))
        (v (maxima-native-expr/expr variable)))
    (maxima-eval-lisp-expr `((maxima::$ev) ((maxima::$charpoly)
                                            ,m ,v)
                             maxima::$expand))))

(clim:define-command (generate-matrix-command :name "Generate matrix" :menu t :command-table maxima-commands)
    ((value maxima-native-expr :prompt "Expression (i,j)")
     (width maxima-native-expr :prompt "Width")
     (height maxima-native-expr :prompt "Height"))
  "Generates a matrix of the given dimensions"
  (let ((value-expr (maxima-native-expr/expr value))
        (w (maxima-native-expr/expr width))
        (h (maxima-native-expr/expr height)))
    (maxima-eval-lisp-expr `((maxima::$genmatrix) ,value-expr ,w ,h))))

(clim:make-command-table 'maxima-matrix-command-table
                         :errorp nil
                         :menu '(("Characteristic polynomial" :command characteristic-polynomial-command)
                                 ("Generate matrix" :command generate-matrix-command)))
