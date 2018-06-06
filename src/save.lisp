(in-package :maxima-client)

(defun collect-maxima-current-state ()
  (loop
    for sym being each symbol in "MAXIMA"
    when (alexandria:starts-with-subseq "$" (symbol-name sym))
      collect sym))
