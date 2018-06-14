(in-package :maxima-client)

(clim:define-presentation-type maxima-matrix ()
  :inherit-from t)

(clim:define-presentation-method clim:accept ((type maxima-matrix) stream (view clim:textual-view) &key)
  )
