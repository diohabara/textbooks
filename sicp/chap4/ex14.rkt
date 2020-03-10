;; 4.14
; why does Louis's map fail even though Eva's works?
; Eva: type in the definition of map
; Loius: installed the system version of map as a primitive for metacircular
; ->
; if we install map as a primitive procedure,
; when using map, the procedure that that mapped and the argumets
; will be considered as an argument
; the map does not allow such a syntax
; so, it fails