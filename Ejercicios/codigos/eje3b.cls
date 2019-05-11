;Insertar leeVector para ejecutar


(deftemplate Vector
	(multifield t)
)

(defrule leeVector
  (leeVector)
  =>
  (open "DatosT.txt" mydata)
  (assert (SeguirLeyendo))
)
(defrule LeerValoresCierreFromFile
  ?f <- (SeguirLeyendo)
  =>
  (bind ?Leido (readline mydata))
  (retract ?f)
  (if (neq ?Leido EOF) then
    (assert (SeguirLeyendo))
    (bind ?insertar  (explode$ ?Leido))
    (assert (Vector 
            (t ?insertar)
            ))
  else
      (assert (Cerramos))

  ))


(defrule closefile
  ?f <- (Cerramos)
  =>
  (retract ?f)

  (close mydata)
 )

