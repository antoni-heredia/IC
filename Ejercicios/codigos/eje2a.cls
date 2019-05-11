
;Insertar buscarMinimo para ejecutar

(deftemplate t (slot S (type NUMBER)))

(deffacts varios
  (t (S 21))
  (t (S 2))
  (t (S 35))
)
(defrule minSdeT
  (buscarMinimo)
  =>
  (bind ?primero TRUE)
  (bind ?minimo 0)
  (do-for-all-facts ((?FACT t)) TRUE
    (if (eq ?primero TRUE)
   then
	(bind ?minimo ?FACT:S)
	(bind ?primero FALSE)
    else
	
	(if (< ?FACT:S ?minimo) 
    	then
        	(bind ?minimo ?FACT:S))
    )
  )
  (printout t "El minimo es: " ?minimo crlf)
)


