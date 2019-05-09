(defrule bajoDemanda
    ?borrar <- (ContarHechos ?X)
    (hecho ?X $?a)
    (not(contado ?X $?a))
    (not (CONTADOR ?N))
    =>
    (assert(contado ?X $?a))
	(assert(CONTADOR ?X  1 )) 
	(retract ?borrar) 
	(printout t "Cantidad: " 1 crlf)
)

(defrule bajoDemandaContador
    ?borrar <- (CONTADOR  ?X  ?cant)
    (hecho ?X  $?a)
    (not(contado ?X $?a)) 
    =>
    (bind ?Total (+ ?cant 1)) 
	(retract ?borrar) 
    (assert(contado ?X $?a)) 
    (assert(CONTADOR ?X  ?Total) ) 
	(printout t "Cantidad: " ?Total crlf)
)