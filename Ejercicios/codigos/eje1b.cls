(defrule bajoDemanda
    (hecho ?X $?a)
    (not(contado ?X $?a))
    (not (CONTADOR ?X ?N))
    =>
    (assert(contado ?X $?a))
	(assert(CONTADOR ?X  1 )) 
	(printout t "Cantidad: " 1 crlf)
)
(defrule bajoDemanda2
    (hecho ?X $?a)
    (not(contado ?X $?a))
    ?borrar <- (CONTADOR ?X ?cant)
    =>
    (bind ?Total (+ ?cant 1)) 
	(retract ?borrar) 
    (assert(contado ?X $?a))
	(assert(CONTADOR ?X  ?Total )) 
	(printout t "Cantidad: " ?Total crlf)
)