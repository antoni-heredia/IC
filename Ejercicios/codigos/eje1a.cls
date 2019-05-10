(defrule bajoDemanda
    ?borrar <- (ContarHechos ?X)
    (hecho ?X $?a)
    (not(contado ?X $?))
    (not (CONTADOR ?X ?N))
    =>
    (assert(contado ?X $?a))
	(assert(CONTADOR ?X  1 )) 
	(retract ?borrar) 
	(printout t "Cantidad: " 1 crlf)
)
(defrule bajoDemanda2
    ?borrar <- (ContarHechos ?X)
    (hecho ?X $?a)
    ?borrar2 <- (contado ?X $?a)
    ?borrar3 <- (CONTADOR ?X ?N)
    =>
	(retract ?borrar)
    (retract ?borrar2) 
	(retract ?borrar3) 
	(assert(ContarHechos ?X))
)
(defrule bajoDemanda3
    ?borrar <- (ContarHechos ?X)
    (hecho ?X $?a)
    ?borrar2 <- (contado ?X $?a)
    =>
	(retract ?borrar)
    (retract ?borrar2) 
	(assert(ContarHechos ?X))
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