(deftemplate Vector
	(multifield t)
)

(deffacts VariosVectores
(Vector
  (t  21 1 13 14 12)
)
)

(defrule minXiT
  (buscarMinimo)
  =>

  (bind ?primero TRUE)
  (bind ?minimo 0)
  (do-for-all-facts ((?FACT Vector)) TRUE   

    (foreach ?l ?FACT:t
        (if (eq ?primero TRUE)
        then
            (bind ?minimo ?l)
            (bind ?primero FALSE)
        else
            (if (< ?l  ?minimo) 
            then
                (bind ?minimo ?l))
            )
    )
  )
  (printout t "El minimo es: " ?minimo crlf)
)