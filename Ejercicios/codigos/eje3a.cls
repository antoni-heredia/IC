;Insertar guardarVector para ejecutar

(deftemplate Vector
	(multifield t)
)

(deffacts varios
  (Vector
    (t  21 1 13 14 12)
  )
)

(defrule guardarVector
  (guardarVector)
  =>
  (open "DatosT.txt" mydata "w")

  (do-for-all-facts ((?FACT Vector)) TRUE   

    (foreach ?l ?FACT:t
       (printout mydata ?l)
    )
    (printout mydata crlf)

  )
  (close mydata)
)

