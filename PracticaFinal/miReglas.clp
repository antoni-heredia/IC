
;declaro las habitaciones que tengo
(deffacts habitaciones
(habitacion cocina)
(habitacion despacho)
(habitacion dormitorio2)
(habitacion dormitorio3) 
(habitacion dormitorio4)
(habitacion dormitorio5)
(habitacion banio)
(habitacion banioPrincipal)
(habitacion salon)
(habitacion comedor)
(habitacion pasillo)
)
;Declaro las luminosidad minima de cada habitacion
(deffacts luminosidades
(lumMinimos  cocina 200)
(lumMinimos  despacho 500)
(lumMinimos  dormitorio2 300)
(lumMinimos  dormitorio3 300) 
(lumMinimos  dormitorio4 300)
(lumMinimos  dormitorio5 300)
(lumMinimos  banio 200)
(lumMinimos  banioPrincipal 200)
(lumMinimos  salon 300 )
(lumMinimos  comedor 300)
(lumMinimos  pasillo 200)
)
;los pasos de una habitaicon a otra en los que no hay puerta
(deffacts paso_sin_puerta
(
    paso salon_comedor comedor salon )
)

;aqui defino que puertas hay en cada habitacion, cada puerta tiene un nombre
; y si dos habitaciones tienen la misma puerta es que hay un paso entre ellas(se usara una regla para deducirlo)
(deffacts puertas_interiores
(puerta p_cocina cocina)
(puerta p_despacho despacho)
(puerta p_dormitorio2 dormitorio2)
(puerta p_dormitorio3 dormitorio3) 
(puerta p_dormitorio4 dormitorio4)
(puerta p_dormitorio5 dormitorio5)
(puerta p_banio banio)
(puerta p_banioPrincipal banioPrincipal)
(puerta p_comedor comedor)
(puerta p_cocina pasillo)
(puerta p_despacho pasillo)
(puerta p_dormitorio2 pasillo)
(puerta p_dormitorio3 pasillo)
(puerta p_dormitorio4 pasillo)
(puerta p_dormitorio5 pasillo)
(puerta p_banio pasillo)
(puerta p_banioPrincipal pasillo)
(puerta p_comedor pasillo)
)

;las puertas de salida de la casa
(deffacts puerta_salida
(puerta p_s_pasillo pasillo)
)


;las ventanas que tienen la casa
(deffacts ventanas
(ventanas v_cocina cocina)
(ventanas v_despacho despacho)
(ventanas v_dormitorio2 dormitorio2)
(ventanas v_dormitorio3 dormitorio3) 
(ventanas v_dormitorio4 dormitorio4)
(ventanas v_dormitorio5 dormitorio5)
(ventanas v_banio banio)
(ventanas v_salon salon)
(ventanas v_comedor comedor)
(ventanas v_pasillo pasillo)
)

;esta regla indica si se pude pasar de una habitacion a otra cuando hay una puerta
; se añade tanto el paso de "C" a "B" como de "B" a "C", porque lo encontrara dos veces
(defrule posible_pasar
    (puerta ?nombre_puerta ?habitacion1)
    (puerta ?nombre_puerta ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
)

;esta regla es la misma que la anterior pero para cuando hay un paso sin puerta
(defrule posible_sin_puerta
    (paso ?nombre ?habitacion1 ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
    (assert (posible_pasar ?habitacion2 ?habitacion1))

)

;cuando una habitacion solo tiene una puerta de entrada a ella
(defrule necesario_paso

    (posible_pasar ?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (necesario_pasar ?habitacion1 ?habitacion2))
)

;cuando una habitacion tiene mas de una puerta de entrada
(defrule mas_de_un_paso1

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ?habitacion1 ~?habitacion2)
    (not(mas_de_un_paso ?habitacion1))
    =>
    (assert (mas_de_un_paso ?habitacion1))
)
;tiene la misma funcion que la anterior, pero teniendo en cuenta que puede ser la segunda habitacion la que tenga mas de un paso
(defrule mas_de_un_paso2

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ~?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (mas_de_un_paso ?habitacion2))
)
;Puede dar el caso en que se ejecute la regla "necesario_paso" antes que "mas_de_un_paso"
; entonces borra de necesario_paso las que tenga mas de un paso
(defrule borrar_pasos_necesarios

    (mas_de_un_paso ?habitacion)
    ?Borrar <- (necesario_pasar ?b ?habitacion)
    =>
    (retract ?Borrar)
)

;Una habitacion es interior cuando no tiene ventanas
(defrule habitacion_interior

    (habitacion ?nombre)
    (not(ventanas ?nombreVentana ?nombre))
     =>
    (assert (habitacion_interior ?nombre))
)

;Cuando llega un valor nuevo de los sensores, guardamos la hora actual que nos da el simulador
(defrule registrar_valor "registra un nuevo valor que lleva de los senores"
    (valor ?tipo ?habitacion ?v )
    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)
    =>
    (bind ?tmp (totalsegundos  ?h ?m ?s ) ) ; guardamos en una variable la hora del sistema para que coincida en todos los asserts
    (assert(valor_registrado ?tmp ?tipo ?habitacion ?v))
)

;cambiamos el ultimo valor de un tipo por el que acaba de llegar
(defrule registrar_ultimo "cuando llega un valor registrado, si ha llegado otro antes, lo borra y aniade uno nuevo a ultimoregistro"
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    ?ultimo <- (ultimo_registro ?tipo ?habitacion ?tiemp)
    (test (neq ?tmp ?tiemp ))
    (test (>= ?tmp ?tiemp ))
    =>
    (retract ?ultimo)
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))

)

;"igual que antes pero siendo el priemro que llega y no teniendo nada que borrar"
(defrule registrar_ultimo2 
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    (not(ultimo_registro ?tipo ?habitacion ?temp))
    =>
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))
)

;cuando llega un ON del sensor de movimiento y es la primera vez que ocurre
(defrule registrar_activacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v on ))
    (not(ultima_activacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_activacion movimiento ?habitacion ?t))
)


;cuando llega un ON del sensor de movimiento y NO es la primera vez que ocurre
(defrule registrar_activacion2
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v on ))
    (ultima_desactivacion ?a ?b ?c)
    ?ultimo <- (ultima_activacion ?tipo ?habitacion ?tiemp)
    (test (neq ?t ?c ))
    (test (> ?t ?c ))
    (test (neq ?c ?tiemp  ))
    (test (> ?c ?tiemp  ))
    =>
    (retract ?ultimo)
    (assert(ultima_activacion movimiento ?habitacion ?t))
)

;cuando llega un OFF del sensor de movimiento y es la primera vez que ocurre
(defrule registrar_desactivacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v off ))
    (not(ultima_desactivacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_desactivacion movimiento ?habitacion ?t))
)

;cuando llega un OFF del sensor de movimiento y NO es la primera vez que ocurre
(defrule registrar_desactivacion2
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v off ))
    (ultima_activacion ?a ?b ?c)
    ?ultimo <- (ultima_desactivacion ?tipo ?habitacion ?tiemp)
    (test (neq ?t ?c ))
    (test (> ?t ?c ))
    (test (neq ?c ?tiemp  ))
    (test (> ?c ?tiemp  ))
    =>
    (retract ?ultimo)
    (assert(ultima_desactivacion movimiento ?habitacion ?t))
)

;cuando ya se guarda la informacion de un "valor" este se puede borrar
(defrule borrar_todos_valor 
    (ultimo_registro ?tipo ?habitacion ?tmp)
    ?borrar <- (valor ?tipo ?habitacion ?v )
    =>
    (retract ?borrar)
)

;regla para obtener el informe de una habitacion, mostrando el primero con mayor tiempo
(defrule obtenerInforme
    ?borrar <- (informe ?h)
    (valor_registrado ?tiempo ? ?h ?) 
	(valor_registrado ?t&:(>= ?t ?tiempo) ?tipo ?h ?v) 
    =>
	(assert(registroimpreso ?t ?tipo ?h ?v) ) 
	(retract ?borrar) 
	(printout t "--------Informe habitacion " ?h "--------: " crlf) 
	(printout t "Tiempo: " ?t   " Valor: " ?v" Tipo: " ?tipo crlf)
)
;Con esta regla iremos guardando el ultimo con mayor tiempo que todavia no se ha mostrado
(defrule obtenerInforme2
	?Borrar <- (registroimpreso ?tiempo ?tipo ?h ?valor) 
	(valor_registrado ?t&~?tiempo ?tipo2 ?h ?valor2)
	(valor_registrado ?t2&:(and (>= ?t2 ?t) (< ?t2 ?tiempo)) ?tipo3 ?h ?valor3)
=>
	(retract ?Borrar) 
	(assert (registroimpreso ?t2 ?tipo3 ?h ?valor3))
	(printout t "Tiempo: " ?t2  " Valor: " ?valor3 " Tipo: " ?tipo3 crlf) 
	)
;borrar el ultimo registro impreso por pantalla que tengamos
(defrule obtenerInforme3
	?Borrar <- (registroimpreso ?t1 ?tipo ?h ?valor) ;
=>
	(retract ?Borrar)
)



(defrule personaSale
    (valor_registrado ?t ?tipo ?puerta ?v)
    (test (eq ?tipo magnetico ))

;declaro las habitaciones que tengo
(deffacts habitaciones
(habitacion cocina)
(habitacion despacho)
(habitacion dormitorio2)
(habitacion dormitorio3) 
(habitacion dormitorio4)
(habitacion dormitorio5)
(habitacion banio)
(habitacion banioPrincipal)
(habitacion salon)
(habitacion comedor)
(habitacion pasillo)
)
;Declaro las luminosidad minima de cada habitacion
(deffacts luminosidades
(lumMinimos  cocina 200)
(lumMinimos  despacho 500)
(lumMinimos  dormitorio2 300)
(lumMinimos  dormitorio3 300) 
(lumMinimos  dormitorio4 300)
(lumMinimos  dormitorio5 300)
(lumMinimos  banio 200)
(lumMinimos  banioPrincipal 200)
(lumMinimos  salon 300 )
(lumMinimos  comedor 300)
(lumMinimos  pasillo 200)
)
;los pasos de una habitaicon a otra en los que no hay puerta
(deffacts paso_sin_puerta
(
    paso salon_comedor comedor salon )
)

;aqui defino que puertas hay en cada habitacion, cada puerta tiene un nombre
; y si dos habitaciones tienen la misma puerta es que hay un paso entre ellas(se usara una regla para deducirlo)
(deffacts puertas_interiores
(puerta p_cocina cocina)
(puerta p_despacho despacho)
(puerta p_dormitorio2 dormitorio2)
(puerta p_dormitorio3 dormitorio3) 
(puerta p_dormitorio4 dormitorio4)
(puerta p_dormitorio5 dormitorio5)
(puerta p_banio banio)
(puerta p_banioPrincipal banioPrincipal)
(puerta p_comedor comedor)
(puerta p_cocina pasillo)
(puerta p_despacho pasillo)
(puerta p_dormitorio2 pasillo)
(puerta p_dormitorio3 pasillo)
(puerta p_dormitorio4 pasillo)
(puerta p_dormitorio5 pasillo)
(puerta p_banio pasillo)
(puerta p_banioPrincipal pasillo)
(puerta p_comedor pasillo)
)

;las puertas de salida de la casa
(deffacts puerta_salida
(puerta p_s_pasillo pasillo)
)


;las ventanas que tienen la casa
(deffacts ventanas
(ventanas v_cocina cocina)
(ventanas v_despacho despacho)
(ventanas v_dormitorio2 dormitorio2)
(ventanas v_dormitorio3 dormitorio3) 
(ventanas v_dormitorio4 dormitorio4)
(ventanas v_dormitorio5 dormitorio5)
(ventanas v_banio banio)
(ventanas v_salon salon)
(ventanas v_comedor comedor)
(ventanas v_pasillo pasillo)
)

;esta regla indica si se pude pasar de una habitacion a otra cuando hay una puerta
; se añade tanto el paso de "C" a "B" como de "B" a "C", porque lo encontrara dos veces
(defrule posible_pasar
    (puerta ?nombre_puerta ?habitacion1)
    (puerta ?nombre_puerta ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
)

;esta regla es la misma que la anterior pero para cuando hay un paso sin puerta
(defrule posible_sin_puerta
    (paso ?nombre ?habitacion1 ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
    (assert (posible_pasar ?habitacion2 ?habitacion1))

)

;cuando una habitacion solo tiene una puerta de entrada a ella
(defrule necesario_paso

    (posible_pasar ?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (necesario_pasar ?habitacion1 ?habitacion2))
)

;cuando una habitacion tiene mas de una puerta de entrada
(defrule mas_de_un_paso1

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ?habitacion1 ~?habitacion2)
    (not(mas_de_un_paso ?habitacion1))
    =>
    (assert (mas_de_un_paso ?habitacion1))
)
;tiene la misma funcion que la anterior, pero teniendo en cuenta que puede ser la segunda habitacion la que tenga mas de un paso
(defrule mas_de_un_paso2

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ~?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (mas_de_un_paso ?habitacion2))
)
;Puede dar el caso en que se ejecute la regla "necesario_paso" antes que "mas_de_un_paso"
; entonces borra de necesario_paso las que tenga mas de un paso
(defrule borrar_pasos_necesarios

    (mas_de_un_paso ?habitacion)
    ?Borrar <- (necesario_pasar ?b ?habitacion)
    =>
    (retract ?Borrar)
)

;Una habitacion es interior cuando no tiene ventanas
(defrule habitacion_interior

    (habitacion ?nombre)
    (not(ventanas ?nombreVentana ?nombre))
     =>
    (assert (habitacion_interior ?nombre))
)

;Cuando llega un valor nuevo de los sensores, guardamos la hora actual que nos da el simulador
(defrule registrar_valor "registra un nuevo valor que lleva de los senores"
    (valor ?tipo ?habitacion ?v )
    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)
    =>
    (bind ?tmp (totalsegundos  ?h ?m ?s ) ) ; guardamos en una variable la hora del sistema para que coincida en todos los asserts
    (assert(valor_registrado ?tmp ?tipo ?habitacion ?v))
)

;cambiamos el ultimo valor de un tipo por el que acaba de llegar
(defrule registrar_ultimo "cuando llega un valor registrado, si ha llegado otro antes, lo borra y aniade uno nuevo a ultimoregistro"
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    ?ultimo <- (ultimo_registro ?tipo ?habitacion ?tiemp)
    (test (neq ?tmp ?tiemp ))
    (test (>= ?tmp ?tiemp ))
    =>
    (retract ?ultimo)
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))

)

;"igual que antes pero siendo el priemro que llega y no teniendo nada que borrar"
(defrule registrar_ultimo2 
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    (not(ultimo_registro ?tipo ?habitacion ?temp))
    =>
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))
)

;cuando llega un ON del sensor de movimiento y es la primera vez que ocurre
(defrule registrar_activacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v on ))
    (not(ultima_activacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_activacion movimiento ?habitacion ?t))
)


;cuando llega un ON del sensor de movimiento y NO es la primera vez que ocurre
(defrule registrar_activacion2
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v on ))
    (ultima_desactivacion ?a ?b ?c)
    ?ultimo <- (ultima_activacion ?tipo ?habitacion ?tiemp)
    (test (neq ?t ?c ))
    (test (> ?t ?c ))
    (test (neq ?c ?tiemp  ))
    (test (> ?c ?tiemp  ))
    =>
    (retract ?ultimo)
    (assert(ultima_activacion movimiento ?habitacion ?t))
)

;cuando llega un OFF del sensor de movimiento y es la primera vez que ocurre
(defrule registrar_desactivacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v off ))
    (not(ultima_desactivacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_desactivacion movimiento ?habitacion ?t))
)

;cuando llega un OFF del sensor de movimiento y NO es la primera vez que ocurre
(defrule registrar_desactivacion2
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v off ))
    (ultima_activacion ?a ?b ?c)
    ?ultimo <- (ultima_desactivacion ?tipo ?habitacion ?tiemp)
    (test (neq ?t ?c ))
    (test (> ?t ?c ))
    (test (neq ?c ?tiemp  ))
    (test (> ?c ?tiemp  ))
    =>
    (retract ?ultimo)
    (assert(ultima_desactivacion movimiento ?habitacion ?t))
)

;cuando ya se guarda la informacion de un "valor" este se puede borrar
(defrule borrar_todos_valor 
    (ultimo_registro ?tipo ?habitacion ?tmp)
    ?borrar <- (valor ?tipo ?habitacion ?v )
    =>
    (retract ?borrar)
)

;regla para obtener el informe de una habitacion, mostrando el primero con mayor tiempo
(defrule obtenerInforme
    ?borrar <- (informe ?h)
    (valor_registrado ?tiempo ? ?h ?) 
	(valor_registrado ?t&:(>= ?t ?tiempo) ?tipo ?h ?v) 
    =>
	(assert(registroimpreso ?t ?tipo ?h ?v) ) 
	(retract ?borrar) 
	(printout t "--------Informe habitacion " ?h "--------: " crlf) 
	(printout t "Tiempo: " ?t   " Valor: " ?v" Tipo: " ?tipo crlf)
)
;Con esta regla iremos guardando el ultimo con mayor tiempo que todavia no se ha mostrado
(defrule obtenerInforme2
	?Borrar <- (registroimpreso ?tiempo ?tipo ?h ?valor) 
	(valor_registrado ?t&~?tiempo ?tipo2 ?h ?valor2)
	(valor_registrado ?t2&:(and (>= ?t2 ?t) (< ?t2 ?tiempo)) ?tipo3 ?h ?valor3)
=>
	(retract ?Borrar) 
	(assert (registroimpreso ?t2 ?tipo3 ?h ?valor3))
	(printout t "Tiempo: " ?t2  " Valor: " ?valor3 " Tipo: " ?tipo3 crlf) 
	)
;borrar el ultimo registro impreso por pantalla que tengamos
(defrule obtenerInforme3
	?Borrar <- (registroimpreso ?t1 ?tipo ?h ?valor) ;
=>
	(retract ?Borrar)
)



(defrule personaSale
    (valor_registrado ?t ?tipo ?puerta ?v)
    (valor_registrado ?t1 ?tipo1 ?hab1 ?v1)

    (test (eq ?tipo1 movimiento ))
    (test (eq ?tipo magnetico ))

    (test (eq ?puerta p_s_pasillo ))
    (test (eq ?hab1 pasillo ))

    (test (eq ?v on ))
    (test (eq ?v1 on ))
    =>
    (printout t "La persona ha salido de la casa: " ?t crlf) 

)