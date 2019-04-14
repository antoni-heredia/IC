
(deffacts habitaciones
(habitacion cocina)
(habitacion dormitorio1)
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
(deffacts paso_sin_puerta
(
    paso salon_comedor comedor salon )
)
(deffacts puertas_interiores
(puerta p_cocina cocina)
(puerta p_dormitorio1 dormitorio1)
(puerta p_dormitorio2 dormitorio2)
(puerta p_dormitorio3 dormitorio3) 
(puerta p_dormitorio4 dormitorio4)
(puerta p_dormitorio5 dormitorio5)
(puerta p_banio banio)
(puerta p_banioPrincipal banioPrincipal)
(puerta p_comedor comedor)
(puerta p_cocina pasillo)
(puerta p_dormitorio1 pasillo)
(puerta p_dormitorio2 pasillo)
(puerta p_dormitorio3 pasillo)
(puerta p_dormitorio4 pasillo)
(puerta p_dormitorio5 pasillo)
(puerta p_banio pasillo)
(puerta p_banioPrincipal pasillo)
(puerta p_comedor pasillo)
)

(deffacts puerta_salida
(puerta p_s_pasillo pasillo)
)



(deffacts ventanas
(ventanas v_cocina cocina)
(ventanas v_dormitorio1 dormitorio1)
(ventanas v_dormitorio2 dormitorio2)
(ventanas v_dormitorio3 dormitorio3) 
(ventanas v_dormitorio4 dormitorio4)
(ventanas v_dormitorio5 dormitorio5)
(ventanas v_banio banio)
(ventanas v_salon salon)
(ventanas v_comedor comedor)
(ventanas v_pasillo pasillo)
)

(defrule posible_pasar
    (puerta ?nombre_puerta ?habitacion1)
    (puerta ?nombre_puerta ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))

    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
)
(defrule posible_sin_puerta
    (paso ?nombre ?habitacion1 ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
    (assert (posible_pasar ?habitacion2 ?habitacion1))

)

(defrule necesario_paso

    (posible_pasar ?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (necesario_pasar ?habitacion1 ?habitacion2))
)

(defrule mas_de_un_paso1

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ?habitacion1 ~?habitacion2)
    (not(mas_de_un_paso ?habitacion1))
    =>
    (assert (mas_de_un_paso ?habitacion1))
)

(defrule mas_de_un_paso2

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ~?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (mas_de_un_paso ?habitacion2))
)

(defrule borrar_pasos_necesarios

    (mas_de_un_paso ?habitacion)
    ?Borrar <- (necesario_pasar ?b ?habitacion)
    =>
    (retract ?Borrar)
)

(defrule habitacion_interior

    (habitacion ?nombre)
    (not(ventanas ?nombreVentana ?nombre))
     =>
    (assert (habitacion_interior ?nombre))
)



(defrule registrar_valor "registra un nuevo valor que lleva de los senores"
    (valor ?tipo ?habitacion ?v )
    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)
    =>
    (bind ?tmp (totalsegundos  ?h ?m ?s ) ) ; guardamos en una variable la hora del sistema para que coincida en todos los asserts
    (assert(valor_registrado ?tmp ?tipo ?habitacion ?v))
)


(defrule registrar_ultimo "cuando llega un valor registrado, si ha llegado otro antes, lo borra y aniade uno nuevo a ultimoregistro"
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    ?ultimo <- (ultimo_registro ?tipo ?habitacion ?tiemp)
    (test (neq ?tmp ?tiemp ))
    (test (>= ?tmp ?tiemp ))
    =>
    (retract ?ultimo)
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))

)

(defrule registrar_ultimo2 "igual que antes pero siendo el priemro que llega y no teniendo nada que borrar"
    (valor_registrado ?tmp ?tipo ?habitacion ?v)
    (not(ultimo_registro ?tipo ?habitacion ?temp))
    =>
    (assert(ultimo_registro ?tipo ?habitacion ?tmp))
)


(defrule registrar_activacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v on ))
    (not(ultima_activacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_activacion movimiento ?habitacion ?t))
)



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

(defrule registrar_desactivacion 
    (valor_registrado ?t ?tipo ?habitacion ?v)
    (test (eq ?tipo movimiento ))
    (test (eq ?v off ))
    (not(ultima_desactivacion ?tipo ?habitacion ?tiempo))
    =>
    (assert (ultima_desactivacion movimiento ?habitacion ?t))
)

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

(defrule borrar_todos_valor 
    (ultimo_registro ?tipo ?habitacion ?tmp)
    ?borrar <- (valor ?tipo ?habitacion ?v )
    =>
    (retract ?borrar)
)

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

(defrule obtenerInforme2
	?Borrar <- (registroimpreso ?tiempo ?tipo ?h ?valor) 
	(valor_registrado ?t&~?tiempo ?tipo2 ?h ?valor2)
	(valor_registrado ?t2&:(and (>= ?t2 ?t) (< ?t2 ?tiempo)) ?tipo3 ?h ?valor3)
=>
	(retract ?Borrar) 
	(assert (registroimpreso ?t2 ?tipo3 ?h ?valor3))
	(printout t "Tiempo: " ?t2  " Valor: " ?valor3 " Tipo: " ?tipo3 crlf) 
	)

(defrule obtenerInforme3
	?Borrar <- (registroimpreso ?t1 ?tipo ?h ?valor) ;
=>
	(retract ?Borrar)
)

(defrule encenderLuzSiNoExiste
    (Manejo_inteligente_luces ?hab)
    (ultimo_registro movimiento ?hab ?tiempo)    
    (ultima_activacion movimiento ?hab ?t)
    (not(estadoluz ?hab on))
    (not(estadoluz ?hab off))

    (test(= ?tiempo ?t))
    =>
    (assert (accion pulsador_luz ?hab encender))
)

(defrule encenderLuzSiEstaoff
    (Manejo_inteligente_luces ?hab)
    (ultimo_registro movimiento ?hab ?tiempo)    
    (ultima_activacion movimiento ?hab ?t)
    (not(estadoluz ?hab on))
    (estadoluz ?hab off)
    (test(= ?tiempo ?t))
    =>
    (assert (accion pulsador_luz ?hab encender))
)

(defrule pareceVacia
    (Manejo_inteligente_luces ?hab)
    (ultimo_registro movimiento ?hab ?tiempo)    
    (ultima_desactivacion movimiento ?hab ?t)
    (estadoluz ?hab on)
    (test(= ?tiempo ?t))
    (not (pareceVacia ?hab ?cualquiertiempo))
    =>
    (assert (pareceVacia ?hab ?t))
)

(defrule comprobacionTiempoVacia
    (Manejo_inteligente_luces ?hab)
    ?borrar <- (pareceVacia ?hab ?tmp)
    (ultimo_registro movimiento ?hab ?tiempo)    
    (valor_registrado ?tiempo movimiento ?hab off)
    (test(>  (- ?tiempo ?tmp ) 20))
    =>
    (assert (accion pulsador_luz ?hab apagar))
    (retract ?borrar)

)
(defrule pareceVaciaPeroLlegaOn
    (Manejo_inteligente_luces ?hab)
    ?borrar <- (pareceVacia ?hab ?tmp)
    (valor_registrado ?t movimiento ?hab on)
    (test(>  (- ?t ?tmp ) 0))
    (test(<  (- ?t ?tmp ) 20))
    =>
    (retract ?borrar)
)



(defrule llegaEncender
    (accion pulsador_luz ?hab encender)
    ?borrarAccion <- (accion pulsador_luz ?hab encender)
    (not(estadoluz ?hab on))
    ?borrar <- (estadoluz ?hab off)
    =>
    (retract ?borrar)    
    (retract ?borrarAccion)
    (assert(estadoluz ?hab on))
)
(defrule llegaEncender1
    (accion pulsador_luz ?hab encender)
    ?borrarAccion <- (accion pulsador_luz ?hab encender)
    (not(estadoluz ?hab on))
    (not(estadoluz ?hab off))
    =>
    (retract ?borrarAccion)
    (assert(estadoluz ?hab on))
)



(defrule llegarApagar
    (accion pulsador_luz ?hab apagar)
    ?borrarAccion <- (accion pulsador_luz ?hab apagar)
    (not(estadoluz ?hab off))
    (estadoluz ?hab on)
    ?borrar <- (estadoluz ?hab on)
    =>
    (retract ?borrar)    
    (retract ?borrarAccion)
    (assert(estadoluz ?hab off))
)
(defrule llegarApagar1
    (accion pulsador_luz ?hab apagar)
    ?borrarAccion <- (accion pulsador_luz ?hab apagar)
    (not(estadoluz ?hab on))
    (not(estadoluz ?hab off))
    =>
    (retract ?borrarAccion)
    (assert(estadoluz ?hab off))
)
(defrule llegaCambiar
    (accion pulsador_luz ?hab cambiar)
    ?borrarAccion <- (accion pulsador_luz ?hab cambiar)
    ?borrar<-(estadoluz ?hab off)
    =>
    (retract ?borrarAccion)
    (retract ?borrar)
    (assert(estadoluz ?hab on))
)
(defrule llegaCambiar1
    (accion pulsador_luz ?hab cambiar)
    ?borrarAccion <- (accion pulsador_luz ?hab cambiar)
    ?borrar<-(estadoluz ?hab on)
    =>
    (retract ?borrarAccion)
    (retract ?borrar)
    (assert(estadoluz ?hab off))
)

