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
    (printout t "Paso aniadido " ?habitacion1 " a " ?habitacion2 crlf)
)
(defrule posible_sin_puerta
    (paso ?nombre ?habitacion1 ?habitacion2)
    (test (neq ?habitacion1 ?habitacion2 ))
    =>
    (assert (posible_pasar ?habitacion1 ?habitacion2))
    (assert (posible_pasar ?habitacion2 ?habitacion1))

    (printout t "Paso sin puerta aniadido " ?habitacion1 " a " ?habitacion2 crlf)
)

(defrule necesario_paso

    (posible_pasar ?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (assert (necesario_pasar ?habitacion1 ?habitacion2))
    (printout t "Necesario paso de " ?habitacion1 " a " ?habitacion2 crlf)
)

(defrule mas_de_un_paso1

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ?habitacion1 ~?habitacion2)
    (not(mas_de_un_paso ?habitacion1))
    =>
    (printout t "mas de un paso" ?habitacion1 crlf)
    (assert (mas_de_un_paso ?habitacion1))
)

(defrule mas_de_un_paso2

    (posible_pasar ?habitacion1 ?habitacion2)
    (posible_pasar ~?habitacion1 ?habitacion2)
    (not(mas_de_un_paso ?habitacion2))
    =>
    (printout t "mas de un paso" ?habitacion2 crlf)
    (assert (mas_de_un_paso ?habitacion2))
)

(defrule borrar_pasos_necesarios

    (mas_de_un_paso ?habitacion)
    ?Borrar <- (necesario_pasar ?b ?habitacion)
    =>
    (printout t "Borrar " ?b " a " ?habitacion crlf)
    (retract ?Borrar)
)

(defrule habitacion_interior

    (habitacion ?nombre)
    (not(ventanas ?nombreVentana ?nombre))
     =>
    (printout t "habitacion interior " ?nombre crlf)
    (assert (habitacion_interior ?nombre))
)