;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FUNCIONES UTILES PARA MANEJAR LA HORA EN CLIPS  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;se ofrecen para facilitar el proceso de añadir timestamp a eventos ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DEPENDERA DEL SISTEMA OPERATIVO (elegir comentando o descomentando a continuacion ;;;;;;;

;(defglobal ?*SO* = 1)              ;;; Windows (valor 1)    comentar si tu SO es linux o macos

(defglobal ?*SO* = 0)             ;;; Linux o MacOs (valor 0)    descoemntar si tu SO es linux o macos


;; Funcion que transforma ?h:?m:?s  en segundos transcurridos desde las 0h en punto ;;;

(deffunction totalsegundos (?h ?m ?s)
   (bind ?rv (+ (* 3600 ?h) (* ?m 60) ?s))
   ?rv)
   
;;;;;; Funcion que devuelve la salida de ejecutar  ?arg en linea de comandos del sistema ;;;
	  
   (deffunction system-string (?arg)
   (bind ?arg (str-cat ?arg " > temp.txt"))
   (system ?arg)
   (open "temp.txt" temp "r")
   (bind ?rv (readline temp))
   (close temp)
   ?rv)
   
;;;;;; Funcion que devuelve el nº de horas de la hora del sistema, si en el sistema son las ?h:?m:?s, devuelve ?h  ;;;;;;;;;;;;
   
   (deffunction horasistema ()
   (if (= ?*SO* 1) 
      then
         (bind ?rv (integer (string-to-field (sub-string 1 2  (system-string "time /t")))))
	   else 
	     (bind ?rv (string-to-field  (system-string "date +%H"))) 
         )    	 	  
   ?rv)
   
;;;;;; Funcion que devuelve el nº de minutos de la hora del sistema, si en el sistema son las ?h:?m:?s, devuelve ?m  ;;;;;;;;;;;; 
 
   (deffunction minutossistema ()
   (if (= ?*SO* 1) 
       then
          (bind ?rv (integer (string-to-field (sub-string 4 5  (system-string "time /t")))))
	   else 
	     (bind ?rv (string-to-field  (system-string "date +%M")))	  )
   ?rv)
   
;;;;;; Funcion que devuelve el nº de segundos de la hora del sistema, si en el sistema son las ?h:?m:?s, devuelve ?s  ;;;;;;;;;;;;
   
   (deffunction segundossistema ()
   (if (= ?*SO* 1) 
       then
          (bind ?rv (integer (string-to-field (sub-string 7 8  (system-string "@ECHO.%time:~0,8%")))))
	   else 
	     (bind ?rv (string-to-field  (system-string "date +%S")))	  )
   ?rv)
   
;;;;;; Funcion que devuelve el valor de ?h  al pasar ?t segundos al formato ?h:?m:?s  ;;;;;;;;;;
   
    (deffunction hora-segundos (?t)
   (bind ?rv  (div ?t 3600))
   ?rv)
   
;;;;;; Funcion que devuelve el valor de ?m  al pasar ?t segundos al formato ?h:?m:?s  ;;;;;;;;;;   
   (deffunction minuto-segundos (?t)
   (bind ?rv (- ?t (* (hora-segundos ?t) 3600)))
   (bind ?rv (div ?rv 60))
   ?rv)

;;;;;; Funcion que devuelve el valor de ?s  al pasar ?t segundos al formato ?h:?m:?s  ;;;;;;;;;;     
   (deffunction segundo-segundos (?t)
   (bind ?rv (- ?t (* (hora-segundos ?t) 3600)))
   (bind ?rv (- ?rv (* (minuto-segundos ?t) 60)))
   ?rv)

 

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



(defrule registrar_valor "registra un nuevo valor que lleva de los senores"
    (valor ?tipo ?habitacion ?v )
    =>
    (bind ?tmp (totalsegundos (horasistema) (minutossistema) (segundossistema)) ) ; guardamos en una variable la hora del sistema para que coincida en todos los asserts
    (printout t "Registrado " ?tipo " valor " ?v " habitacion " ?habitacion " tiempo " ?tmp crlf)
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
    (test (neq ?t ?tiemp ))
    (test (>= ?t ?tiemp ))
    (test (neq ?c ?tiemp  ))
    (test (>= ?c ?tiemp  ))
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
    (test (neq ?t ?tiemp ))
    (test (>= ?t ?tiemp ))
    (test (neq ?c ?tiemp  ))
    (test (>= ?c ?tiemp  ))
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
	?borrar <- (registroimpreso ?tiempo ?tipo ?h ?valor) 
	(valor_registrado ?t&~?tiempo ?tipo2 ?h ?valor2)
	(valor_registrado ?t2&:(and (>= ?t2 ?t) (< ?t2 ?tiempo)) ?tipo3 ?h ?valor3)
=>
	(retract ?borrar) 
	(assert (registroimpreso ?t2 ?tipo3 ?h ?valor3))
	(printout t "Tiempo: " ?t2  " Valor: " ?valor3 " Tipo: " ?tipo3 crlf) 
	)

(defrule obtenerInforme3
	?borrar <- (registroimpreso ?t1 ?tipo ?h ?valor) ;
=>
	(retract ?borrar)
)


(defrule obtenerInforme3
	?borrar <- (registroimpreso ?t1 ?tipo ?h ?valor) ;
=>
	(retract ?borrar)
)

