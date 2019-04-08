(defrule cargardatosasimular
=>
(load-facts DatosSimulados.txt) 
)



(defrule datosensor
(declare (salience 9998))
?f <-  (datosensor  ?h ?m ?s  ?tipo ?habitacion ?val)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (valor ?tipo ?habitacion ?val))
(printout t  ?h1 ":" ?m1 ":" ?s1 ": sensor " ?tipo " " ?habitacion " " ?val crlf)
(retract ?f)
)

(defrule datoluminosidad
(declare (salience 10000))
(datosensor  ?h ?m ?s  movimiento ?habitacion on)
(luminosidad ?habitacion ?l)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (valor luminosidad ?habitacion ?l))
(printout t  ?h1 ":" ?m1 ":" ?s1 ": Luminosidad " ?habitacion " " ?l crlf)
)

(defrule pulsadorluzon
(declare (salience 10000))
(datosensor  ?h ?m ?s  pulsador ?habitacion on)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (accion pulsador_luz ?habitacion encender))
)

(defrule pulsadorluzoff
(declare (salience 10000))
(datosensor  ?h ?m ?s  pulsador ?habitacion off)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (accion pulsador_luz ?habitacion apagar))
)


(defrule datosensor_movimiento_consistencia
(declare (salience 10000))
(datosensor  ?h ?m ?s  movimiento ?habitacion on)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (siguientedatosensor  (hora_suma ?h ?m ?s 60) (minuto_suma ?h ?m ?s 60) (segundo_suma ?h ?m ?s 60) movimiento ?habitacion  on ))
;(printout t "creado siguientedatosensormovimiento "  ?habitacion " para las " (hora_suma ?h ?m ?s 60)   ":" (minuto_suma ?h ?m ?s 60) ":" (segundo_suma ?h ?m ?s 60) crlf)
)


(defrule generar_datosensor_movimiento
(declare (salience 9999))
?f <- (siguientedatosensor  ?h ?m ?s movimiento ?habitacion  on )
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
=>
(assert (datosensor  ?h ?m ?s movimiento ?habitacion  on ))
(retract ?f)
)

(defrule limpiar_siguientedatosensor_movimiento
(declare (salience 10000))
?f <- (siguientedatosensor  ?h ?m ?s movimiento ?habitacion  on )
(datosensor  ?h1 ?m1 ?s1 movimiento ?habitacion  ?)
(test (<= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
(test (<= (totalsegundos ?h ?m ?s) (+ (totalsegundos ?h1 ?m1 ?s1) 59)))
=>
(retract ?f)
;(printout t "borrado siguientedatosensormovimiento" crlf)
)


(defrule pedirinforme
(declare (salience 10000))
?f <-  (pedirinforme  ?h ?m ?s ?habitacion)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
=>
(assert (informe ?habitacion))
(retract ?f)
)

(defrule manejointeligenteluces
(declare (salience 10000))
?f <-  (manejoluces ?habitacion)
=>
(assert (Manejo_inteligente_luces ?habitacion))
(retract ?f)
)

(defrule encenderluz
(declare (salience 10000))
?f <-  (accion pulsador_luz ?habitacion encender)
?g <- (luminosidad ?habitacion ?l)
(bombilla ?habitacion ?l2)
=>
(assert (datosensor ?*hora* ?*minutos* ?*segundos* pulsador ?habitacion on))
(assert (luminosidad ?habitacion (+ ?l ?l2)))
(retract ?f ?g)
)

(defrule apagarluz
(declare (salience 10000))
?f <-  (accion pulsador_luz ?habitacion apagar)
?g <- (luminosidad ?habitacion ?l)
(bombilla ?habitacion ?l2)
=>
(assert (datosensor ?*hora* ?*minutos* ?*segundos* pulsador ?habitacion off))
(assert (luminosidad ?habitacion (min 5 (- ?l ?l2))))
(retract ?f ?g)
)


