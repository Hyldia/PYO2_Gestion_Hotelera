{-
* Servicio de disponibilidad
* R-10:ver las habitaciones disponibles en un día
* R-20: saber qué habitaciones están libres en todo un rango de fechas,para no reservar dos veces la misma habitación en fechas que se cruzan
* Reutiliza las funciones de fechas y ocupación de EstadisticasService
-}
module Services.DisponibilidadService where
    import Models.Reserva (Reserva)
    import Models.Habitacion (Habitacion)
    import qualified Models.Habitacion as H   -- "H." evita choque de nombres con DetalleReserva (ambos tienen idHabitacion)
    import Services.EstadisticasService (Fecha, fechaATexto, solicitarFecha, habitacionesLibresEn, ocupacionesEn, nochesEntre, cargarHabitaciones, cargarReservas)

    {-
    * Muestra una habitación en una línea: id y tipo.
    * Entrada: una habitación. Salida: IO 
    -}
    mostrarHabitacionDisponible :: Habitacion -> IO ()
    mostrarHabitacionDisponible habitacion =
        putStrLn ("  ID: " ++ H.idHabitacion habitacion ++ " | Tipo: " ++ H.tipoHabitacion habitacion)

    {-
    * Opción "Ver disponibilidad"
    * Pide un día y muestra las habitaciones que están libres ese día.
    * Entradas: ninguna (lee del teclado y de los archivos)Salida: IO 
    * Restricción: si todavía no se han generado habitaciones, lo avisa y no pide fecha
    * Una habitación está ocupada si tiene una reserva no cancelada con
    * entrada <= día < salida (el día de salida no cuenta).
    -}
    opcionVerDisponibilidad :: IO ()
    opcionVerDisponibilidad = do
        habitaciones <- cargarHabitaciones
        if null habitaciones
            then putStrLn "No existen habitaciones registradas."
            else do
                reservas <- cargarReservas
                fecha <- solicitarFecha "Ingrese el dia a consultar (aaaa-mm-dd):"
                let libres = habitacionesLibresEn fecha habitaciones reservas
                putStrLn ("\n===== HABITACIONES DISPONIBLES EL " ++ fechaATexto fecha ++ " =====")
                if null libres
                    then putStrLn "No hay habitaciones disponibles para ese dia."
                    else mapM_ mostrarHabitacionDisponible libres