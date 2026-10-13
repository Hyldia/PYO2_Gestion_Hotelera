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

    {-
    * Habitaciones libres durante TODAS las noches de una estadía
    * Entradas: fecha de entrada, fecha de salida, las habitaciones y las reservas
    * Salida: solo las habitaciones que no están ocupadas en ninguna noche del rango
    * Restricción: la salida debe ser posterior a la entrada (se valida antes de llamarla)
    * si no, no hay noches y todas parecerían libres.
    * Así se evita reservar una habitación en fechas que se cruzan con otra
    * reserva, pero se permite reservarla en otras fechas.
    * 
    -}
    habitacionesLibresEnRango :: Fecha -> Fecha -> [Habitacion] -> [Reserva] -> [Habitacion]
    habitacionesLibresEnRango entrada salida habitaciones reservas =
        filter libreTodasLasNoches habitaciones
        where
            noches = nochesEntre entrada salida
            idsOcupadosEn noche = map fst (ocupacionesEn noche reservas)
            libreTodasLasNoches habitacion =
                all (\noche -> H.idHabitacion habitacion `notElem` idsOcupadosEn noche) noches