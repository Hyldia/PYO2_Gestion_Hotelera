module Services.EstadoOcupacionService where
    import Models.Reserva --se usa para acceder a los datos de las reservas
    import Models.Habitacion (Habitacion) --tipo de las habitaciones del hotel
    import qualified Models.Habitacion as H -- alias para evitar choque de nombres (idHabitacion y tipoHabitacion existen tambien en DetalleReserva)
    import Services.EstadisticasService (Fecha, fechaATexto, solicitarFecha, habitacionesLibresEn, mostrarOcupadas, cargarHabitaciones, cargarReservas) --funciones que ya calculan y leen lo necesario

    {-
    * Imprime las habitaciones disponibles (libres) en una fecha con su tipo.
    * Entradas: la fecha, todas las habitaciones y las reservas. 
    *Salida: IO ().
    -}
    mostrarDisponibles :: Fecha -> [Habitacion] -> [Reserva] -> IO ()
    mostrarDisponibles fecha habitaciones reservas = do
        let libres = habitacionesLibresEn fecha habitaciones reservas
        putStrLn ("\nHabitaciones disponibles el " ++ fechaATexto fecha ++ ":")
        if null libres
            then putStrLn "===========No hay habitaciones libres para esta fecha==========="
            else mapM_ (\habitacion -> putStrLn ("  Habitacion " ++ H.idHabitacion habitacion ++ " | Tipo: " ++ H.tipoHabitacion habitacion)) libres

    {-
    * Pide la fecha y muestra las habitaciones disponibles y las ocupadas (con el codigo de la reserva).
    * Entradas: ninguna. Salidas : IO ().
    -}
    opcionEstadoOcupacion :: IO ()
    opcionEstadoOcupacion = do
        habitaciones <- cargarHabitaciones
        if null habitaciones
            then putStrLn "No existen habitaciones registradas."
            else do
                reservas <- cargarReservas
                fecha <- solicitarFecha "Ingrese la fecha a consultar (aaaa-mm-dd):"
                putStrLn "\n================ ESTADO DE OCUPACION ================"
                mostrarDisponibles fecha habitaciones reservas
                mostrarOcupadas fecha reservas
