module Menus.AdminMenu where -- Modulo para el menú de administración
    import Services.TipoHabitacionService
    import Utils.Archivo
    import Services.HabitacionService
    import Services.EstadisticasService
    import Services.ReservaService (opcionHistorialReservas)
    import Services.TarifaService (opcionCargarTarifas)
    import Services.EstadoOcupacionService (opcionEstadoOcupacion)

    mostrarMenuAdmin :: IO ()
    mostrarMenuAdmin = do
        putStrLn "\n\n====== MENU ADMINISTRATIVO ======"
        putStrLn "1: Cargar tipo de habitaciones"
        putStrLn "2. Asignar cantidad de habitaciones por tipo"
        putStrLn "3. Ver habitaciones"
        putStrLn "4. Cargar tarifas"
        putStrLn "5. Consultar historial de reservaciones"
        putStrLn "6. Consultar estado de ocupación"
        putStrLn "7. Estadísticas de ocupación"
        putStrLn "8. Volver al menu principal"
        putStrLn "Seleccione una opcion: "

        opcion <- getLine
        case opcion of
            "1" -> do
                putStrLn "Ingrese la ruta del archivo:"
                ruta <- getLine
                
                tipos <- cargarTiposDesdeRuta ruta
                if null tipos
                    then do
                        putStrLn "No se cargaron tipos de habitación."
                        mostrarMenuAdmin
                    else do
                        -- Tipos de habitación repetidos
                        mostrarTiposDuplicados tipos
                        guardarTiposHabitacion tipos
                        putStrLn "-----------------------------------------------"
                        putStrLn "Tipos de habitación cargados exitosamente."
                        mostrarMenuAdmin
            "2" -> do
                generarHabitacionesHotel
                mostrarMenuAdmin
            "3" -> do
                mostrarHabitaciones
                mostrarMenuAdmin
            "4" -> do
                opcionCargarTarifas
                mostrarMenuAdmin
            "5" -> do
                opcionHistorialReservas
                mostrarMenuAdmin
            "6" -> do
                opcionEstadoOcupacion
                mostrarMenuAdmin
            "7" -> do
                menuEstadisticas
                mostrarMenuAdmin
            "8" -> do
                putStrLn "Volviendo al menu principal..."
                return ()
            _ -> do
                putStrLn "Opcion invalida."
                mostrarMenuAdmin

    menuEstadisticas :: IO ()
    menuEstadisticas = do
        putStrLn "\n\n===== ESTADISTICAS DE OCUPACION ====="
        putStrLn "1. Total de huespedes por mes-año"
        putStrLn "2. Habitaciones ocupadas en un dia especifico"
        putStrLn "3. Total de habitaciones no ocupadas en un dia especifico"
        putStrLn "4. Monto recaudado (con impuestos)"
        putStrLn "5. Top 3 de dias de la semana con mas ocupacion"
        putStrLn "6. Volver al menu administrativo"
        putStrLn "Seleccione una opcion: "
        opcion <- getLine
        case opcion of
            "1" -> do
                mostrarHuespedesPorMes
                menuEstadisticas
            "2" -> do
                opcionHabitacionesOcupadas
                menuEstadisticas
            "3" -> do
                opcionHabitacionesLibres
                menuEstadisticas
            "4" -> do
                mostrarMontoRecaudado
                menuEstadisticas
            "5" -> do
                mostrarTop3DiasSemana
                menuEstadisticas
            "6" -> do
                putStrLn "Volviendo al menu administrativo..."
                return ()
            _ -> do
                putStrLn "Opcion invalida."
                menuEstadisticas
