module Menus.AdminMenu where -- Modulo para el menú de administración
    import Services.TipoHabitacionService
    import Utils.Archivo

    mostrarMenuAdmin :: IO ()
    mostrarMenuAdmin = do
        putStrLn "\n\n====== MENU ADMINISTRATIVO ======"
        putStrLn "1: Cargar tipo de habitaciones"
        putStrLn "2. Asignar cantidad de habitaciones"
        putStrLn "3. Ver habitaciones"
        putStrLn "4. Cargar tarifas"
        putStrLn "5. Consultar reservaciones"
        putStrLn "6. Estado de ocupación"
        putStrLn "7. Estadísticas"
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
            "2" -> putStrLn "Funcionalidad pendiente."
            "3" -> putStrLn "Funcionalidad pendiente."
            "4" -> putStrLn "Funcionalidad pendiente."
            "5" -> putStrLn "Funcionalidad pendiente."
            "6" -> putStrLn "Funcionalidad pendiente."
            "7" -> putStrLn "Funcionalidad pendiente."
            "8" -> do
                putStrLn "Volviendo al menu principal..."
                return ()
            _ -> do
                putStrLn "Opcion invalida."
                mostrarMenuAdmin
                