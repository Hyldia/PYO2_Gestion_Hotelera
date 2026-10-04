module AdminMenu where -- Modulo para el menú de administración
    mostarMenuAdmin :: IO ()
    mostarMenuAdmin = do
        putStrLn "====== MENU ADMINISTRATIVO ======"
        putStrLn "1: Cargar tipo de habitaciones"
        putStrLn "2. Asignar cantidad de habitaciones"
        putStrLn "3. Ver habitaciones"
        putStrLn "4. Cargar tarifas"
        putStrLn "5. Consultar reservaciones"
        putStrLn "6. Estado de ocupación"
        putStrLn "7. Estadísticas"
        putStrLn "8. Volver al menu principal"
        putStr "Seleccione una opcion: "

        opcion <- getLine
        case opcion of
            "1" -> putStrLn "Funcionalidad pendiente."
            "2" -> putStrLn "Funcionalidad pendiente."
            "3" -> putStrLn "Funcionalidad pendiente."
            "4" -> putStrLn "Funcionalidad pendiente."
            "5" -> putStrLn "Funcionalidad pendiente."
            "6" -> putStrLn "Funcionalidad pendiente."
            "7" -> putStrLn "Funcionalidad pendiente."
            "8" -> return ()
            _ -> do
                putStrLn "Opcion invalida."
                mostarMenuAdmin
                