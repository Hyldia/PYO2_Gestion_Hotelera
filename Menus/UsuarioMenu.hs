module UsuarioMenu where
    mostrarMenuUsuario :: IO ()
    mostrarMenuUsuario = do
        putStrLn "\n===== MENU GENERAL ====="
        putStrLn "1. Ver disponibilidad"
        putStrLn "2. Reservacion"
        putStrLn "3. Anular reservacion"
        putStrLn "4. Facturar reservacion"
        putStrLn "5. Volver al menu principal"
        putStr "Seleccione una opcion: "

        opcion <- getLine
        case opcion of
            "1" -> putStrLn "Funcionalidad pendiente."
            "2" -> putStrLn "Funcionalidad pendiente."
            "3" -> putStrLn "Funcionalidad pendiente."
            "4" -> putStrLn "Funcionalidad pendiente."
            "5" -> return ()
            _ -> do
                putStrLn "Opcion invalida."
                mostrarMenuUsuario