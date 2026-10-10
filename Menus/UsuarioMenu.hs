module Menus.UsuarioMenu where
    import Services.ReservaService
    mostrarMenuUsuario :: IO ()
    mostrarMenuUsuario = do
        putStrLn "\n\n===== MENU GENERAL ====="
        putStrLn "1. Ver disponibilidad"
        putStrLn "2. Reservacion"
        putStrLn "3. Anular reservacion"
        putStrLn "4. Facturar reservacion"
        putStrLn "5. Volver al menu principal"
        putStrLn "Seleccione una opcion: "

        opcion <- getLine
        case opcion of
            "1" -> putStrLn "Funcionalidad pendiente."
            "2" -> putStrLn "Funcionalidad pendiente."
            "3" -> do 
                opcionAnularReserva
                mostrarMenuUsuario
            "4" -> putStrLn "Funcionalidad pendiente."
            "5" -> do
                putStrLn "Volviendo al menu principal..."
                return ()
            _ -> do
                putStrLn "Opcion invalida."
                mostrarMenuUsuario