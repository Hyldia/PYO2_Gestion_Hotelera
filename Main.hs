module Main where
    import Menus.AdminMenu
    import Menus.UsuarioMenu

    menuPrincipal :: IO ()
    menuPrincipal = do
        putStrLn "\n\n===== SISTEMA DE GESTION HOTELERA ====="
        putStrLn "1. Menu Administrativo"
        putStrLn "2. Opciones Generales"
        putStrLn "3. Salir"
        putStrLn "Seleccione una opcion: "

        opcion <- getLine
        case opcion of
            "1" -> do
                mostrarMenuAdmin
                menuPrincipal
            "2" -> do
                mostrarMenuUsuario
                menuPrincipal
            "3" -> do
                putStrLn "Gracias por utilizar el sistema."
                return ()
            _ -> do
                putStrLn "Opcion invalida."
                main
                
    main :: IO ()
    main = menuPrincipal