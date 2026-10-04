import AdminMenu
import UsuarioMenu

main :: IO ()
main = do
    putStrLn "\n===== SISTEMA DE GESTION HOTELERA ====="
    putStrLn "1. Menu Administrativo"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr "Seleccione una opcion: "

    opcion <- getLine
    case opcion of
        "1" -> mostrarMenuAdmin
        "2" -> mostrarMenuUsuario
        "3" -> do
            putStrLn "Gracias por utilizar el sistema."
            return ()
        _ -> do
            putStrLn "Opcion invalida."
            main