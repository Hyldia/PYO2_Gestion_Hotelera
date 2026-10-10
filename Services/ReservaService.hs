{-
* Servicio de reservaciones
* manejo de estados de la reserva (Activa, Facturada, Cancelada)
* código automático de reserva
* También lee y guarda las reservas en database/reservas.txt para que las demás opciones (reservar, anular, historial) puedan usarlas.
-}

module Services.ReservaService where
    import Data.List (find)              -- find: busca el primer elemento que cumple una condición
    import Text.Printf (printf)          -- printf: da formato al código R001
    import Models.Reserva
    import Models.EstadoReserva
    import Utils.Archivo (escribirArchivo)

    {-
    * Se define una sola vez para no repetirla en cada función.
    * Restricción: el programa se ejecuta desde la carpeta raíz del proyecto.
    -}
    rutaReservas :: FilePath
    rutaReservas = "database/reservas.txt"

    {-
    * leer el archivo de reservas completo.
    * Entradas: ninguna.
    * Salida: IO String con el texto del archivo
    * lee el archivo 
    * length recorre todo el texto y obliga a leerlo completo 
    * seq = "evalúa lo de la izquierda antes de devolver lo de la derecha".
    -}

    leerArchivoReservas :: IO String
    leerArchivoReservas = do
        contenido <- readFile rutaReservas
        length contenido `seq` return contenido

    {-
    * obtener todas las reservas guardadas.
    * Entradas: ninguna.
    * Salida: IO [Reserva] (lista vacía si no hay reservas)
    * Restricción: cada línea del archivo es una Reserva escrita con show
    -}

    obtenerReservas :: IO [Reserva]
    obtenerReservas = do
        contenido <- leerArchivoReservas
        return (map read (filter (not . null) (lines contenido)))

    {-
    * guardar la lista completa de reservas en el archivo.
    * Entrada: la lista de reservas.
    * Salida: IO () (escribe en disco, no devuelve nada).
    * Restricción: reemplaza todo el archivo, así que se le pasa la lista completa. 
    -}
    guardarReservas :: [Reserva] -> IO ()
    guardarReservas reservas =
        escribirArchivo rutaReservas (unlines (map show reservas))