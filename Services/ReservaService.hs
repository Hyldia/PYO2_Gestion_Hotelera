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


    {-
    * generar el código de la siguiente reserva
    * Entrada: la lista de reservas existentes
    * Salida: el código nuevo
    * Restricción: las reservas nunca se borran (al anular solo cambian a Cancelada), por eso "cantidad + 1" nunca se repite.
    -}
    generarCodigoReserva :: [Reserva] -> String
    generarCodigoReserva reservas = printf "R%03d" (length reservas + 1)

    {-
    * buscar una reserva por su código.
    * Entradas: el código y la lista de reservas.
    * Salida: Just reserva si existe, Nothing si no existe.
    -}

    buscarReserva :: String -> [Reserva] -> Maybe Reserva
    buscarReserva codigo reservas =
        find (\reserva -> codigoReserva reserva == codigo) reservas

    {-
    * saber si una reserva está activa
    * Entrada: una reserva
    * Salida: True si su estado es Activa
    * Se usa antes de anular o facturar: solo se permite si está Activa.
    -}
    estaActiva :: Reserva -> Bool
    estaActiva reserva = estadoReserva reserva == Activa

    {-
    * cambiar el estado de una reserva Anular -> Cancelada. Facturar -> Facturada 
    * Entradas: el código, el estado nuevo y la lista de reservas
    * Salida: una lista nueva con esa reserva cambiada
    * Restricción: no valida si está Activa; eso se revisa antes con estaActiva.
    -}
    cambiarEstadoReserva :: String -> EstadoReserva -> [Reserva] -> [Reserva]
    cambiarEstadoReserva codigo nuevoEstado reservas = map actualizar reservas
        where
            actualizar reserva
                | codigoReserva reserva == codigo = reserva { estadoReserva = nuevoEstado }
                | otherwise = reserva