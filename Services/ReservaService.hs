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
    import Data.Char (toUpper, isSpace)  -- toUpper: pasa a mayúscula; isSpace: detecta espacios

    import Models.DetalleReserva         -- para leer los datos de cada habitación de la reserva

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

    {-
    * Limpia el código que escribe el usuario: quita espacios y lo pasa a mayúsculas.
    * Entrada: el texto escrito
    * Salida: el código limpio
    -}
    normalizarCodigo :: String -> String
    normalizarCodigo texto = map toUpper (filter (not . isSpace) texto)

    {-
    * Revisa si una reserva se puede anular y, si se puede, la cancela
    * Entradas: el código de la reserva y la lista de reservas.
    * Salida: Left con el mensaje de error, o Right con la lista ya actualizada.
    * Restricción: solo se anulan reservas que existan y estén Activas.
    * Las habitaciones se "liberan" solas: la disponibilidad ignora las reservas Canceladas, así que no hay que borrar nada
    -}
    anularReserva :: String -> [Reserva] -> Either String [Reserva]
    anularReserva codigo reservas =
        case buscarReserva codigo reservas of
            Nothing -> Left ("No existe una reserva con el codigo " ++ codigo ++ ".")
            Just reserva
                | estaActiva reserva -> Right (cambiarEstadoReserva codigo Cancelada reservas)
                | otherwise -> Left ("La reserva " ++ codigo ++ " no se puede anular porque esta " ++ show (estadoReserva reserva) ++ ".")

    {-
    * Opción "Anular reservación" del menú general
    * Pide el código, intenta anular y muestra el resultado
    * Entradas: ninguna (lee del teclado). Salida: IO ()
    * Sigue el patrón leer -> transformar -> guardar:
    *   obtenerReservas (IO) -> anularReserva (pura) -> guardarReservas (IO)
    * Aquí importa que leerArchivoReservas lea completo el archivo: se leey enseguida se vuelve a escribir reservas.txt.
    -}
    opcionAnularReserva :: IO ()
    opcionAnularReserva = do
        putStrLn "Ingrese el codigo de la reserva a anular (ej: R001) o 0 para volver:"
        texto <- getLine
        let codigo = normalizarCodigo texto
        case codigo of
            "0" -> putStrLn "Volviendo al menu general..."
            "" -> do
                putStrLn "Debe ingresar un codigo."
                opcionAnularReserva
            _ -> do
                reservas <- obtenerReservas
                case anularReserva codigo reservas of
                    Left mensajeError -> do
                        putStrLn mensajeError
                        putStrLn "Intente de nuevo.\n"
                        opcionAnularReserva
                    Right reservasActualizadas -> do
                        guardarReservas reservasActualizadas
                        putStrLn ("Reserva " ++ codigo ++ " anulada. Sus habitaciones quedan libres para esas fechas.")

    {-
    * Convierte una habitación reservada en una línea de texto.
    * Entrada: un DetalleReserva.
    * Salida: texto con id, tipo, adultos y niños de esa habitación.
    -}
    detalleATexto :: DetalleReserva -> String
    detalleATexto detalle =
        "    Habitacion " ++ idHabitacion detalle
        ++ " | Tipo: " ++ tipoHabitacion detalle
        ++ " | Adultos: " ++ show (adultosHabitacion detalle)
        ++ " | Ninos: " ++ show (ninosHabitacion detalle)

    {-
    * Muestra en pantalla toda la información de una reserva.
    * Entrada: una reserva.
    * Se usa en el historial y se va a reutilizar en el comprobante
    * Haskell: mapM_ aplica una acción IO a cada elemento de una lista;
    * (putStrLn . detalleATexto) es composición: primero convierte a texto y luego imprime.
    -}
    mostrarReserva :: Reserva -> IO ()
    mostrarReserva reserva = do
        putStrLn ("Reserva: " ++ codigoReserva reserva ++ "   Estado: " ++ show (estadoReserva reserva))
        putStrLn ("  Cliente: " ++ nombreCliente reserva)
        putStrLn ("  Fecha y hora de reservacion: " ++ fechaReserva reserva)
        putStrLn ("  Entrada: " ++ fechaEntrada reserva ++ "   Salida: " ++ fechaSalida reserva)
        putStrLn ("  Adultos: " ++ show (adultosTotales reserva) ++ "   Ninos: " ++ show (ninosTotales reserva))
        putStrLn ("  Monto (sin IVA): " ++ printf "%.2f" (montoReserva reserva))
        putStrLn "  Habitaciones:"
        mapM_ (putStrLn . detalleATexto) (detalleReserva reserva)
        putStrLn "--------------------------------------------------"

    {-
    * Opción "Consultar reservaciones" del menú administrativo 
    * Lee todas las reservas y las muestra, sin importar su estado.
    * Entradas: ninguna. Salida: IO
    -}
    opcionHistorialReservas :: IO ()
    opcionHistorialReservas = do
        reservas <- obtenerReservas
        putStrLn "\n===== HISTORIAL DE RESERVACIONES ====="
        if null reservas
            then putStrLn "No hay reservaciones registradas."
            else mapM_ mostrarReserva reservas