module Services.FacturaService where
    import Text.Printf (printf)          -- printf: da formato al codigo F001 y a los montos
    import Models.Reserva
    import Models.EstadoReserva
    import Models.Factura
    import Models.Hotel
    import Services.HotelService (obtenerHotel)
    import Services.ReservaService (buscarReserva, estaActiva, normalizarCodigo, cambiarEstadoReserva, guardarReservas, detalleATexto) --se usa para buscar, validar y actualizar la reserva
    import Services.EstadisticasService (cargarReservas) --lee las reservas y devuelve lista vacia si el archivo no existe
    import Utils.Archivo (archivoExiste, leerArchivo, agregarArchivo) --se usa para leer y agregar facturas al archivo
    import Utils.Impuestos (tasaIVA) --porcentaje del impuesto de ventas (13%)

    {-
    * Se define una sola vez para no repetirla en cada funcion.
    -}
    rutaFacturas :: FilePath
    rutaFacturas = "database/facturas.txt"

    {-
    * Lee las facturas guardadas; si el archivo todavia no existe devuelve lista vacia en lugar de fallar.
    * Entradas: ninguna. Salida: IO [Factura].
    -}
    obtenerFacturas :: IO [Factura]
    obtenerFacturas = do
        existe <- archivoExiste rutaFacturas
        if existe
            then do
                contenido <- leerArchivo rutaFacturas
                return (map read (filter (not . null) (lines contenido)))
            else return []

    {-
    * Genera el codigo de la siguiente factura.
    * Entrada: la lista de facturas existentes. Salida: el codigo nuevo (F001, F002, ...).
    * NOTA: las facturas nunca se borran, por eso "cantidad + 1" nunca se repite.
    -}
    generarCodigoFactura :: [Factura] -> String
    generarCodigoFactura facturas = printf "F%03d" (length facturas + 1)

    {-
    * Revisa si una reserva se puede facturar.
    * Entradas: el codigo de la reserva y la lista de reservas.
    * Salida: Left con el mensaje de error, o Right con la reserva encontrada.
    * NOTA: solo se facturan reservas que existan y esten Activas.
    -}
    validarFacturacion :: String -> [Reserva] -> Either String Reserva
    validarFacturacion codigo reservas =
        case buscarReserva codigo reservas of
            Nothing -> Left ("No existe una reserva con el codigo " ++ codigo ++ ".")
            Just reserva
                | estaActiva reserva -> Right reserva
                | otherwise -> Left ("La reserva " ++ codigo ++ " no se puede facturar porque su estado es " ++ show (estadoReserva reserva) ++ ".")

    {-
    * Crea la factura de una reserva.
    * Entradas: el codigo de la factura y la reserva.
    * Salida: la factura con subtotal (monto sin IVA), IVA y total.
    * NOTA: el IVA se importa de tasaIVA en Utils.Impuestos.
    -}
    crearFactura :: String -> Reserva -> Factura
    crearFactura codigo reserva = Factura
        { codigoFactura = codigo
        , codigoReservaFacturada = codigoReserva reserva
        , subtotalFactura = subtotal
        , ivaFactura = iva
        , totalFactura = subtotal + iva
        }
        where
            subtotal = montoReserva reserva
            iva = subtotal * tasaIVA

    {-
    * Factura una reserva: valida, crea la factura y marca la reserva como Facturada.
    * Entradas: el codigo de la reserva, las reservas y las facturas existentes.
    * Salida: Left con el mensaje de error, o Right con la factura nueva, la reserva facturada y la lista de reservas actualizada.
    -}
    facturarReserva :: String -> [Reserva] -> [Factura] -> Either String (Factura, Reserva, [Reserva])
    facturarReserva codigo reservas facturas =
        case validarFacturacion codigo reservas of
            Left mensajeError -> Left mensajeError
            Right reserva ->
                Right ( crearFactura (generarCodigoFactura facturas) reserva
                      , reserva
                      , cambiarEstadoReserva codigo Facturada reservas )

    {-
    * Imprime la factura completa: datos del hotel, codigos, detalle de la reserva y totales.
    * Entradas: el hotel, la reserva facturada y la factura. Salida: IO ().
    -}
    mostrarFactura :: Hotel -> Reserva -> Factura -> IO ()
    mostrarFactura hotel reserva factura = do
        putStrLn "\n================ FACTURA ================"
        putStrLn ("  " ++ nombre hotel)
        putStrLn ("  Cedula juridica: " ++ cedulaJuridica hotel)
        putStrLn ("  Sitio web: " ++ sitioWeb hotel ++ "   Telefono: " ++ telefono hotel)
        putStrLn ("  " ++ provincia hotel ++ ", " ++ pais hotel)
        putStrLn "  ----------------------------------------"
        putStrLn ("  Factura: " ++ codigoFactura factura)
        putStrLn ("  Reserva: " ++ codigoReservaFacturada factura)
        putStrLn ("  Cliente: " ++ nombreCliente reserva)
        putStrLn ("  Fecha y hora de reservacion: " ++ fechaReserva reserva)
        putStrLn ("  Entrada: " ++ fechaEntrada reserva ++ "   Salida: " ++ fechaSalida reserva)
        putStrLn ("  Adultos: " ++ show (adultosTotales reserva) ++ "   Ninos: " ++ show (ninosTotales reserva))
        putStrLn "  Habitaciones:"
        mapM_ (putStrLn . detalleATexto) (detalleReserva reserva)
        putStrLn "  ----------------------------------------"
        putStrLn ("  Subtotal: " ++ printf "%.2f" (subtotalFactura factura))
        putStrLn ("  IVA (13%): " ++ printf "%.2f" (ivaFactura factura))
        putStrLn ("  Total: " ++ printf "%.2f" (totalFactura factura))
        putStrLn "=========================================="

    {-
    * Opcion "Facturar reservacion" del menu general.
    * Pide el codigo, intenta facturar y muestra el resultado.
    * Entradas: ninguna (lee del teclado). Salida: IO ()
    -}
    opcionFacturarReserva :: IO ()
    opcionFacturarReserva = do
        putStrLn "Ingrese el codigo de la reserva a facturar (ej: R001):"
        texto <- getLine
        let codigo = normalizarCodigo texto
        reservas <- cargarReservas
        facturas <- obtenerFacturas
        case facturarReserva codigo reservas facturas of
            Left mensajeError -> putStrLn mensajeError
            Right (factura, reserva, reservasActualizadas) -> do
                hotel <- obtenerHotel
                guardarReservas reservasActualizadas
                agregarArchivo rutaFacturas (show factura ++ "\n")
                mostrarFactura hotel reserva factura
                putStrLn ("Reserva " ++ codigo ++ " facturada.")
