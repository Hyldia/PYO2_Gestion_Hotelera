{-
* Servicio para crear reservaciones
* R-11: pide los datos de la reserva, los valida, asigna habitaciones libres y la guarda.
* R-12: el monto se calcula con MontoService según las tarifas cargadas (sin IVA)
* R-13: muestra el comprobante en pantalla (reutiliza mostrarReserva)
* R-20: solo se asignan habitaciones libres en TODAS las noches de la estadía
-}
module Services.CrearReservaService where
    import Data.Char (isSpace)
    import Data.Time (getZonedTime, formatTime, defaultTimeLocale)   -- fecha y hora actual de la reservación
    import Models.Reserva
    import Models.EstadoReserva
    import Models.DetalleReserva
    import Models.TipoHabitacion
    import Models.Tarifa (Tarifa)
    import Models.Habitacion (Habitacion)
    import qualified Models.Habitacion as H     -- "H." evita el choque con los campos de DetalleReserva
    import Services.ReservaService (obtenerReservas, guardarReservas, generarCodigoReserva, mostrarReserva)
    import Services.DisponibilidadService (habitacionesLibresEnRango)
    import Services.EstadisticasService (Fecha, fechaATexto, solicitarFecha, cargarHabitaciones)
    import Services.TipoHabitacionService (obtenerTiposHabitacion)
    import Services.TarifaService (obtenerTarifas, codigosFaltantes)
    import Services.MontoService (calcularMonto)
    import Utils.Validaciones (esNumeroEnteroPositivo)

    -- Lo que el usuario pide para UNA habitación: (tipo, adultos, niños)
    type SolicitudHabitacion = (String, Int, Int)

    {-
    * Pide un número entero dentro de un rango y repite la pregunta hasta que sea válido.
    * Entradas: el mensaje, el mínimo y el máximo permitidos. Salida: IO Int
    * Se limita a 6 dígitos para que un número gigante no desborde el Int
    -}
    leerEntero :: String -> Int -> Int -> IO Int
    leerEntero mensaje minimo maximo = do
        putStrLn mensaje
        texto <- getLine
        let limpio = filter (not . isSpace) texto
        if esNumeroEnteroPositivo limpio && length limpio <= 6 && read limpio >= minimo && read limpio <= maximo
            then return (read limpio)
            else do
                putStrLn ("Valor invalido. Ingrese un numero entre " ++ show minimo ++ " y " ++ show maximo ++ ".")
                leerEntero mensaje minimo maximo

    {-
    * Pide el nombre de quien reserva; no puede estar vacío.
    * Salida: IO String con el nombre
    * all isSpace es True si el texto está vacío o solo tiene espacios
    -}
    leerNombre :: IO String
    leerNombre = do
        putStrLn "Nombre de la persona que reserva:"
        nombre <- getLine
        if all isSpace nombre
            then do
                putStrLn "El nombre no puede estar vacio."
                leerNombre
            else return nombre

    {-
    * Pide las fechas de entrada y salida; la salida debe ser posterior a la entrada.
    * Salida: IO (entrada, salida)
    * Las fechas son tuplas (año, mes, día) y Haskell las compara en orden cronológico
    -}
    leerFechasEstadia :: IO (Fecha, Fecha)
    leerFechasEstadia = do
        entrada <- solicitarFecha "Fecha de entrada (aaaa-mm-dd):"
        salida <- solicitarFecha "Fecha de salida (aaaa-mm-dd):"
        if salida > entrada
            then return (entrada, salida)
            else do
                putStrLn "La fecha de salida debe ser posterior a la de entrada."
                leerFechasEstadia

    {-
    * Muestra los tipos de habitación numerados y pide elegir uno.
    * Entrada: los tipos. Salida: IO TipoHabitacion con el tipo elegido
    -}
    elegirTipo :: [TipoHabitacion] -> IO TipoHabitacion
    elegirTipo tipos = do
        putStrLn "Tipos de habitacion:"
        mapM_ (\(numero, tipo) -> putStrLn ("  " ++ show numero ++ ". " ++ nombreTipo tipo ++ " (maximo " ++ show (maximoPersonas tipo) ++ " personas)")) (zip [1 :: Int ..] tipos)
        opcion <- leerEntero "Elija el tipo:" 1 (length tipos)
        return (tipos !! (opcion - 1))

    {-
    * Pide el tipo, los adultos y los niños de UNA habitación.
    * Entradas: los tipos y el número de habitación (para mostrarlo) Salida: IO SolicitudHabitacion
    * Restricción: adultos + niños no puede pasar del máximo del tipo, y debe haber al menos 1 persona.
    * Por eso, si no hay adultos, se exige mínimo 1 niño.
    -}
    leerSolicitud :: [TipoHabitacion] -> Int -> IO SolicitudHabitacion
    leerSolicitud tipos numero = do
        putStrLn ("\n--- Habitacion " ++ show numero ++ " ---")
        tipo <- elegirTipo tipos
        let maximo = maximoPersonas tipo
        adultos <- leerEntero ("Adultos en esta habitacion (0 a " ++ show maximo ++ "):") 0 maximo
        let minimoNinos = if adultos == 0 then 1 else 0
        ninos <- leerEntero ("Ninos en esta habitacion (" ++ show minimoNinos ++ " a " ++ show (maximo - adultos) ++ "):") minimoNinos (maximo - adultos)
        return (nombreTipo tipo, adultos, ninos)
    {-
    * Revisa que los huéspedes repartidos en las habitaciones sumen los totales indicados
    * Entradas: total de adultos, total de niños y las solicitudes. Salida: True si coinciden.
    -}
    totalesCoinciden :: Int -> Int -> [SolicitudHabitacion] -> Bool
    totalesCoinciden adultos ninos solicitudes =
        sum (map (\(_, a, _) -> a) solicitudes) == adultos
        && sum (map (\(_, _, n) -> n) solicitudes) == ninos

    {-
    * Asigna una habitación libre del tipo pedido a cada solicitud, sin repetir habitaciones
    * Entradas: las solicitudes y las habitaciones libres en TODO el rango de fechas.
    * Salida: Right con los detalles ya asignados, o Left con el mensaje si no alcanzan.
    -}
    asignarHabitaciones :: [SolicitudHabitacion] -> [Habitacion] -> Either String [DetalleReserva]
    asignarHabitaciones [] _ = Right []
    asignarHabitaciones ((tipo, adultos, ninos) : resto) libres =
        case filter (\habitacion -> H.tipoHabitacion habitacion == tipo) libres of
            [] -> Left ("No hay suficientes habitaciones de tipo " ++ tipo ++ " disponibles en esas fechas.")
            (habitacion : _) ->
                let detalle = DetalleReserva {
                        idHabitacion = H.idHabitacion habitacion,
                        tipoHabitacion = tipo,
                        adultosHabitacion = adultos,
                        ninosHabitacion = ninos }
                    libresRestantes = filter (/= habitacion) libres
                in fmap (detalle :) (asignarHabitaciones resto libresRestantes)

    {-
    * Revisa que existan los datos necesarios antes de reservar.
    * Entradas: habitaciones, tipos y tarifas. Salida: Just mensaje si falta algo, Nothing si todo está bien.
    -}
    verificarDatosBase :: [Habitacion] -> [TipoHabitacion] -> [Tarifa] -> Maybe String
    verificarDatosBase habitaciones tipos tarifas
        | null tipos = Just "No hay tipos de habitacion cargados."
        | null habitaciones = Just "No existen habitaciones registradas."
        | null tarifas = Just "No hay tarifas cargadas. Primero cargue el archivo de tarifas (Menu administrativo, opcion 4)."
        | not (null (codigosFaltantes tarifas)) = Just ("Faltan las tarifas " ++ show (codigosFaltantes tarifas) ++ ". Cargue el archivo completo de tarifas (Menu administrativo, opcion 4).")
        | otherwise = Nothing

    {-
    * Pide todos los datos, valida, asigna habitaciones, guarda la reserva y muestra el comprobante
    * Entradas: habitaciones, tipos y tarifas ya verificados. Salida: IO ()
    * Si los totales no coinciden o no hay habitaciones suficientes, no se guarda nada
    -}
    realizarReserva :: [Habitacion] -> [TipoHabitacion] -> [Tarifa] -> IO ()
    realizarReserva habitaciones tipos tarifas = do
        putStrLn "\n===== NUEVA RESERVACION ====="
        nombre <- leerNombre
        (entrada, salida) <- leerFechasEstadia
        adultos <- leerEntero "Cantidad total de adultos:" 0 999
        ninos <- leerEntero "Cantidad total de ninos:" (if adultos == 0 then 1 else 0) 999
        cantidad <- leerEntero "Cantidad de habitaciones:" 1 (min (length habitaciones) (adultos + ninos))
        solicitudes <- mapM (leerSolicitud tipos) [1 .. cantidad]
        reservas <- obtenerReservas
        let libres = habitacionesLibresEnRango entrada salida habitaciones reservas
        if not (totalesCoinciden adultos ninos solicitudes)
            then putStrLn "Los huespedes por habitacion no suman los totales indicados. La reservacion no se realizo."
            else case asignarHabitaciones solicitudes libres of
                Left mensaje -> putStrLn (mensaje ++ " La reservacion no se realizo.")
                Right detalles -> do
                    ahora <- getZonedTime
                    let nueva = Reserva {
                            codigoReserva = generarCodigoReserva reservas,
                            nombreCliente = nombre,
                            fechaReserva = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" ahora,
                            fechaEntrada = fechaATexto entrada,
                            fechaSalida = fechaATexto salida,
                            adultosTotales = adultos,
                            ninosTotales = ninos,
                            estadoReserva = Activa,
                            montoReserva = calcularMonto tarifas entrada salida adultos ninos,
                            detalleReserva = detalles }
                    guardarReservas (reservas ++ [nueva])
                    putStrLn "\n===== COMPROBANTE DE RESERVACION ====="
                    mostrarReserva nueva

    {-
    * Opción "Reservacion" del menú general
    * Primero verifica que existan tipos, habitaciones y tarifas; si falta algo, avisa
    -}
    opcionReservar :: IO ()
    opcionReservar = do
        habitaciones <- cargarHabitaciones
        tipos <- obtenerTiposHabitacion
        tarifas <- obtenerTarifas
        case verificarDatosBase habitaciones tipos tarifas of
            Just mensaje -> putStrLn mensaje
            Nothing -> realizarReserva habitaciones tipos tarifas