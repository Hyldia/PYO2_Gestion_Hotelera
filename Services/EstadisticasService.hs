module Services.EstadisticasService where
    import Data.List (sortOn, groupBy, sortBy) -- sortOn se usa como ordenamiento, se usa para poner los meses iguales juntos y groupBy para agruparlos
    import Data.Function (on) -- on compara usando solo una parte en especcifico de los datos
    import Data.Time (Day, addDays, dayOfWeek, defaultTimeLocale, fromGregorianValid, parseTimeM, toGregorian) -- se usa para manejar y parsear fechas
    import Data.Char (isDigit) -- isDigit valida que cada posicion de la fecha sea un numero
    import Models.Reserva --se usa para acceder a los datos de las reservas
    import Models.EstadoReserva --se usa para acceder al estado de las reservas
    import Models.DetalleReserva (DetalleReserva(..)) --se usa para acceder a las habitaciones de cada reserva
    import Models.Habitacion (Habitacion) --tipo de las habitaciones del hotel
    import qualified Models.Habitacion as H -- alias para evitar choque de nombres (idHabitacion existe tambien en DetalleReserva)
    import Services.ReservaService (obtenerReservas, rutaReservas) --se usa para obtener las reservas y la ruta del archivo
    import Services.HabitacionService (obtenerHabitaciones) --se usa para leer las habitaciones guardadas
    import Utils.Archivo (archivoExiste) --se usa para verificar si el archivo de reservas existe
    import Text.Printf (printf) --se usa para dar formato a la fecha y para mostrar el monto con 2 decimales
    import Utils.Impuestos (tasaIVA) --porcentaje del impuesto de ventas (13%)
    import Data.Ord (comparing) --comparing indica por cual dato se ordena
    type Fecha = (Integer, Int, Int)

    --Auxiliares

    {-
     *Convierte una fecha a texto en formato "YYYY-MM-DD".
     *Entrada: la fecha.
    *Salida: el texto.
    -}
    fechaATexto :: Fecha -> String
    fechaATexto (anio, mes, dia) = printf "%04d-%02d-%02d" anio mes dia

    {-
    * Parsea una fecha en formato estricto "YYYY-MM-DD" a una tupla (año, mes, dia).
    * Entrada: cadena de texto con la fecha. 
    *Salida: Just (año, mes, dia) o Nothing si el formato o la fecha es invalido.
    -}
    parsearFecha :: String -> Maybe Fecha
    parsearFecha fecha
        | formatoValido fecha = fmap toGregorian (parseTimeM False defaultTimeLocale "%Y-%m-%d" fecha :: Maybe Day)
        | otherwise = Nothing
        where
            -- Exige 4 digitos, guion, 2 digitos, guion, 2 digitos
            formatoValido texto = length texto == 10 && and (zipWith coincide texto "dddd-dd-dd")
            coincide caracter 'd' = isDigit caracter
            coincide caracter guion = caracter == guion

    {-
    * Obtiene las fechas de entrada y salida de una reserva ya convertidas a Fecha.
    * Entrada: una reserva. Salida: Just (entrada, salida) o Nothing si alguna fecha guardada es invalida.
    -}
    periodoDeReserva :: Reserva -> Maybe (Fecha, Fecha)
    periodoDeReserva reserva = do
        entrada <- parsearFecha (fechaEntrada reserva)
        salida <- parsearFecha (fechaSalida reserva)
        return (entrada, salida)
    {-
    * Deja solo las reservas que cuentan (Activas y Facturadas); las canceladas se ignoran
    * Entrada: lista de reservas.
    *Salida: lista filtrada.
    -}
    reservasVigentes :: [Reserva] -> [Reserva]
    reservasVigentes = filter (\reserva -> estadoReserva reserva /= Cancelada)
    
    {-
    * Lee las reservas guardadas; si el archivo todavia no existe devuelve lista vacia en lugar de fallar.
    *Entradas: ninguna. 
    *Salida: IO [Reserva].
    -}
    cargarReservas :: IO [Reserva]
    cargarReservas = do
        existe <- archivoExiste rutaReservas
        if existe then obtenerReservas else return []

    {-
    * Ruta del archivo de habitaciones (la misma que usa HabitacionService).
    -}
    rutaHabitaciones :: FilePath
    rutaHabitaciones = "database/habitaciones.txt"


    {-
    * Lee las habitaciones guardadas; si el archivo todavia no existe devuelve lista vacia.
    * Entradas: ninguna. Salida: IO [Habitacion].
    -}
    cargarHabitaciones :: IO [Habitacion]
    cargarHabitaciones = do
        existe <- archivoExiste rutaHabitaciones
        if existe then obtenerHabitaciones else return []


    {-
    * Total de huespedes (adultos + niños) agrupado por mes y año de la fecha de entrada.
    * Entrada: las reservas. 
    *Salida: lista filtrada y ordenada por año y mes.
    * Se ignoran las reservas canceladas y las que tengan una fecha de entrada invalida.
    -}
    huespedesPorMes :: [Reserva] -> [((Integer, Int), Int)]
    huespedesPorMes reservas =
        concatMap sumarGrupo (groupBy ((==) `on` fst) (sortOn fst pares))
        where
            pares = [ ((anio, mes), adultosTotales reserva + ninosTotales reserva)
                    | reserva <- reservasVigentes reservas
                    , Just (anio, mes, _) <- [parsearFecha (fechaEntrada reserva)] ]
            -- groupBy nunca produce grupos vacios; el caso [] solo cubre el patron
            sumarGrupo [] = []
            sumarGrupo grupo@((clave, _) : _) = [(clave, sum (map snd grupo))]


    {-
    * Imprime el total de huespedes por mes-año.
    -}

    --Podria vaidar que si el mes de entrada es diferente al mes de salida cuente en ambos meses
    mostrarHuespedesPorMes :: IO ()
    mostrarHuespedesPorMes = do
        reservas <- cargarReservas
        let resultado = huespedesPorMes reservas
        putStrLn "\n================ Huespedes por mes-año: ================"
        if null resultado
            then putStrLn "  (no hay reservaciones)"
            else mapM_ (\((anio, mes), total) -> putStrLn ("  " ++ show mes ++ "/" ++ show anio ++ ": " ++ show total ++ " huespedes")) resultado


    {-
    * Indica si una reserva mantiene ocupadas sus habitaciones en una fecha.
    * Entradas: la fecha y la reserva. Salida: True si no esta cancelada y entrada <= fecha < salida.
    * Las tuplas (año, mes, dia) se comparan en orden cronologico directamente.
    -}
    reservaOcupaFecha :: Fecha -> Reserva -> Bool
    reservaOcupaFecha fecha reserva =
        case periodoDeReserva reserva of
            Nothing -> False
            Just (entrada, salida) ->
                estadoReserva reserva /= Cancelada && entrada <= fecha && fecha < salida

    {-
    * Lista las habitaciones ocupadas en una fecha junto con el codigo de la reserva que las ocupa.
    * Entradas: la fecha y las reservas. 
    *Salida: lista de (idHabitacion, codigoReserva) ordenada por habitacion.
    -}
    ocupacionesEn :: Fecha -> [Reserva] -> [(String, String)]
    ocupacionesEn fecha reservas =
        sortOn fst [ (idHabitacion detalle, codigoReserva reserva)
                   | reserva <- reservas, reservaOcupaFecha fecha reserva
                   , detalle <- detalleReserva reserva ]

    {-
    * Habitaciones del hotel que NO estan ocupadas en una fecha.
    * Entradas: la fecha, todas las habitaciones y las reservas. 
    *Salida: las habitaciones libres.
    -}
    habitacionesLibresEn :: Fecha -> [Habitacion] -> [Reserva] -> [Habitacion]
    habitacionesLibresEn fecha habitaciones reservas =
        filter (\habitacion -> H.idHabitacion habitacion `notElem` idsOcupados) habitaciones
        where idsOcupados = map fst (ocupacionesEn fecha reservas)



    {-
    * Pide una fecha al usuario y repite la pregunta hasta que sea valida.
    * Entrada: el mensaje a mostrar. 
    *Salida: IO Fecha.
    -}
    solicitarFecha :: String -> IO Fecha
    solicitarFecha mensaje = do
        putStrLn mensaje
        texto <- getLine
        case parsearFecha texto of
            Just fecha -> return fecha
            Nothing -> do
                putStrLn "Fecha invalida. Use el formato aaaa-mm-dd (ej: 2026-12-25)."
                solicitarFecha mensaje


    {-
    * Imprime las habitaciones ocupadas en una fecha con su codigo de reserva.
    * Entradas: la fecha y las reservas. 
    *Salida: IO ().
    -}
    mostrarOcupadas :: Fecha -> [Reserva] -> IO ()
    mostrarOcupadas fecha reservas = do
        let ocupadas = ocupacionesEn fecha reservas
        putStrLn ("\nHabitaciones ocupadas el " ++ fechaATexto fecha ++ ":")
        if null ocupadas
            then putStrLn "  (ninguna)"
            else mapM_ (\(habitacion, reserva) -> putStrLn ("  Habitacion " ++ habitacion ++ " | Reserva " ++ reserva)) ocupadas

    {-
    *  muestra las habitaciones ocupadas ese dia.
    * Entradas: ninguna (lee del teclado y de los archivos). 
    *Salida: IO ().
    -}
    opcionHabitacionesOcupadas :: IO ()
    opcionHabitacionesOcupadas = do
        reservas <- cargarReservas
        fecha <- solicitarFecha "Ingrese el dia a consultar (aaaa-mm-dd):"
        mostrarOcupadas fecha reservas

    {-
    * muestra cuantas habitaciones NO estan ocupadas ese dia.
    * Entradas: ninguna (lee del teclado y de los archivos). 
    *Salida: IO ().
    -}
    opcionHabitacionesLibres :: IO ()
    opcionHabitacionesLibres = do
        habitaciones <- cargarHabitaciones
        if null habitaciones
            then putStrLn "No existen habitaciones registradas."
            else do
                reservas <- cargarReservas
                fecha <- solicitarFecha "Ingrese el dia a consultar (aaaa-mm-dd):"
                let libres = habitacionesLibresEn fecha habitaciones reservas
                putStrLn ("\nHabitaciones no ocupadas el " ++ fechaATexto fecha ++ ": " ++ show (length libres))

    {-
    * Monto total recaudado con impuesto de las reservas que no estan canceladas.
    * Cada reserva guarda su monto sin IVA, por eso se multiplica por (1 + tasaIVA), o sea 1.13.
    * Entrada: las reservas. Salida: el monto total con impuestos.
    -}
    montoRecaudado :: [Reserva] -> Double
    montoRecaudado reservas =
        sum [ montoReserva reserva * (1 + tasaIVA) | reserva <- reservasVigentes reservas ]

    {-
    * Imprime el monto recaudado con impuestos (ignora las reservas canceladas).
    * Entradas: ninguna (lee las reservas del archivo). Salida: IO ().
    -}
    mostrarMontoRecaudado :: IO ()
    mostrarMontoRecaudado = do
        reservas <- cargarReservas
        putStrLn "\n================ Monto recaudado ================"
        putStrLn ("  Total con impuestos (sin reservas canceladas): " ++ printf "%.2f" (montoRecaudado reservas))
        putStrLn "==============================================="

    {-
    * Numero del dia de la semana de una fecha (0 = domingo ... 6 = sabado).
    * Entrada: la fecha. 
    *Salida: el numero; 0 si la fecha no existe en el calendario.
    -}
    diaSemana :: Fecha -> Int
    diaSemana (anio, mes, dia) =
        case fromGregorianValid anio mes dia of
            Just fecha -> fromEnum (dayOfWeek fecha) `mod` 7
            Nothing -> 0

    {-
    * Nombre del dia de la semana a partir de su numero.
    * Entrada: numero de 0 (domingo) a 6 (sabado). 
    *Salida: el nombre.
    -}
    nombreDiaSemana :: Int -> String
    nombreDiaSemana numero =
        ["Domingo", "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado"] !! (numero `mod` 7)

    {-
    * Noches de una estadia: desde la entrada hasta la salida sin incluir el dia de salida.
    * Entradas: fecha de entrada y de salida. 
    *Salida: lista de fechas (vacia si alguna no existe o salida <= entrada).
    -}
    nochesEntre :: Fecha -> Fecha -> [Fecha]
    nochesEntre (anioE, mesE, diaE) (anioS, mesS, diaS) =
        case (fromGregorianValid anioE mesE diaE, fromGregorianValid anioS mesS diaS) of
            (Just entrada, Just salida) -> map toGregorian [entrada .. addDays (-1) salida]
            _ -> []

    {-
    * Los 3 dias de la semana con mas habitaciones ocupadas.
    * Cada habitacion ocupada en una noche cuenta 1 para el dia de la semana de esa noche.
    * Entrada: las reservas. 
    *Salida: hasta 3 pares (nombre del dia, cantidad), de mayor a menor.
    * Solo se muestran dias con al menos una ocupacion. En empate se respeta el orden domingo a sabado.
    -}
    top3DiasSemana :: [Reserva] -> [(String, Int)]
    top3DiasSemana reservas =
        take 3 (filter ((> 0) . snd) (sortBy (comparing (negate . snd)) conteos))
        where
            -- Un elemento (el numero del dia de la semana) por cada habitacion ocupada en cada noche
            nochesOcupadas = [ diaSemana noche
                             | reserva <- reservasVigentes reservas
                             , Just (entrada, salida) <- [periodoDeReserva reserva]
                             , noche <- nochesEntre entrada salida
                             , _ <- detalleReserva reserva ]
            -- Cuenta cuantas veces aparece cada dia (0 = domingo ... 6 = sabado)
            conteos = [ (nombreDiaSemana dia, length (filter (== dia) nochesOcupadas)) | dia <- [0 .. 6] ]

    {-
    * Imprime el top 3 de dias de la semana con mas habitaciones ocupadas.
    * Entradas: ninguna (lee las reservas del archivo). Salida: IO ().
    -}
    mostrarTop3DiasSemana :: IO ()
    mostrarTop3DiasSemana = do
        reservas <- cargarReservas
        let top = top3DiasSemana reservas
        putStrLn "\n================ Top 3 dias con mas ocupacion ================"
        if null top
            then putStrLn "  (no hay ocupaciones)"
            else mapM_ (\(posicion, (dia, cantidad)) -> putStrLn ("  " ++ show posicion ++ ". " ++ dia ++ " - " ++ show cantidad ++ " habitaciones")) (zip [1 :: Int ..] top)
        putStrLn "=============================================================="
               