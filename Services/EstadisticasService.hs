module Services.EstadisticasService where
    import Data.List (sortOn, groupBy) -- sortOn se usa como ordenamiento, se usa para poner los meses iguales juntos y groupBy para agruparlos
    import Data.Function (on) -- on compara usando solo una parte en especcifico de los datos
    import Data.Time (Day, defaultTimeLocale, parseTimeM, toGregorian)
    import Models.Reserva --se usa para acceder a los datos de las reservas
    import Models.EstadoReserva --se usa para acceder al estado de las reservas
    import Services.ReservaService (obtenerReservas, rutaReservas) --se usa para obtener las reservas y la ruta del archivo
    import Utils.Archivo (archivoExiste) --se usa para verificar si el archivo de reservas existe

    parsearFecha :: String -> Maybe (Integer, Int, Int)
    parsearFecha fecha = do
        fechaParseada <- parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha :: Maybe Day
        let (anio, mes, dia) = toGregorian fechaParseada
        return (anio, mes, dia)

    {-
    * Deja solo las reservas que cuentan (Activas y Facturadas); las canceladas se ignoran
    * Entrada: lista de reservas.
      Salida: lista filtrada.
    -}
    reservasVigentes :: [Reserva] -> [Reserva]
    reservasVigentes = filter (\reserva -> estadoReserva reserva /= Cancelada)

    {-
    * Total de huespedes (adultos + ninos) agrupado por mes y año de la fecha de entrada.
    * Entrada: las reservas. 
      Salida: lista filtrada y ordenada por año y mes.
    * Se ignoran las reservas canceladas y las que tengan una fecha de entrada invalida.
    -}
    huespedesPorMes :: [Reserva] -> [((Integer, Int), Int)]
    huespedesPorMes reservas =
        map sumarGrupo (groupBy ((==) `on` fst) (sortOn fst pares))
        where
            pares = [ ((anio, mes), adultosTotales reserva + ninosTotales reserva)
                    | reserva <- reservasVigentes reservas
                    , Just (anio, mes, _) <- [parsearFecha (fechaEntrada reserva)] ]
            sumarGrupo grupo = (fst (head grupo), sum (map snd grupo))


    {-
    * Lee las reservas guardadas; si el archivo todavia no existe devuelve lista vacia en lugar de fallar.
    * Entradas: ninguna. Salida: IO [Reserva].
    -}
    cargarReservas :: IO [Reserva]
    cargarReservas = do
        existe <- archivoExiste rutaReservas
        if existe then obtenerReservas else return []

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
