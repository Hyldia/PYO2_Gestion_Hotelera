{-
* Cálculo del monto de una reserva
* El monto se calcula por noche y por persona, según:
*   - Temporada: Verde (abril a octubre) o Alta (noviembre a marzo).
*   - Tipo de día: entre semana (domingo a jueves) o fin de semana (viernes y sábado).
*   - Tipo de huésped: adulto o niño.
* La noche del día de salida no se cobra. El monto es sin IVA.
-}
module Services.MontoService where
    import Data.List (find)              -- find: busca la tarifa con el código pedido
    import Models.Tarifa
    import Services.EstadisticasService (Fecha, diaSemana, nochesEntre)   -- funciones de fechas de Integrante 3

    {-
    * Tipo de huésped: decide si se usa la tarifa de adulto o la de niño.
    * Haskell: tipo suma con dos constructores (igual que EstadoReserva).
    -}
    data TipoHuesped = Adulto | Nino
        deriving (Show, Eq)

    {-
    * Indica si una fecha está en temporada Verde (abril a octubre).
    * Entrada: la fecha (año, mes, día). Salida: True si es Verde, False si es Alta.
    * Haskell: el patrón (_, mes, _) toma solo el mes de la tupla; "_" ignora lo demás.
    -}
    esTemporadaVerde :: Fecha -> Bool
    esTemporadaVerde (_, mes, _) = mes >= 4 && mes <= 10

    {-
    * Indica si una noche es de fin de semana (viernes o sábado).
    * diaSemana (de Integrante 3) da 0 = domingo ... 5 = viernes, 6 = sábado.
    -}
    esFinDeSemana :: Fecha -> Bool
    esFinDeSemana noche = diaSemana noche >= 5

    {-
    * Código de la tarifa (1 al 8) que aplica a una noche y un tipo de huésped.
    * Entradas: la noche y el tipo de huésped. Salida: el código.
    * Se arma sumando: base 1 si es Verde o 5 si es Alta,
    *                  +2 si es fin de semana, +1 si es niño.
    * Ej: sábado de noviembre, adulto = 5 + 2 + 0 = 7 (Alta, fin de semana, adulto).
    -}
    codigoTarifaPara :: Fecha -> TipoHuesped -> Int
    codigoTarifaPara noche huesped = base + extraFinDeSemana + extraNino
        where
            base = if esTemporadaVerde noche then 1 else 5
            extraFinDeSemana = if esFinDeSemana noche then 2 else 0
            extraNino = if huesped == Nino then 1 else 0

    {-
    * Busca el monto por persona de una tarifa.
    * Entradas: el código y las tarifas cargadas Salida: el monto
    * Restricción: antes de reservar se verifica que estén las 8 tarifas, si aun así faltara una, devuelve 0 para no detener el programa
    -}
    montoDeTarifa :: Int -> [Tarifa] -> Double
    montoDeTarifa codigo tarifas =
        case find (\tarifa -> codigoTarifa tarifa == codigo) tarifas of
            Just tarifa -> montoTarifa tarifa
            Nothing -> 0

    {-
    * Monto de UNA noche para todos los huéspedes
    * Entradas: tarifas, cantidad de adultos, cantidad de niños y la noche
    * Salida: adultos * tarifa de adulto + niños * tarifa de niño
    * Haskell: fromIntegral convierte Int a Double; Haskell no mezcla
    *          tipos numéricos automáticamente.
    -}
    montoPorNoche :: [Tarifa] -> Int -> Int -> Fecha -> Double
    montoPorNoche tarifas adultos ninos noche =
        fromIntegral adultos * montoDeTarifa (codigoTarifaPara noche Adulto) tarifas
        + fromIntegral ninos * montoDeTarifa (codigoTarifaPara noche Nino) tarifas

    {-
    * Monto total de la estadía, sin IVA
    * Entradas: tarifas, fecha de entrada, fecha de salida, adultos y niños.
    * Salida: la suma del monto de cada noche (la noche de salida no cuenta)
    -}
    calcularMonto :: [Tarifa] -> Fecha -> Fecha -> Int -> Int -> Double
    calcularMonto tarifas entrada salida adultos ninos =
        sum (map (montoPorNoche tarifas adultos ninos) (nochesEntre entrada salida))