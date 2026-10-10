{-
* Representa una de las 8 tarifas que se pueden aplicar a una habitación del hotel.
* codigoTarifa: Identificador único de la tarifa.
* montoTarifa: Precio por persona.
-}
module Models.Tarifa where
    data Tarifa = Tarifa {
        codigoTarifa :: Int,
        montoTarifa :: Double
    }
    deriving (Show, Read, Eq)