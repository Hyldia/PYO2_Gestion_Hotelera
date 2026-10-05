{-
* Información general del hotel.
* Esta información se usa al generar las facturas.
-}
module Models.Hotel where
    data Hotel = Hotel {
        nombreEmpresa :: String,
        cedulaJuridica :: String,
        sitioWeb :: String,
        telefono :: String,
        pais :: String,
        provincia :: String
    }
    deriving (Show, Read, Eq)