{-
* Representa la factura generada a partir de una reserva activa.
* codigoFactura: Identificador único de la factura que se genera automaticamnente.
* subtotal: Monto total de la reserva sin IVA.
* IVA: 13% de los montos en lo que aplica el impuesto
* total: Monto total de la reserva con IVA incluido.
-}
module Models.Factura where
    data Factura = Factura {
    codigoFactura :: String,
    codigoReservaFacturada :: String,
    subtotalFactura :: Double,
    ivaFactura :: Double,
    totalFactura :: Double
    }
    deriving (Show, Read, Eq)