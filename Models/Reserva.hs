{-
* Representa una reserva en el hotel.
* Estado de la reserva: 
    - Activa
    - Facturada
    - Cancelada
-}
module Reserva where
    import DetalleReserva
    import EstadoReserva
    data Reserva = Reserva {
        codigoReserva :: String,
        nombreCliente :: String, -- Nombre del cliente que realiza la reserva
        fechaReserva :: String,
        fechaEntrada :: String,
        fechaSalida :: String,
        adultosTotales :: Int,
        ninosTotales :: Int,
        estadoReserva :: EstadoReserva,
        montoReserva :: Double, -- Monto de la reserva sin IVA
        detalleReserva :: [DetalleReserva] --Lista de las habitaciones asociadas a la reserva
    }
    deriving (Show, Read, Eq)