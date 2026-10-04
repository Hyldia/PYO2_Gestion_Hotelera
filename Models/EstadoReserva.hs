{-
* Representa los posibles estados de una reserva.
*
* Activa: Reserva creada y pendiente de facturación.
* Facturada: Reserva ya facturada.
* Cancelada: Reserva anulada por el usuario.
-}

module EstadoReserva where
data EstadoReserva
    = Activa
    | Facturada
    | Cancelada
    deriving (Show, Read, Eq)