{-
* Representa los posibles estados de una reserva.
*
* Activa: Reserva creada y pendiente de facturación.
* Facturada: Reserva ya facturada.
* Cancelada: Reserva anulada por el usuario.
-}

module Models.EstadoReserva where -- Se define el modulo de estado reserva
data EstadoReserva
    = Activa
    | Facturada
    | Cancelada
    deriving (Show, Read, Eq) -- Se derivan las instancias de Show, Read y Eq para poder mostrar, leer y comparar los estados de reserva