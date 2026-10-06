{-
* Representa la ocupación de una habitación dentro de una reserva.
* idHabitacion: Identificador de la habitación asignada.
* adultosHabitacion: Cantidad de adultos hospedados.
* ninosHabitacion: Cantidad de niños hospedados.
-}
module Models.DetalleReserva where
    data DetalleReserva = DetalleReserva {
        idHabitacion :: String,
        tipoHabitacion :: String,
        adultosHabitacion :: Int,
        ninosHabitacion :: Int
    }
    deriving (Show, Read, Eq)