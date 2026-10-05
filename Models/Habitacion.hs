{-
* Representa una habitación física del hotel.
* idHabitacion: Identificador que se genera automáticamente.
* tipoHabitacion: Nombre del tipo al que pertenece.
-}
module Models.Habitacion where
    data Habitacion = Habitacion {
        idHabitacion :: String,
        tipoHabitacion :: String
    }
    deriving (Show, Read, Eq)

