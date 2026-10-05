{-
* Representa un tipo de habitación del hotel y se carga desde un archivo de texto.
* nombreTipo: nombre único del tipo de habitación
* descripcionTipo: descripción del tipo de habitación
* maximoPersonas: número máximo de personas que se pueden alojarse en este tipo de habitación
-}
module Models.TipoHabitacion where
    data TipoHabitacion = TipoHabitacion {
        nombreTipo :: String,
        descripcionTipo :: String,
        maximoPersonas :: Int
    } deriving (Show, Read, Eq)