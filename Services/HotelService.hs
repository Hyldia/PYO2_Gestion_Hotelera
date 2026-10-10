module Service.HotelService where
    import Models.Hotel
    import Utils.Archivo
    import Utils.Validaciones

    {-
    * Convierte una línea con los datos del hotel en un objeto Hotel.
    * @param Texto con los seis datos del hotel separados por comas.
    * @return: un objeto de tipo Hotel con la información de la línea.
    -}
    convertirHotel :: String -> Hotel
    convertirHotel linea = 
        let campos separarPorComa linea
        in Hotel {nombreHotel = campos !! 0, 
                    cedulaJuridica = campos !! 1, 
                    sitioweb = campos !! 2, 
                    telefono = campos !! 3,
                    pais = campos !! 4,
                    provincia = campos !! 5
                }

    {-
    * Obtiene la información del hotel almacenada en la base de datos.
    * @param: no recibe parámetros.
    * @return: un objeto de tipo Hotel con la información del hotel.
    -}
    obtenerHotel :: IO Hotel
    obtenerHotel = do
        contenido <- leerArchivo "database/hotel.txt"
        return (convertirHotel contenido)

    {-
    * Verifica que una línea tenga el formato válido para los datos de un hotel.
    * @param linea: texto con los datos del hotel separados por comas.
    * @return: True si contiene seis campos completos; False en caso contrario.
    -}
    validarFormatoHotel :: String -> Bool
    validarFormatoHotel linea =
        let campos = separarPorComa linea
        in length campos == 6 && camposCompletos campos