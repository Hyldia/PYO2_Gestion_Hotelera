module Services.TipoHabitacionService where
    import Models.TipoHabitacion
    import Utils.Archivo
    import Utils.Validaciones
        {-
    * Convierte una linea del archivo en un objeto de tipo TipoHabitacion
    * @param linea: una linea del archivo de texto que representa un tipo de habitación
    * @return: un objeto de tipo TipoHabitacion con los atributos correspondientes a la linea del archivo
    -}
    convertirTipoHabitacion :: String -> TipoHabitacion
    convertirTipoHabitacion linea
        | not (validarFormatoTipoHabitacion linea) = 
            error ("Formato de línea inválido: " ++ linea ++ ". Se esperaba el formato: nombre,descripcion,maximoPersonas (con maximoPersonas mayor a 0)")
        | otherwise =
            let campos = separarPorComa linea
            in TipoHabitacion {nombreTipo = campos !! 0, descripcionTipo = campos !! 1, maximoPersonas = read(campos !! 2)} -- Lee los campos de la linea y los asigna a los atributos del objeto TipoHabitacion

    {-
    * Convierte todas las lineas del archivo en una lista de objetos de tipo TipoHabitacion
    * @param contenido: una lista de lineas del archivo de texto que representan tipos de habitación
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    convertirContenidoATiposHabitacion :: String -> [TipoHabitacion]
    convertirContenidoATiposHabitacion contenido = map convertirTipoHabitacion (lines contenido) -- Aplica la función convertirTipoHabitacion a cada linea del archivo y devuelve una lista de objetos TipoHabitacion
    -- lines: divide una cadena de texto (String) en una lista de cadenas ([String]) usando  el salto de linea como separadores

    {-
    * Lee el archivo indicado por el usuario
    * @param ruta: la ruta del archivo de texto que contiene los tipos de habitación
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    cargarTiposDesdeRuta :: FilePath -> IO [TipoHabitacion]
    cargarTiposDesdeRuta ruta = do
        contenido <- leerArchivo ruta
        return (convertirContenidoATiposHabitacion contenido) -- Lee el archivo y convierte las lineas en una lista de objetos TipoHabitacion

    {-
    * Guarda el contenido cargado dentro de la base de datos
    * @param contenido: el contenido que se desea guardar en el archivo de texto de la base de datos
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    guardarTiposHabitacion :: String -> IO ()
    guardarTiposHabitacion contenido = 
        escribirArchivo "database/tiposHabitacion.txt" contenido -- Guarda el contenido en el archivo de texto de la base de datos

    {-
    * Devuelve todos los tipos almacenados en la base de datos
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    obtenerTiposHabitacion :: IO [TipoHabitacion]
    obtenerTiposHabitacion = do
        contenido <- leerArchivo "database/tiposHabitacion.txt"
        return (convertirContenidoATiposHabitacion contenido) -- Lee el archivo de la base de datos y convierte las lineas en una lista de objetos TipoHabitacion
