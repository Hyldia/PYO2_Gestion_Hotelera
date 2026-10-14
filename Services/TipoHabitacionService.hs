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
    convertirTipoHabitacion linea =
        let campos = separarPorComa linea
        in TipoHabitacion {nombreTipo = campos !! 0, descripcionTipo = campos !! 1, maximoPersonas = read(campos !! 2)} -- Lee los campos de la linea y los asigna a los atributos del objeto TipoHabitacion

    {-
    * Convierte las validas lineas del archivo en una lista de objetos de tipo TipoHabitacion
    * @param contenido: una lista de lineas del archivo de texto que representan tipos de habitación
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    convertirContenidoATiposHabitacion :: String -> [TipoHabitacion]
    convertirContenidoATiposHabitacion contenido = 
        map convertirTipoHabitacion lineasValidas -- Convierte las lineas validas en objetos TipoHabitacion
        where
            -- Filtra las lineas validas del archivo de texto
            lineasValidas = filter validarFormatoTipoHabitacion (filter(not . null) (lines contenido))

    {-
    * Lee el archivo indicado por el usuario
    * @param ruta: la ruta del archivo de texto que contiene los tipos de habitación
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    cargarTiposDesdeRuta :: FilePath -> IO [TipoHabitacion]
    cargarTiposDesdeRuta ruta = 
        do
            existe <- archivoExiste ruta -- Verifica si el archivo existe en la ruta especificada
            if not existe
                then do
                    putStrLn "Error: el archivo no existe."
                    return []
                else do
                    contenido <- leerArchivo ruta -- Lee el contenido del archivo de text
                    if null contenido
                        then do
                            putStrLn "Error: el archivo está vacío."
                            return []
                        else do
                            mostrarAdvertencias contenido -- Muestra las lineas invalidas encontradas durante la carga del archivo
                            return (convertirContenidoATiposHabitacion contenido) -- Lee el archivo y convierte las lineas en una lista de objetos TipoHabitacion

    {-
    * Guarda los tipos de habitación unicos en la base de datos, ignorando los duplicados
    * @param nuevosTipos: la lista de tipos de habitacion que se desea guardar en la base de datos
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    guardarTiposHabitacion ::  [TipoHabitacion] -> IO ()
    guardarTiposHabitacion nuevosTipos = 
        do
            tiposActuales <- obtenerTiposHabitacion
            let tiposNoRepetidos = filter(\tipo -> not (existeTipoHabitacion(nombreTipo tipo)tiposActuales)) nuevosTipos -- Filtra los tipos de habitacion que no estan registrados en la base de datos
            if null tiposNoRepetidos
                then
                    putStrLn "No se pueden agregar tipos de habitación duplicados." -- No hay tipos de habitacion nuevos, no se muestra ningún mensaje
                else
                    agregarArchivo "database/tiposHabitacion.txt" (unlines [nombreTipo tipo ++ "," ++ descripcionTipo tipo ++ "," ++ show (maximoPersonas tipo) | tipo <- tiposNoRepetidos]) -- Convierte los objetos TipoHabitacion en lineas de texto y las agrega al final del archivo de la base de datos
                    
    {-
    * Devuelve todos los tipos almacenados en la base de datos
    * @return: una lista de objetos de tipo TipoHabitacion con los atributos correspondientes a cada linea del archivo
    -}
    obtenerTiposHabitacion :: IO [TipoHabitacion]
    obtenerTiposHabitacion = do
        contenido <- leerArchivo "database/tiposHabitacion.txt"
        return (convertirContenidoATiposHabitacion contenido) -- Lee el archivo de la base de datos y convierte las lineas en una lista de objetos TipoHabitacion

    {-
    * Obtiene todas las líneas que no cumplen con el formato requerido.
    -}
    obtenerLineasInvalidas :: String -> [String]
    obtenerLineasInvalidas contenido = 
        filter (not . validarFormatoTipoHabitacion) (filter(not . null) (lines contenido)) -- Filtra las lineas invalidas del archivo de texto

    {-
    * Muestra las líneas inválidas encontradas durante la carga del archivo.
    -}
    mostrarAdvertencias :: String -> IO ()
    mostrarAdvertencias contenido =
        do
            let lineasInvalidas = obtenerLineasInvalidas contenido
            if null lineasInvalidas
                then
                    return () -- No hay líneas inválidas, no se muestra ningún mensaje
                else do
                    putStrLn "\nADVERTENCIA:"
                    putStrLn "Las siguientes lineas fueron ignoradas por ser invalidas:"
                    mapM_ (\linea -> putStrLn ("- " ++ linea)) lineasInvalidas -- Muestra las lineas invalidas encontradas


    {-
    * Verificar si ya exite un tipo de habitacion con el mismo nombre
    * @param nombre: el nombre del tipo de habitacion que se desea verificar y lista de tipos de habitacion existentes
    * @return: True si ya existe un tipo de habitacion con el mismo nombre, False en caso contrario
    -}
    existeTipoHabitacion :: String -> [TipoHabitacion] -> Bool
    existeTipoHabitacion nombre tipos = 
        any (\tipo -> nombreTipo tipo == nombre) tipos -- Verifica si ya existe un tipo de habitacion con el mismo nombre

    {-
    * Mostrar los tipos de habitación dupplicados que se encontraron al cargar el archivo

    -}
    mostrarTiposDuplicados :: [TipoHabitacion] -> IO ()
    mostrarTiposDuplicados nuevosTipos = 
        do
            tiposActuales <- obtenerTiposHabitacion
            let tiposDuplicados = filter (\tipo -> existeTipoHabitacion (nombreTipo tipo) tiposActuales) nuevosTipos -- Filtra los tipos de habitacion que ya estan registrados en la base de datos
            if null tiposDuplicados
                then
                    return () -- No hay tipos duplicados, no se muestra ningún mensaje
            else do
                putStrLn "\nADVERTENCIA:"
                putStrLn "Los siguientes tipos de habitación ya existen y fueron ignorados:"
                mapM_ (\tipo -> putStrLn ("- " ++ nombreTipo tipo)) tiposDuplicados -- Muestra los tipos de habitacion duplicados encontrados
                putStrLn "---------------------------------------------------------------"