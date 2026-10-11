
module Services.HabitacionService where
    import Models.Habitacion
    import Models.TipoHabitacion
    import Services.TipoHabitacionService
    import Utils.Archivo
    import Utils.Validaciones

    {-
    * Genera el identificador de una habitacion a partir de un numero.
    * Entrada: un numero consecutivo.
    * Salida: el identificador con el formato H001, H010, H100, etc.
    -}
    generarId :: Int -> String
    generarId numero
        | numero < 10 =
            "H00" ++ show numero
        | numero < 100 =
            "H0" ++ show numero
        | otherwise =
            "H" ++ show numero

    {-
    * Convierte una habitacion a su representacion de texto para guardarla.
    * Entrada: una habitacion.
    * Salida: el identificador y el tipo de habitacion separados por una coma.
    -}
    habitacionATexto :: Habitacion -> String
    habitacionATexto habitacion = 
        idHabitacion habitacion ++ "," ++ tipoHabitacion habitacion -- Une el id y el tipo de habitacion separados por una coma.

    {-
    * Verifica si el archivo de habitaciones existe y contiene datos.
    * Entrada: ninguna.
    * Salida: devuelve True si ya hay habitaciones guardadas.
    -}
    habitacionesGeneradas :: IO Bool
    habitacionesGeneradas = 
        do
            existe <- archivoExiste "database/Habitaciones.txt" -- Comprueba si el archivo de habitaciones ya existe.
            if not existe
                then
                    return False
                else do
                    contenido <- leerArchivo "database/Habitaciones.txt" -- Lee el contenido del archivo existente.
                    return (not(null contenido)) -- Indica si el archivo contiene habitaciones.
                    
    {-
    * Genera habitaciones de un mismo tipo con identificadores consecutivos.
    * Entrada: el nombre del tipo, la cantidad de habitaciones y el numero inicial.
    * Salida: la lista de habitaciones generadas.
    -}
    generarHabitacionesTipo :: String -> Int -> Int -> [Habitacion]
    generarHabitacionesTipo nombreTipo cantidad numeroInicial = 
        [Habitacion{idHabitacion = generarId numero, tipoHabitacion = nombreTipo} -- Crea una habitacion con su id y tipo.
            | numero <- [numeroInicial .. numeroInicial + cantidad - 1] -- Genera consecutivamente la cantidad solicitada.
        ]

    {-
    * Guarda las habitaciones en el archivo de datos.
    * Entrada: la lista de habitaciones que se desea guardar.
    * Salida: escribe las habitaciones en el archivo.
    -}
    guardarHabitaciones :: [Habitacion] -> IO ()
    guardarHabitaciones habitaciones = escribirArchivo "database/habitaciones.txt" (unlines(map habitacionATexto habitaciones)) -- Guarda las habitaciones en el archivo.

    {-
    * Genera las habitaciones del hotel si aun no han sido creadas.
    * Entrada: ninguna; solicita al usuario las cantidades para cada tipo.
    * Salida: guarda las habitaciones o informa que ya existen.
    -}
    generarHabitacionesHotel :: IO ()
    generarHabitacionesHotel =
        do
            generadas <- habitacionesGeneradas -- Verifica si las habitaciones ya fueron generadas.
            if generadas
                then do
                    putStrLn "\nLas habitaciones ya fueron generadas."
                    putStrLn "Esta operacion solo puede realizarse una vez."
                    putStrLn "--------------------------------------------------"
                else do
                    tipos <- obtenerTiposHabitacion -- Obtiene los tipos de habitacion disponibles.
                    habitaciones <- solicitarCantidades tipos 1 -- Solicita cantidades y asigna ids consecutivos desde 1.
                    guardarHabitaciones habitaciones
                    putStrLn "Las habitaciones fueron generadas correctamente."

    {-
    * Solicita la cantidad de habitaciones para cada tipo y las genera.
    * Entrada: la lista de tipos de habitacion y el siguiente numero consecutivo.
    * Salida: devuelve todas las habitaciones generadas.
    -}
    solicitarCantidades :: [TipoHabitacion] -> Int -> IO [Habitacion]
    solicitarCantidades [] _ = -- Caso base: ya se solicitaron cantidades para todos los tipos.
        return [] -- No quedan habitaciones por agregar.
    solicitarCantidades (tipo : restantes) consecutivo = -- Procesa el tipo actual y luego los tipos restantes.
        do
            putStrLn("Ingrese la cantidad de habitaciones para " ++ nombreTipo tipo ++ ":")
            cantidadTexto <- getLine -- Lee la cantidad ingresada para este tipo de habitacion.
            if not (esNumeroEnteroPositivo cantidadTexto) -- Valida que la cantidad sea un entero positivo.
                then do
                    putStrLn "Debe ingresar un numero entero positivo."
                    solicitarCantidades (tipo : restantes) consecutivo
                else do
                    let cantidad = read cantidadTexto -- Convierte la cantidad validada a entero.
                    let habitacionesActuales = generarHabitacionesTipo(nombreTipo tipo) cantidad consecutivo -- Genera las habitaciones de este tipo.
                    habitacionesRestantes <- solicitarCantidades restantes (consecutivo + cantidad) -- Continua con el siguiente tipo usando el id consecutivo.
                    return (habitacionesActuales ++ habitacionesRestantes)
