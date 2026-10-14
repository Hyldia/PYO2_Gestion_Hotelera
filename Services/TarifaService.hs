{-
* Solo se acepta un archivo COMPLETO y CORRECTO: las 8 tarifas, sin errores. Si no, no se carga nada
* Servicio de tarifas
* Pide al usuario la ruta de un archivo con tarifas, lo carga y guarda las
* tarifas en database/tarifas.txt para usarlas al calcular el monto de las reservas.
* Formato de cada línea: codigo,monto
*   1: Verde entre semana adulto     2: Verde entre semana niño
*   3: Verde fin de semana adulto    4: Verde fin de semana niño
*   5: Alta entre semana adulto      6: Alta entre semana niño
*   7: Alta fin de semana adulto     8: Alta fin de semana niño
* Verde = abril a octubre. Alta = noviembre a marzo.
* Entre semana = domingo a jueves. Fin de semana = viernes y sábado.
-}
module Services.TarifaService where
    import Data.List (nub, nubBy, sortOn)   -- nub: quita repetidos; nubBy: quita repetidos según un criterio; sortOn: ordena según un campo
    import Data.Char (isSpace)           -- isSpace: detecta espacios (y el \r de archivos de Windows)
    import Text.Printf (printf)          -- printf: muestra montos con 2 decimales
    import Models.Tarifa
    import Utils.Archivo (leerArchivo, escribirArchivo, archivoExiste)
    import Utils.Validaciones (separarPorComa, esNumeroEnteroPositivo)

    -- Ruta del archivo donde el sistema guarda las tarifas cargadas
    rutaTarifas :: FilePath
    rutaTarifas = "database/tarifas.txt"

    {-
    * Quita todos los espacios de una línea (incluye el \r de los archivos de Windows).
    * Entrada: la línea Salida: la línea sin espacios. Así "7, 109" también se acepta
    -}
    limpiarLinea :: String -> String
    limpiarLinea linea = filter (not . isSpace) linea

    {-
    * Verifica que un texto sea un monto válido: solo dígitos, con decimales opcionales
    * Entrada: el texto  Salida: True si es válido
    * 
    -}
    esMontoValido :: String -> Bool
    esMontoValido texto =
        case break (== '.') texto of
            (entera, "") -> esNumeroEnteroPositivo entera
            (entera, _ : decimales) -> esNumeroEnteroPositivo entera && esNumeroEnteroPositivo decimales

    {-
    * Verifica que una línea tenga el formato codigo,monto con código del 1 al 8.
    * Entrada: la línea ya limpia. Salida: True si es válida.
    -}
    lineaTarifaValida :: String -> Bool
    lineaTarifaValida linea =
        case separarPorComa linea of
            [codigo, monto] -> codigo `elem` codigosValidos && esMontoValido monto
            _ -> False
        where
            codigosValidos = map show [1 .. 8 :: Int]

    {-
    * Convierte una línea válida "codigo,monto" en una Tarifa.
    * Restricción: la línea ya fue validada con lineaTarifaValida.
    -}
    convertirTarifa :: String -> Tarifa
    convertirTarifa linea =
        let campos = separarPorComa linea
        in Tarifa { codigoTarifa = read (campos !! 0), montoTarifa = read (campos !! 1) }

    {-
    * Convierte el contenido de un archivo en la lista de tarifas válidas.
    * Ignora líneas vacías o inválidas. Si un código se repite, se queda con el primero 
    -}
    tarifasDesdeTexto :: String -> [Tarifa]
    tarifasDesdeTexto contenido =
        nubBy (\a b -> codigoTarifa a == codigoTarifa b)
              (map convertirTarifa (filter lineaTarifaValida (map limpiarLinea (lines contenido))))

    {-
    * Lee las tarifas guardadas en el sistema. Las usa el cálculo del monto de la reserva.
    * Salida: IO [Tarifa] (vacía si todavía no se han cargado tarifas).
    -}
    obtenerTarifas :: IO [Tarifa]
    obtenerTarifas = do
        existe <- archivoExiste rutaTarifas
        if existe
            then do
                contenido <- leerArchivo rutaTarifas
                return (tarifasDesdeTexto contenido)
            else return []

    -- Convierte una tarifa en la línea "codigo,monto" para guardarla
    tarifaATexto :: Tarifa -> String
    tarifaATexto tarifa = show (codigoTarifa tarifa) ++ "," ++ show (montoTarifa tarifa)

    {-
    * Guarda las tarifas en el sistema, ordenadas por código. Reemplaza todo el archivo.
    -}
    guardarTarifas :: [Tarifa] -> IO ()
    guardarTarifas tarifas =
        escribirArchivo rutaTarifas (unlines (map tarifaATexto (sortOn codigoTarifa tarifas)))

    {-
    * Junta la tarifas que ya estaban con las nuevas del archivo.
    * Si un código viene en el archivo nuevo, su monto REEMPLAZA al anterior (lo pide el enunciado)
    * Los códigos que no vienen se mantienen, así no se pierde nada y no hay repetidos
    -}
    combinarTarifas :: [Tarifa] -> [Tarifa] -> [Tarifa]
    combinarTarifas actuales nuevas =
        nuevas ++ filter (\tarifa -> codigoTarifa tarifa `notElem` codigosNuevos) actuales
        where
            codigosNuevos = map codigoTarifa nuevas

    -- Códigos del 1 al 8 que todavía no tienen tarifa
    codigosFaltantes :: [Tarifa] -> [Int]
    codigosFaltantes tarifas = filter (\codigo -> codigo `notElem` map codigoTarifa tarifas) [1 .. 8]

    -- Nombre de cada tarifa según su código
    nombreTarifa :: Int -> String
    nombreTarifa codigo =
        case codigo of
            1 -> "Verde, entre semana, adulto"
            2 -> "Verde, entre semana, nino"
            3 -> "Verde, fin de semana, adulto"
            4 -> "Verde, fin de semana, nino"
            5 -> "Alta, entre semana, adulto"
            6 -> "Alta, entre semana, nino"
            7 -> "Alta, fin de semana, adulto"
            8 -> "Alta, fin de semana, nino"
            _ -> "Desconocida"

    -- Muestra las tarifas en pantalla, ordenadas por código
    mostrarTarifas :: [Tarifa] -> IO ()
    mostrarTarifas tarifas =
        mapM_ mostrarUna (sortOn codigoTarifa tarifas)
        where
            mostrarUna tarifa =
                putStrLn ("  " ++ show (codigoTarifa tarifa) ++ ". " ++ nombreTarifa (codigoTarifa tarifa)
                          ++ ": " ++ printf "%.2f" (montoTarifa tarifa))

    {-
    * Valida que el archivo de tarifas esté COMPLETO y CORRECTO.
    * Entrada: el contenido del archivo.
    * Salida: Right con las 8 tarifas si todo está bien, o Left con la lista de errores encontrados.
    * Reglas:
    *   - El archivo no puede estar vacío.
    *   - Todas las líneas deben tener el formato codigo,monto (código del 1 al 8).
    *   - Cada código del 1 al 8 debe venir exactamente una vez.
    * Las líneas vacías se ignoran.
    * Haskell: Either permite devolver TODOS los errores (Left) o el resultado bueno (Right).
    -}
    validarArchivoTarifas :: String -> Either [String] [Tarifa]
    validarArchivoTarifas contenido
        | null lineas = Left ["El archivo esta vacio, no tiene tarifas."]
        | not (null errores) = Left errores
        | otherwise = Right (map convertirTarifa lineas)
        where
            lineas = filter (not . null) (map limpiarLinea (lines contenido))
            invalidas = filter (not . lineaTarifaValida) lineas
            codigos = map (codigoTarifa . convertirTarifa) (filter lineaTarifaValida lineas)
            repetidos = nub (filter (\codigo -> length (filter (== codigo) codigos) > 1) codigos)
            faltantes = filter (`notElem` codigos) [1 .. 8]
            errores = map (\linea -> "Linea con formato invalido: " ++ linea) invalidas
                    ++ (if null repetidos then [] else ["Codigos repetidos: " ++ show repetidos])
                    ++ (if null faltantes then [] else ["Faltan las tarifas: " ++ show faltantes])


    {-
    * Opción "Cargar tarifas" del menú administrativo
    * Pide la ruta de un archivo, lo lee, valida sus líneas y guarda las tarifas.
    * Entradas: ninguna (lee del teclado). Salida: IO ().
    * Restricciones: el archivo debe existir; las líneas inválidas se ignoran y se avisan.
    -}
    opcionCargarTarifas :: IO ()
    opcionCargarTarifas = do
        putStrLn "Ingrese la ruta del archivo de tarifas:"
        ruta <- getLine
        existe <- archivoExiste ruta
        if not existe
            then putStrLn "Error: el archivo no existe."
            else do
                contenido <- leerArchivo ruta
                case validarArchivoTarifas contenido of
                    Left errores -> do
                        putStrLn "\nNo se cargaron las tarifas. El archivo debe traer las 8 tarifas correctas:"
                        mapM_ (\error -> putStrLn ("  - " ++ error)) errores
                        actuales <- obtenerTarifas
                        if null actuales
                            then putStrLn "Todavia no hay tarifas cargadas en el sistema."
                            else putStrLn "Se mantienen las tarifas que ya estaban cargadas."
                    Right tarifas -> do
                        guardarTarifas tarifas
                        putStrLn "\nTarifas cargadas correctamente:"
                        mostrarTarifas tarifas