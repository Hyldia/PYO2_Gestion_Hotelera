{-
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
    import Data.List (nubBy, sortOn)     -- nubBy: quita repetidos según un criterio; sortOn: ordena según un campo
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
                let lineas = filter (not . null) (map limpiarLinea (lines contenido))
                let invalidas = filter (not . lineaTarifaValida) lineas
                let nuevas = tarifasDesdeTexto contenido
                mapM_ (\linea -> putStrLn ("  Linea ignorada por formato invalido: " ++ linea)) invalidas
                if null nuevas
                    then putStrLn "No se encontraron tarifas validas en el archivo."
                    else do
                        actuales <- obtenerTarifas
                        let todas = combinarTarifas actuales nuevas
                        guardarTarifas todas
                        putStrLn "\nTarifas cargadas correctamente:"
                        mostrarTarifas todas
                        let faltantes = codigosFaltantes todas
                        if null faltantes
                            then return ()
                            else putStrLn ("Atencion: faltan las tarifas " ++ show faltantes)