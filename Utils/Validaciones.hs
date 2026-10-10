-- Archivo donde se encuentran las funciones de validación de datos para reuutilizar en los servicios de la aplicación
module Utils.Validaciones where
    {-
    * Función que separa una cadena de texto en una lista de cadenas, utilizando la coma como delimitador
    * @param cadena: la cadena de texto que se desea separar
    * @return: una lista de cadenas de texto, donde cada elemento es una parte de la cadena original separada por comas
    -}
    separarPorComa :: String -> [String]
    separarPorComa [] = [""]
    -- Cuando encuentra una coma, inicia una nueva parte
    separarPorComa (',' : restoCadena) = 
        "" : separarPorComa restoCadena
-- Agrega el carácter actual a la parte que se está construyendo
    separarPorComa (caracterActual : restoCadena) = 
        case separarPorComa restoCadena of
            [] -> 
                [[caracterActual]] -- Si no hay más partes, crea una nueva parte con el carácter actual
            (primeraParte : partesRestantes) -> (caracterActual : primeraParte) : partesRestantes -- Agrega el carácter actual a la primera parte y mantiene las demás partes sin cambios
        
    {-
    * Verifica que una línea
    * - Debe tener exactamente 3 campos.
    * - Ningún campo puede estar vacío.
    * - maximoPersonas debe ser numérico.
    * - maximoPersonas debe ser mayor a 0.
    
    *@param linea: la línea de texto que se desea verificar
    *@return: True si la línea cumple con el formato esperado, False en caso contrario
    -}
    validarFormatoTipoHabitacion :: String -> Bool
    validarFormatoTipoHabitacion linea =
        let campos = separarPorComa linea
        in length campos == 3 && camposCompletos campos && esNumeroEnteroPositivo (campos !! 2) && read (campos !! 2) > 0 -- Verifica que los campos sean 3 y que el tercer campo sea un número entero positivo mayor a 0

    {-
    * Verifica que una cadena de texto represente un número entero positivo
    * @param texto: la cadena de texto que se desea verificar
    * @return: True si la cadena representa un número entero positivo, False en caso contrario
    -}
    esNumeroEnteroPositivo :: String -> Bool
    esNumeroEnteroPositivo texto =
        not (null texto) && all (`elem` ['0'..'9']) texto

    {-
    * Verificar que ningun campo de la cadena de texto este vacio
    * @param cadena: la cadena de texto que se desea verificar
    * @return: True si ningun campo esta vacio, False en caso contrario
    -}
    camposCompletos :: [String] -> Bool
    camposCompletos campos = all (not . null) campos

