module Utils.Validaciones where
    {-
    * Función que separa una cadena de texto en una lista de cadenas, utilizando la coma como delimitador
    * @param cadena: la cadena de texto que se desea separar
    * @return: una lista de cadenas de texto, donde cada elemento es una parte de la cadena original separada por comas
    -}
    separarPorComa :: String -> [String]
    separarPorComa [] = [""]
    separarPorComa (',' : restoCadena) = -- Cuando encuentra una coma, inicia una nueva parte
        "" : separarPorComa restoCadena
    separarPorComa (caracterActual : restoCadena) =  -- Agrega el carácter actual a la parte que se está construyendo
        let partesProcesadas = separarPorComa restoCadena
        in (caracterActual : head partesProcesadas) : tail partesProcesadas

    {-
    * Verifica que una línea tenga exactamente
    * los 3 campos requeridos:
    * nombre, descripcion y maximoPersonas.
    -}
    validarFormatoTipoHabitacion :: String -> Bool
    validarFormatoTipoHabitacion linea =
        let campos = separarPorComa linea
        in length campos == 3 && esNumeroEnteroPositivo (campos !! 2) && read (campos !! 2) > 0 -- Verifica que los campos sean 3 y que el tercer campo sea un número entero positivo mayor a 0

    {-
    * Verifica que una cadena de texto represente un número entero positivo
    * @param texto: la cadena de texto que se desea verificar
    * @return: True si la cadena representa un número entero positivo, False en caso contrario
    -}
    esNumeroEnteroPositivo :: String -> Bool
    esNumeroEnteroPositivo texto =
        not (null texto) && all (`elem` ['0'..'9']) texto