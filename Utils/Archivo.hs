-- Archivo reutilizable para leer, escribir y agregar contenido a archivos de texto
module Utils.Archivo where
    import System.Directory -- Para verificar si un archivo existe
    import System.IO -- Para leer y escribir archivos de texto

    -- Lee el contenido completo archivo
    leerArchivo :: FilePath -> IO String
    leerArchivo ruta =
        do
            manejoArchivo <- openFile ruta ReadMode -- Abre el archivo en modo lectura
            contenido <- hGetContents manejoArchivo -- Obtiene el contenido del archivo
            length contenido `seq` hClose manejoArchivo -- Cierra el archivo después de leerlo
            return contenido -- Devuelve el contenido del archivo como una cadena de texto

    -- Escribe contenido en un archivo y si ese contenido ya esxite lo remplaza
    escribirArchivo :: FilePath -> String -> IO ()
    escribirArchivo ruta contenido = writeFile ruta contenido

    -- Agregar contenido al final de un archivo
    agregarArchivo :: FilePath -> String -> IO ()
    agregarArchivo ruta contenido = appendFile ruta contenido

    -- Verifica si un archivo existe en la ruta especificada
    archivoExiste :: FilePath -> IO Bool
    archivoExiste ruta = doesFileExist ruta
    