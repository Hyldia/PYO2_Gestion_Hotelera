-- Archivo reutilizable para leer, escribir y agregar contenido a archivos de texto
module Utils.Archivo where
    -- Lee el contenido completo archivo
    leerArchivo :: FilePath -> IO String
    leerArchivo ruta = readFile ruta

    -- Escribe contenido en un archivo y si ese contenido ya esxite lo remplaza
    escribirArchivo :: FilePath -> String -> IO ()
    escribirArchivo ruta contenido = writeFile ruta contenido

    -- Agregar contenido al final de un archivo
    agregarArchivo :: FilePath -> String -> IO ()
    agregarArchivo ruta contenido = appendFile ruta contenido