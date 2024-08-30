import System.IO ()

-- Definición del tipo de datos para representar la información del artículo
data Articulo = Articulo
  { nombre :: String,
    categoria :: String
  }
  deriving (Show, Read)

-- Función para registrar entrada de Artículo
registrarArticulo :: Articulo -> [Articulo] -> [Articulo]
registrarArticulo articulo inventario = articulo : inventario

-- Función para buscar artículos por categoría
buscarPorCategoria :: String -> [Articulo] -> [Articulo]
buscarPorCategoria catego inventario = filter (\art -> categoria art == catego) inventario

-- Función para mostrar un artículo
mostrarArticulos :: Articulo -> String
mostrarArticulos (Articulo nombreArticulo categoriaArticulo) =
  "Articulo {nombre = \"" ++ nombreArticulo ++ "\", categoria = \"" ++ categoriaArticulo ++ "\"}"

-- Función para mostrar la lista de artículos
mostrarInventario :: [Articulo] -> String
mostrarInventario inventario = unlines (map mostrarArticulos inventario)

-- Función para cargar el archivo de texto
cargarInventario :: IO [Articulo]
cargarInventario = do
  contenido <- readFile "inventario.txt"
  let lineas = lines contenido
      inventario = map leerArticulo lineas
  length inventario `seq` return inventario 
  where
    leerArticulo linea = read linea :: Articulo
    
-- Función para guardar el inventario en el archivo de texto
guardarInventario :: [Articulo] -> IO ()
guardarInventario inventario = do
  let contenido = unlines (map show inventario)
  writeFile "inventario.txt" contenido

-- Función principal
main :: IO ()
main = do
  inventario <- cargarInventario
  putStrLn "Bienvenido al sistema de Gestión de Inventario"
  nuevoInventario <- cicloPrincipal inventario
  guardarInventario nuevoInventario

-- Función para el ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO [Articulo]
cicloPrincipal inventario = do
  putStrLn "\nSeleccione una opción:"
  putStrLn "1. Registrar entrada de artículo"
  putStrLn "2. Buscar artículos por categoría"
  putStrLn "3. Listar todos los artículos"
  putStrLn "4. Mostrar cantidad de artículos por categoría"
  putStrLn "5. Salir"

  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Ingrese el nombre del artículo: "
      nombreArt <- getLine
      putStrLn "Ingrese la categoría del artículo: "
      categoriaArt <- getLine
      let articulo = Articulo nombreArt categoriaArt
      let nuevoInventario = registrarArticulo articulo inventario
      guardarInventario nuevoInventario
      putStrLn "Artículo registrado exitosamente."
      cicloPrincipal nuevoInventario
    "2" -> do
      putStrLn "Ingrese la categoría para buscar artículos: "
      categoriaBuscada <- getLine
      let resultados = buscarPorCategoria categoriaBuscada inventario
      putStrLn $ mostrarInventario resultados
      cicloPrincipal inventario
    "3" -> do
      putStrLn $ mostrarInventario inventario
      cicloPrincipal inventario
    "4" -> do
      putStrLn "Ingrese la categoría para saber la cantidad de artículos: "
      categoriaBuscada <- getLine
      let cantidad = length (buscarPorCategoria categoriaBuscada inventario)
      putStrLn $ categoriaBuscada ++ ": " ++ show cantidad
      cicloPrincipal inventario
    "5" -> do
      putStrLn "Saliendo del sistema."
      return inventario
    _ -> do
      putStrLn "Opción no válida. Por favor, intente de nuevo."
      cicloPrincipal inventario