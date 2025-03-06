import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    identificacion :: String,
    ingreso :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar el ingreso del estudiante
registrarIngreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso idEstudiante tiempo listaEstudiantes =
    Estudiante idEstudiante tiempo Nothing : listaEstudiantes

-- Función para registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEstudiante tiempo =
    map (\e -> if identificacion e == idEstudiante then e { salida = Just tiempo } else e)

-- Función para buscar un estudiante por su identificación
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEstudiante listaEstudiantes =
    find (\e -> identificacion e == idEstudiante) listaEstudiantes

-- Función para calcular el tiempo que un estudiante permaneció registrado
tiempoEnRegistro :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnRegistro estudiante tiempoActual =
    case salida estudiante of
        Just tiempoSalida -> diffUTCTime tiempoSalida (ingreso estudiante)
        Nothing           -> diffUTCTime tiempoActual (ingreso estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarRegistro :: [Estudiante] -> IO ()
guardarRegistro listaEstudiantes = do
    resultado <- reintentar 5 (writeFile "registro.txt" (unlines (map mostrarEstudiante listaEstudiantes)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el registro: " ++ show ex
        Right _ -> putStrLn "Registro guardado en el archivo registro.txt."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left _ -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarRegistro :: IO [Estudiante]
cargarRegistro = do
    resultado <- try (readFile "registro.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el registro: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante estudiante =
    identificacion estudiante ++ "," ++ show (ingreso estudiante) ++ "," ++ show (salida estudiante)

-- Función para cargar la información de los estudiantes desde un archivo de texto
leerRegistro :: IO [Estudiante]
leerRegistro = do
    contenido <- readFile "registro.txt"
    let lineas = lines contenido
    return (mapMaybe parsearEstudiante lineas)
    where
        parsearEstudiante :: String -> Maybe Estudiante
        parsearEstudiante linea = case words linea of
            [idEstudiante, ingreso, salida] -> Just $ Estudiante idEstudiante (read ingreso) (readMaybeSalida salida)
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salidaStr = Just (read salidaStr)
        


-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el registro desde el archivo de texto
    listaEstudiantes <- cargarRegistro
    putStrLn "¡Bienvenido al Sistema de Gestión de Registro de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal listaEstudiantes

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal listaEstudiantes = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar ingreso de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por identificación"
    putStrLn "4. Listar los estudiantes registrados"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let listaActualizada = registrarIngreso idEstudiante tiempoActual listaEstudiantes
            guardarRegistro listaActualizada
            cicloPrincipal listaActualizada
        "2" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let listaActualizada = registrarSalida idEstudiante tiempoActual listaEstudiantes
            guardarRegistro listaActualizada
            cicloPrincipal listaActualizada
        "3" -> do
            putStrLn "Ingrese la identificación del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            case buscarEstudiante idEstudiante listaEstudiantes of
                Just estudiante -> do
                    let tiempoTotal = tiempoEnRegistro estudiante tiempoActual
                    putStrLn $ "El estudiante con identificación " ++ idEstudiante ++ " está registrado."
                    putStrLn $ "Tiempo en registro: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en el registro."
            cicloPrincipal listaEstudiantes
        "4" -> do
            putStrLn "Mostrando Lista de estudiantes registrados"
            listaActualizada <- leerRegistro
            mapM_ (\e -> putStrLn $ "Identificación: " ++ identificacion e ++ ", Ingreso: " ++ show (ingreso e) ++ ", Salida: " ++ show (salida e)) listaActualizada
            cicloPrincipal listaActualizada
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal listaEstudiantes