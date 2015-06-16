{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, toUpper)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import GHC.IO.Handle
import Network.HTTP
import Network.URI (parseURI)
import System.Directory
import System.Environment.Executable
import System.IO
import System.Process
import Test.WebDriver
import qualified Test.WebDriver.Class as WD
import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Commands.Wait
import Text.Printf

sleep :: Int -> IO()
sleep msecs =
    threadDelay $ msecs * 1000

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
    where
        combsBySize = foldr partial ([[]] : repeat [])
        partial x next = zipWith (++) (map (map (x:)) ([]:next)) next

possibleButtons :: [String]
possibleButtons =
        map inter $ comb 2 ['0'..'9']
    where
        inter x = intercalate " ou " (map (:[]) x)

findPasswordButtons :: WD.WebDriver m => [Char] -> m (Map Char Element)
findPasswordButtons passwd =
    findPasswordButtons' digits Map.empty
    where
        digits = sort $ nub passwd

findPasswordButtons' :: WD.WebDriver m => [Char] -> Map Char Element -> m (Map Char Element)
findPasswordButtons' [] found = return (found)
findPasswordButtons' (d:ds) found = do
        elems <- mapM fElems (filter (elem d) possibleButtons)
        let butt = head $ concat elems
        findPasswordButtons' ds (Map.insert d butt found)
    where
        fElems x = findElems $ ByCSS (pack $ "img[title=\"" ++ x ++ "\"]")

login :: (WD.WebDriver m, MonadIO m) => String -> String -> String -> [Char] -> m ()
login agencia conta nome senha = do
    openPage "http://www.itau.com.br"

    -- Primeira etapa, digita ag/cc e pede acesso
    findElem (ById "campo_agencia") >>= sendKeys (pack agencia)
    findElem (ById "campo_conta")   >>= sendKeys (pack conta)
    findElem (ByLinkText "Acessar") >>= click

    -- Seleciona o usuário
    findElem (ByLinkText $ toUpper (pack nome)) >>= click
    liftIO $ sleep 500

    -- Digita a senha
    allButtons <- findPasswordButtons senha
    let bs = map (\s -> fromJust (Map.lookup s allButtons)) senha
    _ <- mapM click bs
    liftIO $ sleep 500

    -- Completa o login
    findElem (ByCSS "img[alt=\"Continuar\"]") >>= click

getOFXStartDate :: IO (Text, Text, Text)
getOFXStartDate = do
    now <- getCurrentTime >>= utcToLocalZonedTime
    let today =  localDay $ zonedTimeToLocalTime now
        twoMonthsAgo = addDays (-60) today -- Limite estabelecido pelo Itau
    return . toText $ toGregorian twoMonthsAgo
    where
        fmt2 = pack . printf "%02d"
        toText (year, month, day) = (pack $ show year, fmt2 month, fmt2 day)

getOFX :: WD()
getOFX = do
    findElem (ByLinkText "Conta Corrente") >>= click
    findElem (ByLinkText "Extrato") >>= click
    waitUntil 30 $ findElem (ByLinkText "Salvar em outros formatos") >>= click
    (year, month, day) <- liftIO $ getOFXStartDate
    liftIO $ sleep 1000 -- Apesar da página completa, um script passa zerando os valores dos campos se formos muito rápido
    waitUntil 30 $ findElem (ById "Dia") >>= sendKeys day
    waitUntil 30 $ findElem (ById "Mes") >>= sendKeys month
    waitUntil 30 $ findElem (ById "Ano") >>= sendKeys year
    waitUntil 30 $ findElem (ByCSS "input[value=\"OFX\"]") >>= click
    waitUntil 30 $ findElem (ByCSS "img.TRNinputBTN") >>= click
    liftIO $ sleep 5000 -- aguarda alguns segundos pra que o download tenha acabado


logout :: WD()
logout = do
    findElem (ByCSS "img.btnSair") >>= click
    findElem (ByCSS "img[alt=\"Sair\"]") >>= click
    liftIO $ sleep 5000 -- Aguarda alguns segundos para ter certeza que o logout terminou

myProfile :: IO (PreparedProfile Firefox)
myProfile =
        prepareProfile $ profile
    where
        profile =
             addPref "browser.download.folderList"                   (0 :: Integer)
           $ addPref "browser.download.manager.alertOnEXEOpen"       False
           $ addPref "browser.download.manager.closeWhenDone"        True
           $ addPref "browser.download.manager.focusWhenStarting"    False
           $ addPref "browser.download.manager.showAlertOnComplete"  False
           $ addPref "browser.download.manager.showWhenStarting"     False
           $ addPref "browser.download.manager.useWindow"            False
           $ addPref "browser.download.panel.shown"                  False
           $ addPref "browser.download.useDownloadDir"               True
           $ addPref "browser.helperApps.alwaysAsk.force"            False
           -- sim, o Itau informa o content type como "content/type" :/
           $ addPref "browser.helperApps.neverAsk.saveToDisk"        ("content/type" :: String)
           defaultProfile

myConfig :: IO (WDConfig)
myConfig = do
    pprof <- myProfile
    return $ defaultConfig {
        wdCapabilities = defaultCaps {
            browser = firefox {
                ffProfile = (Just  pprof)
            }
        }
    }

checkAndDownloadSelenium :: FilePath -> IO ()
checkAndDownloadSelenium fname = do
        fExists <- doesFileExist fname
        when (not fExists) $ do
            putStrLn "Selenium server não encontrado. Iniciando download..."
            bytes <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody
            B.writeFile fname bytes
            putStrLn "Download terminado"
    where
        (Just url) = parseURI "http://selenium-release.storage.googleapis.com/2.46/selenium-server-standalone-2.46.0.jar"

startSeleniumServer :: IO ()
startSeleniumServer = do
        (path, _) <- splitExecutablePath
        let fname = fullFilePath path
        checkAndDownloadSelenium fname
        (_, _, Just st_err, _) <- createProcess (shell $ "java -jar " ++ fname){std_out = CreatePipe, std_err = CreatePipe}
        waitStart st_err
    where
        -- hardcoded, por enquanto, com a versão 2.46
        bname = "selenium-server-standalone-2.46.0.jar"
        fullFilePath path = path ++ bname
        waitStart fstream = do
            ln <- hGetLine fstream
            when (not $ isSuffixOf "INFO - Selenium Server is up and running" ln) $ waitStart fstream

stopSeleniumServer :: IO ()
stopSeleniumServer = do
        res <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody ::IO String
        assert (res == "OKOK") $ return ()
    where
        (Just url) = parseURI "http://localhost:4444/selenium-server/driver/?cmd=shutDownSeleniumServer"

main :: IO()
main = do
    putStr "Agência: "
    hFlush stdout
    agencia <- getLine
    putStr "Conta Corrente: "
    hFlush stdout
    conta <- getLine
    putStr "Primeiro nome: "
    hFlush stdout
    nome <- getLine
    putStr "Senha: "
    hFlush stdout
    senha <- withEcho False getLine
    putChar '\n'

    putStrLn "Iniciando Selenium Server"
    startSeleniumServer
    config <- myConfig
    runSession config $ do
        setImplicitWait 100
        liftIO $ putStrLn "Efetuando login"
        login agencia conta nome senha
        liftIO $ putStrLn "Pegando OFX"
        getOFX
        liftIO $ putStrLn "Deslogando"
        logout
        liftIO $ putStrLn "Fechando sessão Selenium"
        closeSession
    putStrLn "Desligando Selenium Server"
    stopSeleniumServer
