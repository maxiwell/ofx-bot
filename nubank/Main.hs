{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import           Data.List
import           Data.Maybe
import           Data.Text (pack, unpack)
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Tuple.Select
import           GHC.IO.Handle
import           Network.HTTP
import           Network.URI (parseURI)
import           System.Directory
import           System.FilePath
import           System.Environment.Executable
import           System.IO
import           System.Process
import           Test.WebDriver
import           Test.WebDriver.Firefox.Profile
import           Test.WebDriver.Commands.Wait
import           Text.Printf

data Tx = Tx {
    txData      :: String,
    txDescricao :: String,
    txValor     :: String,
    txFatura    :: String
} deriving Show

data Fatura = Fatura {
    fatVencimento :: String,
    fatTxs        :: [Tx]
} deriving Show 

sleep :: Int -> IO()
sleep msecs =
    threadDelay $ msecs * 1000

wait :: WD a -> WD a
wait = waitUntil 30

today :: IO Day
today = do
    now <- getCurrentTime >>= utcToLocalZonedTime
    return $ localDay $ zonedTimeToLocalTime now    

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

login :: String -> String -> WD ()
login cpf senha = do
    openPage "http://www.nubank.com.br"
    wait $ findElem (ByLinkText "Login") >>= click
    wait $ findElem (ById "username") >>= sendKeys (pack cpf)
    wait $ findElem (ById "input_001")   >>= sendKeys (pack senha) --senha
    wait $ findElem (ByXPath "//button[@type='submit']") >>= click

logout :: WD()
logout = do
    wait $ findElem (ByCSS "a.logout") >>= click
    liftIO $ sleep 5000 -- Aguarda alguns segundos para ter certeza que o logout terminou

converteMes :: String -> Int 
converteMes m = 
    1 + fromJust(elemIndex m meses)
    where
        meses = ["JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"]
    
converteData :: String -> WD String
converteData str = do
    hj <- liftIO today
    return (show (sel1 (toGregorian hj)) ++ "-" ++ fmt2 (converteMes m) ++ "-" ++ fmt2 (read d :: Int))
    where
        [d, m] = words str 
        fmt2 = printf "%02d"

txToCSV :: Tx -> String
txToCSV tx = 
    intercalate ";" linha
    where 
    linha = [txData tx, "", "Fat Vcto " ++ txFatura tx, "", txDescricao tx, txValor tx, "", ""]

faturaToCSV :: Fatura -> String
faturaToCSV fat = 
    unlines $ map txToCSV (fatTxs fat)

getTx :: String -> Element -> WD Tx
getTx fat ele = do
    dt0  <- wait $ findElemFrom ele (ByCSS "span.date.ng-binding") >>= getText
    dt   <- converteData $ unpack dt0 
    desc <- wait $findElemFrom ele (ByCSS "div.description.ng-binding") >>= getText
    val  <- wait $ findElemFrom ele (ByCSS "div.amount.ng-binding") >>= getText
    return Tx {     
        txData      = dt,
        txDescricao = unpack desc,
        txValor     = unpack val,
        txFatura    = fat    
    }

getTxs :: String -> WD [Tx]
getTxs venc = do
    tableLines <- wait $ findElems (ByCSS "div.charge.ng-scope")
    mapM (getTx venc) tableLines

getFaturaIx :: String -> WD Fatura
getFaturaIx _ix = do
    wait $ findElem (ByCSS "a.menu-item.bills") >>= click -- Menu Faturas
    liftIO $ sleep 5000
    --por enquanto apenas a fatura atual
    qdrEsq <- wait $ findElem (ByCSS "div.amount-due")
    venc0  <- wait $ findElemFrom qdrEsq (ByCSS "span.date.ng-binding") >>= getText
    venc   <- converteData $ unpack venc0
    txs    <- getTxs venc
    return Fatura {
        fatVencimento = venc,
        fatTxs = txs        
    }        
    
getFatura :: WD Fatura
getFatura =
    getFaturaIx ""

myProfile :: String -> IO (PreparedProfile Firefox)
myProfile dir =
    prepareProfile $
         addPref "browser.download.useDownloadDir"               True
       $ addPref "browser.download.dir"                          dir
       $ addPref "browser.download.folderList"                   (2 :: Integer)
       $ addPref "browser.download.manager.alertOnEXEOpen"       False
       $ addPref "browser.download.manager.closeWhenDone"        True
       $ addPref "browser.download.manager.focusWhenStarting"    False
       $ addPref "browser.download.manager.showAlertOnComplete"  False
       $ addPref "browser.download.manager.showWhenStarting"     False
       $ addPref "browser.download.manager.useWindow"            False
       $ addPref "browser.download.panel.shown"                  False
       $ addPref "browser.helperApps.alwaysAsk.force"            False
       $ addPref "browser.helperApps.neverAsk.saveToDisk"        ("content/type,text/plain" :: String)
       $ addPref "xpinstall.signatures.required"                 False
         defaultProfile


myConfig :: String -> IO WDConfig
myConfig dir = do
    pprof <- myProfile dir
    return $ defaultConfig {
        wdCapabilities = defaultCaps {
            browser = firefox {
                ffProfile = Just  pprof
            }
        }
    }

checkAndDownloadSelenium :: FilePath -> IO ()
checkAndDownloadSelenium fname = do
        fExists <- doesFileExist fname
        unless fExists $ do
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
            unless ("INFO - Selenium Server is up and running" `isSuffixOf` ln) $ waitStart fstream

stopSeleniumServer :: IO ()
stopSeleniumServer = do
        res <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody ::IO String
        assert (res == "OKOK") $ return ()
    where
        (Just url) = parseURI "http://localhost:4444/selenium-server/driver/?cmd=shutDownSeleniumServer"

main :: IO()
main = do
    putStr "CPF: "
    hFlush stdout
    cpf <- getLine
    putStr "Senha: "
    hFlush stdout
    senha <- withEcho False getLine
    putChar '\n'

    putStrLn "Iniciando Selenium Server"
    startSeleniumServer
    dir <- getCurrentDirectory
    config <- myConfig dir
    runSession config $ do
        setImplicitWait 200
        liftIO $ putStrLn "Efetuando login"
        login cpf senha
        liftIO $ putStrLn "Pegando CSV"
        fatura <- getFatura
        let fname = "nubank_venc_" ++ fatVencimento fatura ++ ".csv" 
        liftIO $ T.writeFile (combine dir fname) (pack $ faturaToCSV fatura)
        liftIO $ putStrLn "Deslogando"
        logout
        liftIO $ putStrLn "Fechando sessão Selenium"
        closeSession
    putStrLn "Desligando Selenium Server"
    stopSeleniumServer