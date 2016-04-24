{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import           Data.List
import           Data.Maybe
import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Tuple.Select
import           GHC.IO.Handle
import           Network.HTTP
import           Network.URI (parseURI)
import           System.Directory
import           System.FilePath
import           System.Environment  
import           System.Environment.Executable
import           System.IO
import           System.Process
import           Test.WebDriver
import           Test.WebDriver.Firefox.Profile
import           Test.WebDriver.Commands.Wait
import           Text.Printf
import           Text.Regex
import           System.Console.GetOpt

data Flag = OFX 
          | CSV
          | Help
    deriving (Eq, Show)
          
options :: [OptDescr Flag]
options =
  [ Option ['o'] ["ofx", "OFX"] (NoArg OFX) 
        "Baixa as transações e salva no formato OFX (DEFAULT caso nenhuma opção seja especificada)"
  , Option ['c'] ["csv", "CSV"] (NoArg CSV) 
        "Baixa as transações e salva no formato CSV compatível com HomeBank"
  , Option ['h'] ["help"] (NoArg Help)
        "Imprime as opções do programa"
  ]

usageString :: String
usageString = 
        usageInfo header options
    where 
        header = "Uso: ofx-nubank -op1 --long-op2\nEx: ofx-nubank --OFX -c\n"
  
getOpts :: [String] -> IO ([Flag], [String])
getOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageString))

data Tx = Tx {
    txData      :: Maybe Day,
    txDescricao :: String,
    txValor     :: Float,
    txFatura    :: String
} deriving Show

fmtInt2 :: Int -> String
fmtInt2 = printf "%02d"

fmtInt3 :: Int -> String
fmtInt3 = printf "%03d"

fmtDouble2 :: Float -> String
fmtDouble2 = printf "%.2g"

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
    wait $ findElem (ById "username") >>= sendKeys (T.pack cpf)
    wait $ findElem (ById "input_001")   >>= sendKeys (T.pack senha) --senha
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
    
converteData :: String -> WD (Maybe Day)
converteData "" = return Nothing
converteData str = do
    hj <- liftIO today
    return $ Just $ fromGregorian (sel1 $ toGregorian hj) (converteMes m) (read d)
    where
        [d, m] = words str

converteValor :: T.Text -> Float
converteValor val =
        if '-' `elem` T.unpack val
            then negate num
            else num
    where
        el = (`F.elem`  (",0123456789":: String))
        num = read . T.unpack . T.replace "," "." $ T.filter el val

isValidTx :: Tx -> Bool
isValidTx (Tx Nothing _  _  _) = False
isValidTx (Tx _       "" _  _) = False
isValidTx _                    = True

showDDMMYY :: Day -> String
showDDMMYY dt = 
        intercalate "-" [fmtInt2 d, fmtInt2 m, fmtInt2 . fromIntegral $ y `mod` 100]
    where
        (y,m,d) = toGregorian dt
        

txToCSV :: Tx -> String
txToCSV tx = 
    intercalate ";" linha
    where 
    linha = [
        showDDMMYY $ fromJust (txData tx), 
        "", 
        txFatura tx, 
        "", 
        txDescricao tx, 
        fmtDouble2 $ txValor tx, 
        "", 
        ""]

faturaToCSV :: [Tx] -> T.Text
faturaToCSV txs = 
    T.pack $ unlines $ map txToCSV txs
    
txToOFX :: Tx -> String
txToOFX tx = 
    intercalate "\n" [ 
          "<STMTTRN>"
        , "<TRNTYPE>" ++ (if txValor tx < 0 then "DEBIT" else "CREDIT")
        , "<DTPOSTED>" ++ dt
        , "<TRNAMT>" ++ fmtDouble2 (txValor tx)
        , "<FITID>" ++ txFatura tx
        , "<CHECKNUM>" ++ txFatura tx
        , "<MEMO>" ++ txDescricao tx
        , "</STMTTRN>"]
    where
        dt = filter isDigit (showGregorian $ fromJust (txData tx)) ++ "100000[-03:EST]"

faturaToOFX :: [Tx] -> Day -> String -> T.Text
faturaToOFX txs hj loginName = 
        T.pack $ header ++ unlines (map txToOFX txs) ++ footer
    where
        hjg = toGregorian hj
        y = sel1  hjg
        dt = show y ++ fmtInt2 (sel2 hjg) ++ fmtInt2 (sel3 hjg) ++  "100000[-03:EST]"
        header = unlines
            [   "OFXHEADER:100"
              , "DATA:OFXSGML"
              , "VERSION:102"
              , "SECURITY:NONE"
              , "ENCODING:USASCII"
              , "CHARSET:1252"
              , "COMPRESSION:NONE"
              , "OLDFILEUID:NONE"
              , "NEWFILEUID:NONE"
              , ""
              , "<OFX>"
              , "<SIGNONMSGSRSV1>"
              , "<SONRS>"
              , "<STATUS>"
              , "<CODE>0"
              , "<SEVERITY>INFO"
              , "</STATUS>"
              , "<DTSERVER>" ++ dt
              , "<LANGUAGE>POR"
              , "</SONRS>"
              , "</SIGNONMSGSRSV1>"
              , "<BANKMSGSRSV1>"
              , "<STMTTRNRS>"
              , "<TRNUID>1001"
              , "<STATUS>"
              , "<CODE>0"
              , "<SEVERITY>INFO"
              , "</STATUS>"
              , "<STMTRS>"
              , "<CURDEF>BRL"
              , "<BANKACCTFROM>"
              , "<BANKID>0000"
              , "<ACCTID>NUBANK_" ++ loginName 
              , "<ACCTTYPE>CHECKING"
              , "</BANKACCTFROM>"
              , "<BANKTRANLIST>"
            ]
        footer = unlines [
              "</BANKTRANLIST>"
            , "<LEDGERBAL>"
            , "<BALAMT>0.00"
            , "<DTASOF>" ++ dt
            , "</LEDGERBAL>"
            , "</STMTRS>"
            , "</STMTTRNRS>"
            , "</BANKMSGSRSV1>"
            , "</OFX>"
            ]                      


getTx :: String -> Element -> WD Tx
getTx fat ele = do
    dt0  <- wait $ findElemFrom ele (ByCSS "span.date.ng-binding") >>= getText
    dtc   <- converteData $ T.unpack dt0 
    desc <- wait $ findElemFrom ele (ByCSS "div.description.ng-binding") >>= getText
    val  <- wait $ findElemFrom ele (ByCSS "div.amount.ng-binding") >>= getText
    return Tx {     
        txData      = dtc,
        txDescricao = T.unpack desc,
        txValor     = negate $ converteValor val,
        txFatura    = fat    
    }

getTxs :: String -> WD [Tx]
getTxs venc = do
    tableLines <- wait $ findElems (ByCSS "div.charge.ng-scope")
    mapM (getTx venc) tableLines

getVencimento :: WD String 
getVencimento = do 
    -- A caixa com o Vencimento troca de nome e de classe e de caminho dependendo 
    -- da aba selecionada. Então, mato o mosquito com tiro de canhão, pego a 
    -- bagaça toda e taco a regex nela
    venc1 <- wait $ findElem (ByClass "bills-browser") >>= getText
    let vencFull = T.unpack venc1
    return $ case matchRegex regex vencFull of
        Nothing -> ""
        Just ~[venc] -> "Venc. " ++ venc
    where
        regex = mkRegex "Vencimento.+([0-9][0-9]+....)"

getFaturaIx :: Element -> WD [Tx]
getFaturaIx el = do
    wait $ click el
    liftIO $ sleep 1500
    getVencimento >>= getTxs
    

getFaturaIxs :: Int -> [Element] -> WD [Element]
getFaturaIxs ix acc = do
    elems <- findElems (ByCSS $ T.pack $ "#tab_" ++ fmtInt3 ix)
    case elems of
        []   -> return acc
        [el] -> getFaturaIxs (ix + 1) (el:acc)
        _    -> error "should not be here"
    

getFatura :: WD [Tx]
getFatura = do
    wait $ findElem (ByCSS "a.menu-item.bills") >>= click -- Menu Faturas
    liftIO $ sleep 5000
    elems <- getFaturaIxs 2 []
    txs <- mapM getFaturaIx elems
    return $ concat txs

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
        (Just url) = parseURI "http://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.0.jar"

startSeleniumServer :: IO ()
startSeleniumServer = do
        (path, _) <- splitExecutablePath
        let fname = fullFilePath path
        checkAndDownloadSelenium fname
        (_, _, Just st_err, _) <- createProcess (shell $ "java -jar " ++ fname){std_out = CreatePipe, std_err = CreatePipe}
        waitStart st_err
    where
        -- hardcoded, por enquanto, com a versão 2.53
        bname = "selenium-server-standalone-2.53.0.jar"
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

generateCSV :: String -> FilePath -> [Tx] -> IO()
generateCSV cpf dir fatura =
    let fname = "nubank_" ++ cpf ++ ".csv" in do
        putStrLn "Pegando CSV"
        Tio.writeFile (combine dir fname) (faturaToCSV fatura)

generateOFX :: String -> FilePath -> [Tx] -> IO()
generateOFX cpf dir fatura =
    let fname = "nubank_" ++ cpf ++ ".ofx" in do
        putStrLn "Pegando OFX"
        hj <- today
        Tio.writeFile (combine dir fname)  (faturaToOFX fatura hj cpf)

main :: IO()
main = do
    args <- getArgs
    (flags, _) <- getOpts args
    
    if Help `elem`flags || (length args /= length flags) then
        putStrLn usageString
    else do
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
            f0 <- getFatura
            let fatura = filter isValidTx f0
            
            when (CSV `elem` flags) $ 
                liftIO (generateCSV cpf dir fatura)
            when (null flags || OFX `elem` flags) $ 
                liftIO (generateOFX cpf dir fatura)
                    
            liftIO $ putStrLn "Deslogando"
            logout
            liftIO $ putStrLn "Fechando sessão Selenium"
            closeSession
        putStrLn "Desligando Selenium Server"
        stopSeleniumServer
