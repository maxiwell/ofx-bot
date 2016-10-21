{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types 
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
          | Firefox
          | Chrome
          | Help
    deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['o'] ["ofx", "OFX"] (NoArg OFX) 
        "Baixa as transações e salva no formato OFX (DEFAULT caso nenhuma opção seja especificada)"
  , Option ['c'] ["csv", "CSV"] (NoArg CSV) 
        "Baixa as transações e salva no formato CSV compatível com HomeBank"
  , Option ['f'] ["ff", "firefox"] (NoArg Main.Firefox) 
              "Utiliza o Firefox"
  , Option ['C'] ["gc", "chrome"] (NoArg Main.Chrome) 
              "Utiliza o Google Chrome (default)"              
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

fmtInt3Hex :: Int -> String
fmtInt3Hex = printf "%03X"

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

converteMes :: String -> Int 
converteMes m = 
    1 + fromJust(elemIndex m meses)
    where
        meses = ["JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"]

getAno :: Day -> Integer
getAno = sel1 . toGregorian
    
converteData :: Integer -> String -> WD (Maybe Day)
converteData _   ""  = return Nothing
converteData ano str = 
    return $ Just $ fromGregorian ano mConvertido dConvertido
    where
        [d, m] = words str
        mConvertido = converteMes m 
        dConvertido = read d

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

getTx ::  Integer -> Day -> String -> Element -> WD Tx
getTx ano venc fat ele  = do
    dt0 <- wait $ findElemFrom ele (ByCSS "span.date.ng-binding") >>= getText
    dtc <- converteData ano $ T.unpack dt0
    let dt = case dtc of Just dtc2 -> Just (if dtc2 > venc then addGregorianYearsClip (-1) dtc2 else dtc2)
                         Nothing -> Nothing
    desc <- wait $ findElemFrom ele (ByCSS "div.description.ng-binding") >>= getText
    val  <- wait $ findElemFrom ele (ByCSS "div.amount.ng-binding") >>= getText
    return Tx {     
        txData      = dt,
        txDescricao = T.unpack desc,
        txValor     = negate $ converteValor val,
        txFatura    = fat    
    }

getTxs ::  Integer -> Day -> String -> WD [Tx]
getTxs ano vencDay venc = do
    tableLines <- wait $ findElems (ByCSS "div.charge.ng-scope")
    mapM (getTx ano vencDay venc) tableLines

getVencimentoInicial :: WD Day
getVencimentoInicial = do
    hj <- liftIO today
    let (y, m, _) = toGregorian hj
    venc <- getVencimentoDay y
    let (yv, mv, dv) = toGregorian venc -- qualquer ano só para começar
    -- Se for menor, quer dizer que virou o ano, se for maior, é no mesmo ano
    -- Este método assume que não existem parcelamentos no cartão acima de 12X
    return $ fromGregorian (if mv > m then yv else yv + 1) mv dv

getVencimentoDay :: Integer -> WD Day
getVencimentoDay ano = do
    venc <- getVencimento
    dt0 <- converteData ano venc
    return $ fromJust dt0

getVencimento :: WD String 
getVencimento = do 
    -- A caixa com o Vencimento troca de nome e de classe e de caminho dependendo 
    -- da aba selecionada. Então, mato o mosquito com tiro de canhão, pego a 
    -- bagaça toda e taco a regex nela
    venc1 <- wait $ findElem (ByClass "bills-browser") >>= getText
    let vencFull = T.unpack venc1
    return $ case matchRegex regex vencFull of
        Nothing -> ""
        Just ~[venc] -> venc
    where
        regex = mkRegex "Vencimento.+([0-9][0-9]+....)"

getFaturaIxs :: Int -> [Element] -> WD [Element]
getFaturaIxs ix acc = do
    elems <- findElems (ByCSS $ T.pack $ "#tab_" ++ fmtInt3Hex ix)
    case elems of
        []   -> return acc
        [el] -> getFaturaIxs (ix + 1) (el:acc)
        _    -> error "should not be here"

getFaturaIx :: (Day, [Tx]) -> Element -> WD (Day, [Tx])
getFaturaIx (vencBase, acc) el = do
    scrollPrevious el
    wait $ click el
    liftIO $ sleep 1000
    venc <- getVencimento
    dvenc <- getVencimentoDay $ getAno vencBase
    liftIO $ putStrLn ("Obtendo Fatura com Vencimento: " ++ show dvenc)
    txs <- getTxs (getAno vencBase) dvenc ("Venc. " ++ venc)
    return (addGregorianMonthsClip (-1) vencBase, acc ++ txs)

getFatura :: WD [Tx]
getFatura = do
    wait $ findElem (ByCSS "a.menu-item.bills") >>= click -- Menu Faturas
    liftIO $ sleep 5000
    goToEnd
    elems <- getFaturaIxs 2 []
    click $ head elems
    liftIO $ sleep 1000
    vencBase <- getVencimentoInicial
    (_, txs) <- F.foldlM getFaturaIx (vencBase, []) elems 
    return txs

navigationButtons :: WD (Maybe Element, Maybe Element)
navigationButtons = do 
    banner <- findElem $ ByXPath "/html/body/navigation-base/div[1]/div/main/section/bill-browser/div/md-tabs/section"
    elems  <- findElemsFrom banner (ByTag "button")
    case elems of 
        [p, n] -> return (Just p, Just n)
        [e]    -> do
            hp <- isPrevious e
            return $ if hp then (Just e, Nothing) else (Nothing, Just e)
        []     -> return (Nothing, Nothing)
        _      -> error "Numero de botões /= 0, 1 ou 2"
    where
        isPrevious :: Element -> WD Bool
        isPrevious e = do
            at <- attr e "class"
            return $ fromJust at == "md-paginator md-prev ng-scope"

scrollPrevious :: Element -> WD ()
scrollPrevious l = do
    dis <- isDisplayed l
    unless dis $ do
        (mp, _) <- navigationButtons
        click $ fromMaybe (error "O botão da fatura procurada não esta visivel nem o previous") mp
        liftIO $ sleep 1000

goToEnd :: WD ()
goToEnd = do
    (_, n) <- navigationButtons    
    case n of
        Nothing -> return ()
        Just e  -> do
            wait $ click e
            liftIO $ sleep 1500
            goToEnd 


myFFProfile :: String -> IO (PreparedProfile Firefox)
myFFProfile dir =
    prepareProfile $
         addPref "browser.download.useDownloadDir"              True
       $ addPref "browser.download.dir"                         dir
       $ addPref "browser.download.folderList"                  (2 :: Integer)
       $ addPref "browser.download.manager.alertOnEXEOpen"      False
       $ addPref "browser.download.manager.closeWhenDone"       True
       $ addPref "browser.download.manager.focusWhenStarting"   False
       $ addPref "browser.download.manager.showAlertOnComplete" False
       $ addPref "browser.download.manager.showWhenStarting"    False
       $ addPref "browser.download.manager.useWindow"           False
       $ addPref "browser.download.panel.shown"                 False
       $ addPref "browser.helperApps.alwaysAsk.force"           False
       $ addPref "browser.helperApps.neverAsk.saveToDisk"       ("content/type,text/plain" :: String)
       $ addPref "xpinstall.signatures.required"                False
       -- $ addPref "webdriver.gecko.driver"                    ("PATH_TO_WIRES" :: String) 
         defaultProfile


getConfig :: String -> [Flag] -> IO WDConfig
getConfig dir flags  = 
    if Main.Firefox `elem` flags then 
        getFFConfig
    else
        getChromeConfig
    where
    getChromeConfig = return $ useBrowser chrome {
            chromeOptions = ["--start-maximized"]
        }
        defaultConfig
    getFFConfig = do         
        pprof <- myFFProfile dir
        return $ defaultConfig {
            wdCapabilities = defaultCaps {
                additionalCaps = [("marionette", Bool True)],
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
        putStrLn "Desligando Selenium Server"
        res <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody ::IO String
        assert (res == "OKOK") $ return ()
    where
        (Just url) = parseURI "http://localhost:4444/selenium-server/driver/?cmd=shutDownSeleniumServer"

generateCSV :: String -> FilePath -> [Tx] -> IO()
generateCSV cpf dir fatura =
    let fname = "nubank_" ++ cpf ++ ".csv" in do
        putStrLn "Gerando CSV"
        Tio.writeFile (combine dir fname) (faturaToCSV fatura)

generateOFX :: String -> FilePath -> [Tx] -> IO()
generateOFX cpf dir fatura =
    let fname = "nubank_" ++ cpf ++ ".ofx" in do
        putStrLn "Gerando OFX"
        hj <- today
        Tio.writeFile (combine dir fname)  (faturaToOFX fatura hj cpf)
        
getLoginInfo :: IO (String, String)
getLoginInfo = do 
    putStr "CPF: "
    hFlush stdout
    cpf <- getLine
    putStr "Senha: "
    hFlush stdout
    senha <- withEcho False getLine
    putChar '\n'
    return (cpf, senha)

nuBankScript :: String -> String -> FilePath -> [Flag] -> WD ()
nuBankScript cpf senha dir flags = do
    setImplicitWait 200
    -- maximize
    liftIO $ putStrLn "Efetuando login"
    login cpf senha
    f0 <- getFatura
    let fatura = filter isValidTx f0
    
    when (CSV `elem` flags) $ 
        liftIO (generateCSV cpf dir fatura)
    when (null flags || OFX `elem` flags) $ 
        liftIO (generateOFX cpf dir fatura)
            
    liftIO $ putStrLn "Arquivo gerado com sucesso."

    
startSeleniumAndRun :: [Flag] -> IO ()
startSeleniumAndRun flags = do
    (cpf, senha) <- getLoginInfo
    putStrLn "Iniciando Selenium Server"
    startSeleniumServer
    putStrLn "Selenium Server Online"
    dir <- getCurrentDirectory
    config <- getConfig dir flags
    runSession config $ finallyClose $ nuBankScript cpf senha dir flags 

main :: IO()
main = do
    args <- getArgs
    (flags, _) <- getOpts args
    
    if Help `elem`flags || (length args /= length flags) then
        putStrLn usageString
    else
        startSeleniumAndRun flags `catch` exHandler `finally` stopSeleniumServer
        where 
            exHandler :: SomeException -> IO ()
            exHandler e = putStrLn $ "Erro durante a execução" ++ displayException e

