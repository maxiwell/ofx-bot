
import System.Directory
import Network.HTTP
import Network.URI (parseURI)
import Control.Monad
import Data.List
import qualified Data.ByteString as B
import qualified Data.Text as T
--import qualified Control.Exception as Exc
import Control.Exception 
import System.Environment  
import System.Environment.Executable
import System.Console.GetOpt
import System.IO
import System.Process

-- Selenium libs
import Control.Monad.IO.Class
import Test.WebDriver
import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Commands.Wait

data Flag = Cartao
          | Poupanca
          | Help
   deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['c'] ["cc", "CC", "cartao"] (NoArg Cartao) 
                  "Baixa as transações do cartão de crédito"
            , Option ['p'] ["poupanca"] (NoArg Poupanca)
                  "Imprime as opções do programa"
            , Option ['h'] ["help"] (NoArg Help)
                  "Imprime as opções do programa"
            ]


usageString :: [Char]
usageString = 
    usageInfo header options
    where
        header = "Uso: ofx-bb [options]\n\n[options]:"
      
getOpts :: [String] -> IO ([Flag], [String])
getOpts argv = 
    case getOpt Permute options argv of
        (o,n,[])  -> return (o,n)
        (_,_,errs) -> ioError $ userError $ concat errs ++ usageString

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

wait :: WD a -> WD a
wait = waitUntil 30

getLoginInfo :: IO (String, String, String)
getLoginInfo = do
    putStr "Agencia: "
    hFlush stdout
    agencia <- getLine
    putStr "Conta: "
    hFlush stdout
    conta   <- getLine
    putStr "Senha: "
    hFlush stdout
    senha   <- withEcho False getLine
    putChar '\n'
    return (agencia, conta, senha) 


login :: String -> String -> String -> WD ()
login agencia conta senha = do
    openPage "https://www2.bancobrasil.com.br/aapf/login.jsp"
    wait $ findElem (ById "dependenciaOrigem") >>= sendKeys (T.pack agencia)
    wait $ findElem (ById "numeroContratoOrigem") >>= sendKeys (T.pack conta)
    wait $ findElem (ById "senhaConta") >>= sendKeys (T.pack senha)
    wait $ findElem (ById "botaoEntrar") >>= click


getOfx :: Flag -> IO ()
getOfx Cartao = do
    liftIO $ putStrLn "Acessando o Cartao de Credito..."
    wait $ findElem (ByXPath "//a[@nome='Cartões']") >>= click
    wait $ findElem (ByXPath "//a[@codigo='3580']") >>= click
    wait $ findElem (ByXPath "//img[@title='PETROBRAS']") >>= click
    wait $ findElem (ByXPath "//a[@onclick='$.criarCaixaDialogoSalvarFatura(this,event);']") >>= click
    wait $ findElem (ByLinkText "Money 2000+ (ofx)") >>= click

getOfx Poupanca = do 
    liftIO $ putStrLn "Acessando a Poupança..."
    wait $ findElem (ByXPath "//a[@nome='Poupança']") >>= click
    wait $ findElem (ByXPath "//a[@codigo='3909']") >>= click
    wait $ findElem (ByPartialLinkText "30 dias") >>= click
    wait $ findElem (ByCSS "a.botaoToolBar.botaoToolBarSalvar") >>= click
    wait $ findElem (ByLinkText "Money 2000+ (ofx)") >>= click

getOfx x = do 
    liftIO $ putStrLn "Acessando a Conta Corrente..."
    wait $ findElem (ByCSS "li.saldo-texto") >>= click
    wait $ findElem (ByPartialLinkText "30 dias") >>= click
    wait $ findElem (ByCSS "a.botaoToolBar.botaoToolBarSalvar") >>= click
    wait $ findElem (ByLinkText "Money 2000+ (ofx)") >>= click

bbScript :: String -> String -> String -> FilePath -> [Flag] -> WD ()
bbScript agencia conta senha dir flags = do
    setImplicitWait 200
    -- maximize
    liftIO $ putStrLn "Efetuando login"
    login agencia conta senha 

    -- Resolvendo janela de codigo
    -- Try/Catch
    -- findElem (ByXPath "//a[@title='Fechar']") >>= click
    
    getOfx 
    if (Cartao `elem` flags) 
        getOfx Cartao
    if (Poupanca `elem` flags)
        getOfx Poupanca
        
    liftIO $ putStrLn "Arquivo gerado com sucesso."


myFFProfile :: String -> IO (PreparedProfile Firefox)
myFFProfile dir =
    prepareProfile $
         addPref "plugin.state.java"                            (2 :: Integer)
       $ addPref "browser.download.useDownloadDir"              True
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
       $ addPref "browser.helperApps.neverAsk.saveToDisk"       ("text/ofx,text/plain" :: String)
       $ addPref "xpinstall.signatures.required"                False
       -- $ addPref "webdriver.gecko.driver"                    ("PATH_TO_WIRES" :: String) 
         defaultProfile


getConfig :: String -> [Flag] -> IO WDConfig
getConfig dir flags  = 
    getFFConfig
    where
        getFFConfig = do         
            pprof <- myFFProfile dir
            return $ defaultConfig {
                wdCapabilities = defaultCaps {
                    --additionalCaps = [("marionette", Bool True)],
                        browser = firefox {
                        ffProfile = Just pprof
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
        --(Just url) = parseURI $ "http://selenium-release.storage.googleapis.com"
        --              ++ "/3.0-beta2/selenium-server-standalone-3.0.0-beta2.jar"
        (Just url) = parseURI $ "http://selenium-release.storage.googleapis.com"
                      ++ "2.53/selenium-server-standalone-2.53.0.jar" 



startSeleniumServer :: IO ()
startSeleniumServer = do 
    (path, _) <- splitExecutablePath
    let fname = fullFilePath path
    checkAndDownloadSelenium fname 
    (_, _, Just st_err, _) <- createProcess (shell $ "java -jar " ++ fname) 
                                {std_out = CreatePipe, std_err = CreatePipe}
    waitStart st_err
    where
        bname = "selenium-server-standalone-2.53.0.jar"
        --bname = "selenium-server-standalone-3.0.0-beta2.jar"
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


startSeleniumAndRun :: [Flag] -> IO ()
startSeleniumAndRun flags = do
    (agencia, conta, senha) <- getLoginInfo
    putStrLn "Iniciando Selenium Server"
    startSeleniumServer
    putStrLn "Selenium Server Online"
    dir <- getCurrentDirectory
    config <- getConfig dir flags
    runSession config $ finallyClose $ bbScript agencia conta senha dir flags
 
main :: IO() 
main = do
    args <- getArgs
    (flags, _) <- getOpts args

    if Help `elem` flags || (length args /= length flags) then
        putStrLn usageString
    else
        startSeleniumAndRun flags `catch` exHanlder `finally` stopSeleniumServer
        where
            exHanlder :: SomeException -> IO ()
            exHanlder e = putStrLn $ "Erro durante a execucao\n" ++ displayException e


