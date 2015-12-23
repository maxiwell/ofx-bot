import           Control.Monad.IO.Class
import           Test.WebDriver
import           Itau
import           Itau.Selenium
-- import           System.Directory

-- outputDir :: IO String
-- outputDir = getCurrentDirectory

main :: IO()
main = do
    userParams <- getUserParams
    seleniumRun $ do
        setImplicitWait 100
        liftIO $ putStrLn "Efetuando login"
        login userParams
        liftIO $ putStrLn "Pegando OFX"
        getOFX
        liftIO $ putStrLn "Deslogando"
        logout