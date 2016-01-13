import           Control.Monad.IO.Class
import           Itau

main :: IO()
main = do
    userParams <- getUserParams
    itauRun $ do
        liftIO $ putStrLn "Efetuando login"
        login userParams
        liftIO $ putStrLn "Pegando OFX"
        getOFX
        liftIO $ putStrLn "Deslogando"
        logout