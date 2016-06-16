import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text.IO as T
import           Itau
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.FilePath

data Flag = OFX 
          | CSV
          | Firefox
          | Chrome
          | Cartao
          | Help
    deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['o'] ["ofx", "OFX"] (NoArg OFX) 
        "Baixa as transações da conta corrente e salva no formato OFX (DEFAULT caso nenhuma opção seja especificada)"    
  , Option ['c'] ["csv", "CSV"] (NoArg CSV) 
        "Baixa as transações e salva no formato CSV compatível com HomeBank"
  , Option ['C'] ["cc", "CC", "cartao"] (NoArg Cartao) 
        "Baixa as transações do cartão de crédito"
  , Option ['f'] ["ff", "firefox"] (NoArg Firefox) 
        "Utiliza o Firefox (Instável)"
  , Option ['r'] ["gc", "chrome"] (NoArg Chrome) 
        "Utiliza o Google Chrome (default)"
  , Option ['h'] ["help"] (NoArg Help)
        "Imprime as opções do programa"
  ]

usageString :: String
usageString = 
        usageInfo header options
    where 
        header = "Uso: ofx-itau -op1 --long-op2\nEx: ofx-itau --OFX -c --cartao\n"
  
getOpts :: [String] -> IO ([Flag], [String])
getOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageString))

main :: IO()
main = do
    args <- getArgs
    (flags, _) <- getOpts args
    
    if Help `elem`flags || 
       (length args /= length flags) || 
       all (`elem` flags) [Firefox, Chrome] then
           putStrLn usageString
    else do
        userParams <- getUserParams
        itauRun (if Firefox `elem` flags then FF else CH) $ do

            liftIO $ putStrLn "Efetuando login"
            login userParams

            when (OFX `elem` flags || all (`notElem` flags) [CSV, Cartao]) $ do
                liftIO $ putStrLn "Pegando OFX"
                getOFX

            when (CSV `elem` flags) $ do
                liftIO $ putStrLn "Pegando CSV"
                getCSV
            
            when (Cartao `elem` flags) $ do
                liftIO $ putStrLn "Pegando Info. Cartão de Crédito"
                dir <- liftIO getCurrentDirectory
                cc  <- getCCInfo
                liftIO $ case cc of
                    Left err  -> print err
                    Right rcc -> T.writeFile (combine dir "itaucard.csv") (itauCardInfoToCSV rcc)
                
            liftIO $ putStrLn "Deslogando"
            logout


            
            