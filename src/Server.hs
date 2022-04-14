{-# LANGUAGE CPP #-}
module Server (startApp) where
import qualified Api
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Generics (Generic(Rep))
import Network.HTTP.Simple (httpLBS, parseRequest)
import Network.HTTP.Client (Response(..))
import Network.HTTP.Types.Status (ok200)
import Network.URI.Encode (encode, encodeText)
import Network.Wai.Handler.Warp (run)
import Security (Token, chatID, isValidChat, token)
import Servant
  (Application, Handler, JSON, Post, Proxy(..), ReqBody, Server, serve,
  type (:>))
import Servant.Types.SourceT (source)
import System.Environment (getEnv)

#define DEBUG

-- принимаем только апдейты от телеги
type API = Token :> ReqBody '[JSON] Api.Update :> Post '[JSON] ()

-- удобна :)
data Env = Env {
  poll_name :: Text, 
  chat_user :: String,
  mesId     :: String,
  name      :: String
  }

-- надо энкодить название опроса, иначе пизда
encodeUrl :: String -> String
encodeUrl = encode . escape

-- надо эскепить символы в опросе, так сказал делать телеграм
escape :: String -> String
escape = concatMap one
  where
    ban :: String 
    ban = "_*[]()~>#+-=|{}.!"
    one :: Char -> String 
    one c = case c `elem` ban of  
      False -> [c]
      True -> "\\" ++ [c]

-- не делать же ансейв, но да для нас всегда приходит это поле 
-- (так я думаю на момент февраля)
chatUser :: Maybe Text -> String
chatUser Nothing     = "Nothing"
chatUser (Just text) = T.unpack text

-- базовая сылка телеги
stringTelegram :: String
stringTelegram = 
  "https://api.telegram.org/bot" ++ token ++
  "/sendMessage?chat_id=" ++ show chatID

-- сылка в чате
stringLink :: Env -> String 
stringLink Env{..} = 
  "(" ++ "https://t.me/" ++ chat_user ++ "/" ++ mesId ++ ")"

-- название опроса энкодированное и экранированное
stringText :: Env -> String
stringText env@Env{..} =
  ("%5B" ++ (encodeUrl $ T.unpack poll_name) ++ "%5D") ++ 
  stringLink env

-- реквест нормального человека
stringRequest :: Env -> String
stringRequest env@Env{..} = concat [
  stringTelegram,
  "&text=" ++ stringText env,
  "&parse_mode=MarkdownV2",
  "&disable_web_page_preview=True"]

-- если не удался первый реквест :(
badRequest :: Env -> String
badRequest env@Env{..} = concat [
  stringTelegram,
  "&text=" ++ "%5Bdefult%5D" ++ stringLink env,
  "&parse_mode=MarkdownV2",
  "&disable_web_page_preview=True"]

-- делаем реквест, если не удался отправляем двеолтный
-- + все логируем
createRequest :: Env -> Handler ()
createRequest env@Env{..} = do
  resp <- parseRequest request >>= httpLBS
  if (responseStatus resp /= ok200) then do
    bad <- parseRequest (badRequest env) >>= httpLBS
    trace ("Wrong: "     ++ show resp) $
      trace ("Resp Bad: "  ++ show bad) $ 
      trace ("url: "       ++ show request) $ 
      trace ("poll_name: " ++ T.unpack (encodeText poll_name)) $
      trace ("Resp: "      ++ show resp) $
      trace ("update with poll: " ++ name) $
      return ()
  else 
    trace ("url: "       ++ show request) $ 
    trace ("poll_name: " ++ T.unpack (encodeText poll_name)) $
    trace ("Resp: "      ++ show resp) $
    trace ("update with poll: " ++ name) $
    return ()
  where
    request = stringRequest env

-- пока принимаем только опросы + логируем
server :: Server API
server = isPoll where
  isPoll :: Api.Update -> Handler ()
  isPoll name | isValidChat name = case Api.message name of
    Nothing -> trace ("update not a message: "  ++ show name) $ return ()
    (Just mes) -> case Api.poll mes of
      Nothing -> trace ("update not a poll: "  ++ show name) $ return ()
      (Just (Api.Poll _ poll_name)) ->
        createRequest (Env poll_name
              (chatUser (Api.chat_username (Api.chat mes)))
              (show (Api.message_id mes))
              (show name))
  isPoll name = trace ("wrong update: "  ++ show name) $ return ()

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server

-- дебаг нужен для локального запуска, а вне дебага раним на хероку
runServer :: IO ()
runServer = do
  #ifdef DEBUG
  run 8443 app
  #else
  port <- read <$> getEnv "PORT"
  run port app
  #endif

startApp :: IO ()
startApp = runServer








