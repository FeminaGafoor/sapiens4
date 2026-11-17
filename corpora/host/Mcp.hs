module Mcp (mcpMain) where

import Types
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status (statusCode)
import System.IO

data Endpoint = Endpoint String H.Manager

data Tool = Tool Text Text [(Text,Value)] (Endpoint -> Value -> IO Value)

mcpMain :: String -> IO ()
mcpMain url = do
 manager <- H.newManager H.defaultManagerSettings
 let endpoint = Endpoint url manager
     loop = do
      done <- hIsEOF stdin
      unless done $ do
       line <- B.hGetLine stdin
       unless (B.null (B.strip line)) $ do
        reply <- either (const (pure (Just (rpc Null (Left (-32700, "Parse error")))))) (answer endpoint) (eitherDecodeStrict line)
        forM_ reply $ \value -> BL.putStrLn (encode value) >> hFlush stdout
       loop
 loop

rpc :: Value -> Either (Int,Text) Value -> Value
rpc rid outcome = obj (("jsonrpc",str "2.0") : ("id",rid) : either (\(code,message) -> [("error",obj [("code",num code),("message",str message)])]) (\result -> [("result",result)]) outcome)

answer :: Endpoint -> Value -> IO (Maybe Value)
answer endpoint message
 | not (has message "id" && has message "method") = pure Nothing
 | otherwise = Just . rpc (get message "id") <$> case at message "method" of
  "initialize" -> pure (Right (obj [("protocolVersion",str version),("capabilities",obj [("tools",obj [])]),("serverInfo",obj [("name",str "sapiens4"),("version",str "1")])]))
  "ping" -> pure (Right (obj []))
  "tools/list" -> pure (Right (obj [("tools",arr (map schema tools))]))
  "tools/call" -> Right <$> callTool endpoint (get message "params")
  _ -> pure (Left (-32601, "Method not found"))
 where
 asked = at (get message "params") "protocolVersion"
 version = if asked `elem` ["2025-11-25","2025-06-18","2025-03-26"] then asked else "2025-11-25"

tools :: [Tool]
tools =
 [ Tool "corpora" "List the local and remote corpora this Sapiens4 host can reach, with id, name, location and status." [] $ \endpoint _ -> do
    host <- request endpoint "GET" "/api/host" Nothing
    pure (arr [select ["id","name","location","status"] row | row <- items host "corpora"])
 , Tool "sapis" "List the Sapis of one corpora with role, busy state and open workspace tabs." [corpora] $ \endpoint args -> do
    state <- within endpoint args "GET" "/api/state" Nothing
    let tabs owner = [select ["id","title","url","path"] tab | tab <- items (get (get (get state "preferences") "workspaces") owner) "tabs"]
    pure (arr [merge (select ["id","name","role"] row) (obj [("chief",Bool (get row "id" == get state "main_agent_id")),("busy",Bool (any (working (at row "id")) (items state "turns"))),("tabs",arr (tabs (at row "id")))]) | row <- items state "agents", get row "retired" /= Bool True])
 , Tool "chat" "Read the latest turns of one Sapi (input, output, status). Use it to follow a reply after send; with wait it first waits until the Sapi has no queued or running turn." [corpora,sapi,("limit",property "integer" "Number of turns, 1 to 50, default 10"),("wait",property "integer" "Seconds to wait for running work to finish, 0 to 120, default 0")] $ \endpoint args -> do
    owner <- ident args "sapi"
    let limit = let n = int (get args "limit") in if n <= 0 then 10 else min 50 n
        settle remaining = do
         state <- within endpoint args "GET" "/api/state" Nothing
         if any (working owner) (items state "turns") && remaining > 0 then threadDelay 1000000 >> settle (remaining - 1) else pure state
    state <- settle (max 0 (min 120 (int (get args "wait"))))
    let own = [select ["id","status","input","output","error","created"] turn | turn <- items state "turns", at turn "agent" == owner]
    pure (arr (drop (length own - limit) own))
 , Tool "send" "Send a message to one Sapi. It starts work and returns the queued turn id; read the reply with chat, using wait to block until it finishes." [corpora,sapi,("text",property "string" "The message")] $ \endpoint args -> do
    owner <- ident args "sapi"
    require (not (T.null (T.strip (at args "text")))) 400 "Provide the message text"
    within endpoint args "POST" ("/api/agents/" <> owner <> "/messages") (Just (obj [("text",get args "text")]))
 , Tool "lifecycle" "Pause or activate one corpora. Pausing stops running work." [corpora,("action",merge (property "string" "pause or activate") (obj [("enum",arr [str "pause",str "activate"])]))] $ \endpoint args -> do
    current <- within endpoint args "GET" "/api/corpora" Nothing
    operation <- newId
    within endpoint args "PUT" "/api/corpora/lifecycle" (Just (obj [("operation",str operation),("action",get args "action"),("revision",get current "revision")]))
 ]
 where
 property kind about = obj [("type",str kind),("description",str about)]
 corpora = ("corpora",property "string" "Corpora id from the corpora tool")
 sapi = ("sapi",property "string" "Sapi id from the sapis tool")

schema :: Tool -> Value
schema (Tool name about fields _) = obj [("name",str name),("description",str about),("inputSchema",obj [("type",str "object"),("properties",obj fields),("required",arr [str k | (k,_) <- fields, k `notElem` ["limit","wait"]])])]

callTool :: Endpoint -> Value -> IO Value
callTool endpoint params = do
 outcome <- try (case [act | Tool name _ _ act <- tools, name == at params "name"] of
  act : _ -> act endpoint (get params "arguments")
  [] -> failure 400 "Unknown tool") :: IO (Either SomeException Value)
 pure $ case outcome of
  Right value -> content False (jsonText value)
  Left problem -> content True (maybe ("Sapiens4 host request failed: " <> T.pack (take 300 (displayException problem))) (\(APIError _ reason) -> reason) (fromException problem))
 where content bad body = obj [("content",arr [obj [("type",str "text"),("text",str body)]]),("isError",Bool bad)]

ident :: Value -> Text -> IO Text
ident args key = let value = at args key in require (not (T.null value) && T.all (\c -> isAlphaNum c || c `elem` ("-_" :: String)) value) 400 ("Provide a valid " <> key <> " id") >> pure value

within :: Endpoint -> Value -> Text -> Text -> Maybe Value -> IO Value
within endpoint args method path body = do
 corpora <- ident args "corpora"
 request endpoint method ("/api/host/corpora/" <> corpora <> path) body

working :: Text -> Value -> Bool
working owner turn = at turn "agent" == owner && case fromJSON (get turn "status") of Success status -> activeStatus status; _ -> False

request :: Endpoint -> Text -> Text -> Maybe Value -> IO Value
request (Endpoint url manager) method path body = do
 base <- H.parseRequest (url <> T.unpack path)
 response <- H.httpLbs base { H.method = TE.encodeUtf8 method, H.requestHeaders = maybe [] (const [("Content-Type","application/json"),("X-Sapiens-Local","1")]) body, H.requestBody = H.RequestBodyLBS (maybe "" encode body), H.responseTimeout = H.responseTimeoutMicro 30000000, H.redirectCount = 0 } manager
 let code = statusCode (H.responseStatus response)
     value = fromMaybe Null (decode (H.responseBody response))
 when (code < 200 || code >= 300) $ failure code (if T.null (at value "error") then "Sapiens4 host answered HTTP " <> T.pack (show code) else at value "error")
 pure value
