{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import System.Random
import Control.Monad
import Control.Monad.State
import Network.HTTP.Conduit
import Network.HTTP.Client (defaultManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Data.Char as Char
import Data.List as D
import qualified Data.Text as T
import Network (withSocketsDo)

-- почтовый адрес
email = ""

data JSON = 
        Object [(String, JSON)]
        | String String
        | Number Double
        | Null
        | Boolean Bool
        | Array [JSON]
        
data Token =
    TOpenBrace | TCloseBrace
    | TOpenBracket | TCloseBracket
    | TComma | TColon
    | TString String
    | TNumber Double
    | TBoolean Bool
    | TNull
    deriving (Show)
    
tokenize :: String -> [Token]    
tokenize = tokenize' []
    where
        tokenize' acc [] = reverse acc
        tokenize' acc ('{' : xs) = tokenize' (TOpenBrace : acc) xs
        tokenize' acc ('}' : xs) = tokenize' (TCloseBrace : acc) xs
        tokenize' acc ('[' : xs) = tokenize' (TOpenBracket : acc) xs
        tokenize' acc (']' : xs) = tokenize' (TCloseBracket : acc) xs
        tokenize' acc (',' : xs) = tokenize' (TComma : acc) xs
        tokenize' acc (':' : xs) = tokenize' (TColon : acc) xs
        tokenize' acc ('n':'u':'l':'l' : xs) = tokenize' (TNull : acc) xs
        tokenize' acc ('t':'r':'u':'e' : xs) = tokenize' (TBoolean True : acc) xs
        tokenize' acc ('f':'a':'l':'s':'e' : xs) = tokenize' (TBoolean False : acc) xs
        tokenize' acc ('"' : xs) = tokenize' (TString str : acc) left
            where
                (str, left) = tokenizeString [] xs
                tokenizeString acc ('"' : tl) = (reverse acc, tl)
                tokenizeString acc ('\\' : '"' : tl) = tokenizeString ('\"' : acc) tl
                tokenizeString acc (ch : tl) = tokenizeString (ch : acc) tl
                tokenizeString acc [] = error "Invalid string"
        tokenize' acc (ch : xs)
            | Char.isSpace ch = tokenize' acc xs
            | Char.isDigit ch = tokenize' (TNumber (read num :: Double) : acc) left
                where
                    (num, left) = tokenizeNumber [] (ch : xs)
                    tokenizeNumber acc (hd : tl)
                        | hd == '.' = tokenizeNumber (hd : acc) tl
                        | Char.isDigit hd = tokenizeNumber (hd : acc) tl
                        | hd `elem` ['}', ']', ','] = (reverse acc, (hd : tl))
                        | otherwise = error "Invalid number"
        tokenize' _ _ = error "Invalid JSON string"     
        
test1 = "[\"ab\\\"c\", null, true, false]"
test2 = "{\"a\":1}"
test3 = "{\"menu\": {\"header\": \"SVG Viewer\",\"items\": [{\"id\": \"Open\"},{\"id\": \"OpenNew\", \"label\": \"Open New\"},null,{\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},{\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},{\"id\": \"OriginalView\", \"label\": \"Original View\"},null,{\"id\": \"Quality\"},{\"id\": \"Pause\"},{\"id\": \"Mute\"},null,{\"id\": \"Find\", \"label\": \"Find...\"},{\"id\": \"FindAgain\", \"label\": \"Find Again\"},{\"id\": \"Copy\"},{\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},{\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},{\"id\": \"ViewSVG\", \"label\": \"View SVG\"},{\"id\": \"ViewSource\", \"label\": \"View Source\"},{\"id\": \"SaveAs\", \"label\": \"Save As\"},null,{\"id\": \"Help\"},{\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}]}}"
test4 = "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"stroke\":\"#555555\",\"stroke-width\":2,\"stroke-opacity\":1,\"fill\":\"#555555\",\"fill-opacity\":0.5,\"abc\":1,\"kjh\":\"klhkh\"},\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[38.12255859375,34.58799745550482],[38.49609375,34.17090836352573],[39.627685546875,34.57895241036948],[39.0673828125,35.53222622770337],[37.452392578125,35.092945313732635],[38.12255859375,34.58799745550482]]]}},{\"type\":\"Feature\",\"properties\":{\"marker-color\":\"#7e7e7e\",\"marker-size\":\"medium\",\"marker-symbol\":\"\",\"asdsad\":true},\"geometry\":{\"type\":\"Point\",\"coordinates\":[40.341796875,35.11990857099681]}}]}"

parse :: [Token] -> JSON
parse tks = fst $ parse' tks
    where
        parse' (TNull : xs) = (Null, xs)
        parse' (TBoolean bool : xs) = (Boolean bool, xs)
        parse' (TString str : xs) = (String str, xs)
        parse' (TNumber num : xs) = (Number num, xs)
        parse' (TOpenBrace : xs) = (Object obj, left)
            where
                (obj, left) = parseObject [] xs
                parseObject acc (TCloseBrace : tl) = (reverse acc, tl)
                parseObject acc (TComma : tl) = parseObject acc tl
                parseObject acc (TString key : TColon : tl) = parseObject ((key, json) : acc) left
                    where
                        (json, left) = parse' tl
                parseObject _ _ = error "Error in parsing object"        
        parse' (TOpenBracket : xs) = (Array arr, left)
            where
                (arr, left) = parseArray [] xs
                parseArray acc (TCloseBracket : tl) = (reverse acc, tl)
                parseArray acc (TComma : tl) = parseArray acc tl
                parseArray acc tkns = parseArray (json : acc) left
                    where
                        (json, left) = parse' tkns
   
parseJSON :: String -> JSON   
parseJSON = parse . tokenize   
        
-- добавим сответствующие классы типов для JSON
instance Show JSON where
  show = stringify

instance Read JSON where
  readsPrec _ x = readJSON x

readJSON :: ReadS (JSON)
readJSON str = [(parseJSON str, "")]

lab3 (Object list) = 0

stringify (Object list) = "{ " ++ (D.intercalate ", " $ map (\(key, value) -> "\"" ++ key ++ "\" : " ++ stringify value) list) ++ " }"
stringify Null = "null"
stringify (Boolean True) = "true"
stringify (Boolean False) = "false"
stringify (Number num) = show num
stringify (String str) = "\"" ++ str ++ "\""
stringify (Array list) = "[ " ++ (D.intercalate ", " $ map stringify list) ++ " ]"


-- вариант с монадой IO
generateIO :: IO JSON
generateIO = do
  num <- randomRIO (1, 2) :: IO Int
  let json = case num of
               1 -> Object [];
               2 -> Object [("io", Object [])]
  return json

-- чистый вариант с генератором, заключённым в состояние
-- мы храним в состоянии генератор, каждый раз используя
-- его, возвращаем в состояние новый

type GeneratorState = State StdGen

generate' :: GeneratorState JSON
generate' = do
  gen <- get
  let (num, newGen) = randomR (1, 2) gen :: (Int, StdGen)
  let json = case num of
               1 -> Object [];
               2 -> Object [("pure", Object [])]
  put newGen
  return json

generate :: JSON
generate = evalState generate' (mkStdGen 0)

main :: IO()
main = withSocketsDo $ do
  dir <- getCurrentDirectory
  initReq <- parseRequest "POST http://91.239.142.110:13666/lab2"
  handle <- openFile (dir ++ "/Lab2.hs") ReadMode
  hSetEncoding handle utf8_bom
  content <- hGetContents handle
  let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq
  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  hClose handle
  L.putStrLn $ responseBody response
