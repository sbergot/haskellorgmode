{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OrgMode where

import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Control.Applicative hiding (many)

import Control.Lens (makeLenses)
import Data.Aeson.TH

data TextBlock = Paragraph T.Text | ListBlock [ListEntry]
    deriving (Show)

data ListEntry = ListEntry
    { _leTitle :: T.Text
    , _leContent :: [TextBlock]
    }
    deriving (Show)

makeLenses ''ListEntry
deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''ListEntry
makeLenses ''TextBlock
deriveJSON (defaultOptions) ''TextBlock

type Tag = T.Text
type Status = T.Text
type Date = T.Text
type Property = (T.Text, T.Text)

data Outline = Outline
    { _olTitle    :: T.Text
    , _olStatus   :: Maybe Status
    , _olCloseDate :: Maybe Date
    , _olProperties :: [Property]
    , _olTags     :: [Tag]
    , _olText     :: [TextBlock]
    , _olChildren :: [Outline]
    } deriving (Show)
makeLenses ''Outline
deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''Outline

type Header = T.Text

data OrgDoc = OrgDoc
    { _odHeader   :: Header
    , _odOutline  :: Outline
    } deriving (Show)
makeLenses ''OrgDoc
deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''OrgDoc

lvlPrefix :: Int -> Parser ()
lvlPrefix level = replicateM_ (level + 1) $ char_ ' '

tilleol :: Parser T.Text
tilleol = T.pack <$> (many $ noneOf "\n")

noOut :: Monad m => m a -> m ()
noOut = (>> return ())

space_ :: Parser ()
space_ = noOut space

char_ :: Char -> Parser ()
char_ c = noOut $ char c

string_ :: String -> Parser ()
string_ s = noOut $ string s

newline_ :: Parser ()
newline_ = noOut newline

propParser :: Int -> Parser [Property]
propParser level = do
    key "PROPERTIES"
    props <- manyTill singleProp (try $ key "END")
    return props
  where
    key name = do
        lvlPrefix level
        char_ ':'
        string_ name
        char_ ':'
        newline_
    singleProp = do
        lvlPrefix level
        char_ ':'
        name <- many1 (noneOf ": ")
        string_ ": "
        val <- tilleol
        newline_
        return (T.pack name, val)

outlineParser :: [Status] ->  Int -> Parser Outline
outlineParser userStatus level = do
    replicateM_ level $ char_ '*'
    space_
    status <- optionMaybe $ T.pack <$> do
        st <- (choice $ map (try . string . T.unpack) userStatus)
        space_
        return st
    title <- T.pack <$> (many $ noneOf ":\n")
    tags <- option [] $ try tagParser
    newline_
    closeDate <- optionMaybe $ try $ closeDateParser level
    props <- option [] (try $ propParser level)
    _ <- many newline_
    text <- grpLists <$> (nodeTextParser level)
    spaces
    children <- many $ try $ outlineParser userStatus (level + 1)
    return $ Outline (T.strip title) status closeDate props tags text children

grpLists :: [TextBlock] -> [TextBlock]
grpLists blocks = go [] blocks where
    go laccum bs = case bs of
        (ListBlock [le]):rest -> go (le:laccum) rest
        b:rest -> flush ++ (b : go [] rest)
        [] -> flush
      where
        flush = case laccum of
            [] -> []
            _  -> [ListBlock $ reverse laccum]

tagParser :: Parser [Tag]
tagParser = do
    char_ ':'
    tags <- sepEndBy (many1 (noneOf " :\n")) (char_ ':')
    return $ T.pack <$> tags

paragraphParser :: Int -> Parser TextBlock
paragraphParser level = Paragraph <$> (T.strip . T.unlines) <$> many1 line where
    line = do
        lvlPrefix level
        ws <- tilleol
        newline_
        return ws

closeDateParser :: Int -> Parser Date
closeDateParser level = do
    lvlPrefix level
    string_ "CLOSED: ["
    date <- many1 $ noneOf "]\n"
    char_ ']'
    newline_
    return $ T.pack date

listEntryParser :: Int -> Parser ListEntry
listEntryParser level = do
    lvlPrefix level
    string_ "- "
    title <- tilleol
    content <- nodeTextParser (level + 2)
    return $ ListEntry title content

textBlockParser :: Int -> Parser TextBlock
textBlockParser level = choice
    [ try $ (\le -> ListBlock [le]) <$> (listEntryParser level)
    , paragraphParser level
    ]

nodeTextParser :: Int -> Parser [TextBlock]
nodeTextParser level = sepEndBy (textBlockParser level) (many1 newline_)

headerParser :: Parser Header
headerParser = T.concat <$> sepBy hline newline_ where
    hline = option "" (char_ '#' >> tilleol)

orgDocParser :: [Status] ->  Int -> Parser OrgDoc
orgDocParser userStatus level = OrgDoc <$> headerParser <*> outlineParser userStatus level

parseOrgDoc :: [Status] -> String -> T.Text -> Either ParseError OrgDoc
parseOrgDoc userStatus srcName inp = parse (orgDocParser userStatus 1) srcName inp

parseOrgFile :: String -> [Status] -> IO (Either ParseError OrgDoc)
parseOrgFile path tags = do
    SIO.withFile path SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        t <- TIO.hGetContents h
        return $ parseOrgDoc tags path t
