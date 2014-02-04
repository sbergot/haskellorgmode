{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Control.Applicative hiding (many)
import System.Environment

type Paragraph = T.Text
type NodeText = [Paragraph]
type Tag = T.Text
type Tags = [Tag]
type Status = T.Text

data Outline = Outline
    { _olTitle    :: T.Text
    , _olStatus   :: Maybe Status
    , _olTags     :: Tags
    , _olText     :: NodeText
    , _olChildren :: [Outline]
    } deriving (Show)

type Header = T.Text

data OrgDoc = OrgDoc
    { _odHeader   :: Header
    , _odOutline  :: Outline
    } deriving (Show)

tilleol :: Parser T.Text
tilleol = T.pack <$> (many $ noneOf "\n")

outlineParser :: [Status] ->  Int -> Parser Outline
outlineParser userStatus level = do
    replicateM_ level $ char '*'
    space
    status <- optionMaybe $ T.pack <$> (choice $ map (try . string . T.unpack) userStatus)
    title <- T.pack <$> (many $ noneOf ":\n")
    tags <- sepBy tagParser spaces
    newline
    text <- nodeTextParser level
    children <- many $ try $ outlineParser userStatus (level + 1)
    return $ Outline title status tags text children

tagParser :: Parser Tag
tagParser = do
    char ':'
    tag <- many letter
    char ':'
    return $ T.pack tag

paragraphParser :: Int -> Parser Paragraph
paragraphParser level = T.unlines <$> many line where
    line = do
        replicateM_ (level + 1) $ char ' '
        ws <- tilleol
        newline
        return ws

nodeTextParser :: Int -> Parser NodeText
nodeTextParser level = sepBy (paragraphParser level) (many1 newline)

headerParser :: Parser Header
headerParser = T.concat <$> sepBy hline newline where
    hline = option "" (char '#' >> tilleol)

orgDocParser :: [Status] ->  Int -> Parser OrgDoc
orgDocParser userStatus level = OrgDoc <$> headerParser <*> outlineParser userStatus level

parseOrgDoc :: [Status] -> String -> T.Text -> Either ParseError OrgDoc
parseOrgDoc userStatus sourceName inp = parse (orgDocParser userStatus 1) sourceName inp

main = do
    (src:_) <- getArgs
    t <- TIO.readFile src
    case parseOrgDoc ["TODO"] src t of
        Left err -> print err
        Right val -> print $ _odOutline val
