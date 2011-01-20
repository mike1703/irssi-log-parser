module Main where
import System.Environment
import Text.ParserCombinators.Parsec

type Date = String
type Nick = String
type Mode = String
data Time = Time String String -- "00:00" -> 00 00
instance Show Time where show (Time hour minute) = hour ++ ":" ++ minute

data Event = LogOpen | LogClose | DayChanged Date |
                Mode [Nick] Mode | NickAction Nick String |
                TopicChange Nick String | Kick Nick Nick String |
                NickChange Nick Nick | Join Nick | Quit Nick String |
                Part Nick String | Message Nick String

instance Show Event where
    show LogOpen = "log opened"
    show LogClose = "log closed"
    show (DayChanged date) = "day changed to " ++ date
    show (Mode nicks mode) = "mode changed to " ++ mode ++ " for " ++ show nicks
    show (NickAction nick action) = nick ++ " " ++ action
    show (TopicChange nick topic) = nick ++ " changed the topic to " ++ topic
    show (Kick kicker kicked reason) = kicker ++ " kicked " ++ kicked ++ " because " ++ reason
    show (NickChange oldnick newnick) = oldnick ++ " changed his nick to " ++ newnick
    show (Join nick) = nick ++ " joined"
    show (Quit nick reason) = nick ++ " has quit because " ++ reason
    show (Part nick reason) = nick ++ " has parted because " ++ reason
    show (Message nick text) = nick ++ ": " ++ text

data TimedEvent = IrcEvent Time Event

parseTime :: Parser Time
parseTime = do
                hour <- count 2 digit
                char ':'
                minute <- count 2 digit
                return $ Time hour minute

--special    =  %x5B-60 / %x7B-7D ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
nickSpecial = oneOf "[]\\'_^{|}"

--nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
parseNick :: Parser Nick
parseNick = do
                head <- letter <|> nickSpecial
                tail <- many1 (alphaNum <|> nickSpecial <|> char '-')
                return (head : tail)
              

irssiParser = parseNick

readLine :: String -> String
readLine input = case parse irssiParser "irssi log" input of
    Left err -> "No match: " ++ show err
    Right val -> "Yeah, found data: " ++ show val


main :: IO ()
main = do
        args <- getArgs
        putStrLn (readLine (args !! 0))
