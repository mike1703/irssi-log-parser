module Main where
import System.Environment
import Text.ParserCombinators.Parsec

data Nick = Nick String deriving Show
data ChanMode = ChanMode String deriving Show
data Date = Date String deriving Show
data Time = Time String String -- "00:00" -> 00 00
data DateTime = DateTime Date Time | DateWOTime Date
instance Show Time where
    show (Time hour minute) = hour ++ ":" ++ minute
instance Show DateTime where
    show (DateTime date time) = show date ++ "T" ++ show time
    show (DateWOTime date) = show date

data Event = LogOpen DateTime| LogClose DateTime | DayChanged Date |
                Mode [Nick] ChanMode | NickAction Nick String |
                TopicChange Nick String | Kick Nick Nick String |
                NickChange Nick Nick | Join Nick String | Quit Nick String |
                Part Nick String | Message Nick String | NetSplit String |
                NotImplemented String

instance Show Event where
    show (LogOpen date) = "log opened on " ++ show date
    show (LogClose date) = "log closed on " ++ show date
    show (DayChanged date) = "day changed to " ++ show date
    show (Mode nicks mode) = "mode changed to " ++ show mode ++ " for " ++ show nicks
    show (NickAction nick action) = show nick ++ " " ++ show action
    show (TopicChange nick topic) = show nick ++ " changed the topic to " ++ show topic
    show (Kick kicker kicked reason) = show kicker ++ " kicked " ++ show kicked ++ " because " ++ show reason
    show (NickChange oldnick newnick) = show oldnick ++ " changed his nick to " ++ show newnick
    show (Join nick hostmask) = show nick ++ " joined"
    show (Quit nick reason) = show nick ++ " has quit because " ++ show reason
    show (Part nick reason) = show nick ++ " has parted because " ++ show reason
    show (Message nick text) = show nick ++ ": " ++ show text
    show (NetSplit info) = show "NetSplit " ++ info
    show (NotImplemented txt) = "NOT IMPLEMENTED YET: " ++ txt

data TimedEvent = TimedEvent Time Event
instance Show TimedEvent where
    show (TimedEvent time event) = show time ++ " " ++ show event
    
parseTime :: Parser Time
parseTime =
    do
        hour <- count 2 digit
        char ':'
        minute <- count 2 digit
        try (option "00" (char ':' >> count 2 digit)) -- TODO: seconds
        many space
        return $ Time hour minute

--special    =  %x5B-60 / %x7B-7D ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
nickSpecial = oneOf "[]\\'_^{|}" <|> oneOf "`"

parseMode :: Parser ChanMode
parseMode = oneOf " @%+&~" >>= \mode -> return $ ChanMode [mode]

--nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
parseNick :: Parser Nick
parseNick =
    do
        head <- letter <|> nickSpecial
        tail <- many1 (alphaNum <|> nickSpecial <|> char '-')
        return $ Nick (head : tail)

parseHostmask :: Parser String
parseHostmask = many (alphaNum <|> oneOf "~@-.")

parseDateTime :: Parser DateTime
parseDateTime =
    do
        dayName <- count 2 letter; space
        monthName <- count 3 letter; space
        day <- count 2 digit; space
        time <- try parseTime --option (Time "00" "00") parseTime
        year <- count 4 digit
        -- TODO: this is just a temporary place to scan the newline!
        newline
        -- TODO: verifiy the date by using lib functions
        return $ DateTime (Date (dayName ++ " " ++ monthName ++ " " ++ day ++ " " ++ year)) time

parseDate :: Parser DateTime
parseDate =
    do
        dayName <- count 2 letter; space
        monthName <- count 3 letter; space
        day <- count 2 digit; space
        year <- count 4 digit
        -- TODO: this is just a temporary place to scan the newline!
        newline
        -- TODO: verifiy the date by using lib functions
        return $ DateWOTime (Date (dayName ++ " " ++ monthName ++ " " ++ day ++ " " ++ year))

parseText :: Parser String
parseText = manyTill anyChar (try newline)

parseTopic :: Parser String
parseTopic =
    do
        many space
        string "changed the topic of "
        channel <- many (alphaNum <|> oneOf "#.")
        string "to: "
        text <- parseText
        return $ text

parseNetsplit :: Parser Event
parseNetsplit =
    (string "over, joins" >> parseText >>= \nicks -> return $ NetSplit nicks) <|>
        (parseText >>= \splitinfo -> return $ NetSplit splitinfo)

parseSpecialAction :: Parser Event
parseSpecialAction = 
    do
        nick <- parseNick
        try (parseTopic >>= \topic -> return $ TopicChange nick topic) <|>
            try (do space; char '['; hostmask <- parseHostmask; char ']'; string " has joined "; parseText; return $ Join nick hostmask) <|>
            (parseText >>= \dummy -> return $ NotImplemented dummy)

parseMessage :: Parser Event
parseMessage = 
    do
        string "<"
        mode <- parseMode
        nick <- parseNick
        string "> "
        message <- parseText
        return $ Message nick message

parseAction :: Parser Event
parseAction =
    do
        string "* "
        nick <- parseNick
        action <- parseText
        return $ NickAction nick action

parseSpecialEvent :: Parser TimedEvent
parseSpecialEvent =
    do
        string "--- "
        (string "Day changed " >> parseDate >>= \(DateWOTime date) -> return $ TimedEvent (Time "00" "00") (DayChanged date)) <|>
            try (string "Log opened " >> parseDateTime >>= \(DateTime date time) -> return $ TimedEvent time (LogOpen $ DateTime date time)) <|>
            (string "Log closed " >> parseDateTime >>= \(DateTime date time) -> return $ TimedEvent time (LogClose $ DateTime date time))

parseNormalEvent :: Parser TimedEvent
parseNormalEvent =
    do
        time <- parseTime;
        event <- (try( string "-!- Netsplit" >> parseNetsplit) <|> (string "-!- " >> parseSpecialAction) <|> parseAction <|> parseMessage)
        return $ TimedEvent time event
        
parseLine :: Parser TimedEvent
parseLine = parseSpecialEvent <|> parseNormalEvent

irssiParser :: Parser [TimedEvent]
irssiParser = many parseLine

main :: IO ()
main =
    do
        args <- getArgs
        result <- parseFromFile irssiParser (args !! 0)
        case (result) of
            Left err  -> print err
            Right val  -> putStr $ unwords (fmap ((++ "\n") . show) (take 10 val))
