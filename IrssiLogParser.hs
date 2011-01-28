module IrssiLogParser where
import System.Environment
import Text.ParserCombinators.Parsec

data Nick = Nick String deriving Show
data ChanMode = ChanMode String deriving Show
data Date = Date String deriving Show
data Time = Time String String String
data DateTime = DateTime Date Time
instance Show Time where
    show (Time hour minute second) = hour ++ ":" ++ minute ++ ":" ++ second
instance Show DateTime where
    show (DateTime date time) = show date ++ "T" ++ show time

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
        second <- option "00" (try (char ':') >> count 2 digit) -- TODO: seconds
        many space
        return $ Time hour minute second

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
parseHostmask = many (alphaNum <|> oneOf "~@-.:/")

-- possible dateformats
-- Do Jul 01 00:00:35 2010
-- Fr Jul 02 2010
parseDateTime :: Parser DateTime
parseDateTime =
    do
        dayName <- count 2 letter; space
        monthName <- count 3 letter; space
        day <- count 2 digit; space
        time <- option (Time "00" "00" "00") (try parseTime)
        year <- count 4 digit
        -- TODO: verifiy the date by using lib functions
        return $ DateTime (Date (dayName ++ " " ++ monthName ++ " " ++ day ++ " " ++ year)) time

parseText :: Parser String
parseText = many (noneOf "\n")

parseTopic :: Parser String
parseTopic =
    do
        string " changed the topic of "
        channel <- many (alphaNum <|> oneOf "#.")
        string " to: "
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
            try (do space; char '['; hostmask <- parseHostmask; char ']'; string " has quit "; reason <- parseText; return $ Quit nick reason) <|>
            try (do space; char '['; hostmask <- parseHostmask; char ']'; string " has left "; reason <- parseText; return $ Part nick reason) <|>
            try (do space; string "is now known as "; newnick <- parseNick; return $ NickChange nick newnick) <|>
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
        (string "Day changed " >> parseDateTime >>= \(DateTime date time) -> return $ TimedEvent time (DayChanged date)) <|>
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
irssiParser = many (parseLine >>= \te -> newline >> return te)
