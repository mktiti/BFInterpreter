import Data.Array
import Data.Ix
import Data.Char
import Data.Int

data Command = IncrementPtr | 
    DecrementPtr |
    Increment |
    Decrement |
    Output |
    Store |
    IfJump Int |
    JumpBack Int deriving (Show)

type Memory = Array Int Int8

data State = State { 
    code :: [Command],
    mem :: Memory, 
    memPtr :: Int, 
    cmdPtr :: Int, 
    printReq :: Bool, 
    readReq :: Bool } deriving (Show)

createMemory :: Memory
createMemory = array (0, 1000) [(i, 0) | i <- [0..1000]]

createState :: [Command] -> State
createState code = State { 
    code = code,
    mem = createMemory, 
    memPtr = 0,
    cmdPtr = 0, 
    printReq = False, 
    readReq = False
}

execute :: Command -> State -> State
execute IncrementPtr state = state { memPtr = (memPtr state) + 1 }
execute DecrementPtr state = state { memPtr = (memPtr state) - 1 }
execute Increment s = s { mem = mem s // [(memPtr s, (mem s ! memPtr s) + 1)] }
execute Decrement s = s { mem = mem s // [(memPtr s, (mem s ! memPtr s) - 1)] }
execute Output state = state { printReq = True }
execute Store state = state { readReq = True }
execute (IfJump ptr) state = if mem state ! memPtr state == 0 then state { cmdPtr = ptr } else state
execute (JumpBack ptr) state = state { cmdPtr = ptr - 1 }

postExecute :: State -> State
postExecute state = state { printReq = False, readReq = False, cmdPtr = (cmdPtr state) + 1 }

main = do
    codeLine <- getLine
    let code = parseCommands codeLine
        end = length code
--    putStrLn $ show code
    run (createState code) end

run :: State -> Int -> IO ()
run state cl
    | cmdPtr state == cl = return ()
    | otherwise = do
        let state' = execute ((code state) !! (cmdPtr state)) state
        if printReq state' then putChar (chr $ (fromIntegral (mem state ! memPtr state) :: Int)) else return ()
        state'' <- if readReq state' then do
            input <- getChar
            return $ state' { mem = mem state' // [(memPtr state', (fromIntegral $ ord input) :: Int8)] }
        else return state'
        run (postExecute state'') cl

simpleCommands :: [(Char, Command)]
simpleCommands = [('>', IncrementPtr),
    ('<', DecrementPtr),
    ('+', Increment),
    ('-', Decrement),
    ('.', Output),
    (',', Store)]

getSimpleCommand :: Char -> Maybe Command
getSimpleCommand c = findCommand c simpleCommands
    where
        findCommand c [] = Nothing
        findCommand c ((ch, comm):xs) = if c == ch then Just comm else findCommand c xs

parseCommands :: [Char] -> [Command]
parseCommands s = reverse $ parse' s []

parse' :: [Char] -> [Command] -> [Command]
parse' [] cs = cs
parse' (ch:chs) cs = case getSimpleCommand ch of
    Just c -> parse' chs (c:cs)
    Nothing -> case getJumping ch chs cs of
        Just c -> parse' chs (c:cs)
        Nothing -> parse' chs cs

getJumping :: Char -> [Char] -> [Command] -> Maybe Command
getJumping '[' chs cs = fmap (\ptr -> IfJump $ length cs + ptr + 1) $ findClosing chs
getJumping ']' _ cs = fmap JumpBack $ findOpening cs $ length cs
getJumping _ _ _ = Nothing

findOpening :: [Command] -> Int -> Maybe Int
findOpening cs pos = getMatch cs (length cs - 1) 
    where
        getMatch [] _ = Nothing
        getMatch ((IfJump ifPtr):xs) ptr = if ifPtr == pos then Just ptr else getMatch xs (ptr - 1)
        getMatch (x:xs) ptr = getMatch xs (ptr - 1)

findClosing :: [Char] -> Maybe Int
findClosing s = closing 0 1 s
    where 
        closing ptr 1 (']':xs) = Just ptr
        closing ptr n (']':xs) = closing (ptr + 1) (n - 1) xs
        closing ptr n ('[':xs) = closing (ptr + 1) (n + 1) xs
        closing ptr n (x:xs) = closing (ptr + 1) n xs
        closing _ _ [] = Nothing

