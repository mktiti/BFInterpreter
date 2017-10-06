import Data.Array
import Data.Ix
import Data.Char
import Data.Word
import Data.List

data Command = IncrementPtr | 
    DecrementPtr |
    Increment |
    Decrement |
    Output |
    Store |
    IfJump Int |
    JumpBack Int deriving (Show)

type Code = Array Int Command
--type Memory = Array Int Word8
data Memory = Memory { left :: [Word8], current :: Word8, right :: [Word8] }

moveLeft :: Memory -> Memory
moveLeft mem = Memory { 
    left = tail $ left mem,
    current = head $ left mem,
    right = (current mem):(right mem) }

moveRight :: Memory -> Memory
moveRight mem = Memory { 
    left = (current mem):(left mem),
    current = head $ right mem,
    right = tail $ right mem }

data State = State { 
    mem :: Memory,  
    cmdPtr :: Int, 
    printReq :: Bool, 
    readReq :: Bool }

createMemory :: Memory
createMemory = Memory [] 0 zeroes
    where zeroes = 0:zeroes

createState ::  State
createState = State { 
    mem = createMemory, 
    cmdPtr = 0, 
    printReq = False, 
    readReq = False
}

execute :: Command -> State -> State
execute IncrementPtr s = s { mem = moveRight (mem s) }
execute DecrementPtr s = s { mem = moveLeft (mem s) }
execute Increment s = s { mem = (mem s) { current = current (mem s) + 1 } }
execute Decrement s = s { mem = (mem s) { current = current (mem s) - 1 } }
execute Output s = s { printReq = True }
execute Store s = s { readReq = True }
execute (IfJump ptr) s = if current (mem s) == 0 then s { cmdPtr = ptr } else s
execute (JumpBack ptr) s = s { cmdPtr = ptr - 1 }

postExecute :: State -> State
postExecute state = state { printReq = False, readReq = False, cmdPtr = (cmdPtr state) + 1 }

main = do
    codeLine <- getLine
    let code = parseCommands $ intersect codeLine "<>+-.,[]"
        end = length code
    run createState code end

run :: State -> Code -> Int -> IO ()
run state code cl
    | cmdPtr state == cl = return ()
    | otherwise = do
        let state' = execute (code ! (cmdPtr state)) state
        if printReq state' then putChar (chr $ (fromIntegral (current $ mem state) :: Int)) else return ()
        state'' <- if readReq state' then do
            input <- getChar
            return $ state' { mem = (mem state') { current = (fromIntegral $ ord input) :: Word8} }
        else return state'
        run (postExecute state'') code cl

simpleCommands :: [(Char, Command)]
simpleCommands = [('>', IncrementPtr),
    ('<', DecrementPtr),
    ('+', Increment),
    ('-', Decrement),
    ('.', Output),
    (',', Store)]

getSimpleCommand :: Char -> Maybe Command
getSimpleCommand c = fmap snd $ find ((== c) . fst) simpleCommands

parseCommands :: [Char] -> Code
parseCommands s = listArray (0, length ls - 1) ls
    where ls = reverse $ parse' s []

parse' :: [Char] -> [Command] -> [Command]
parse' [] cs = cs
parse' (ch:chs) cs = case getSimpleCommand ch of
    Just c -> parse' chs (c:cs)
    Nothing -> case getJumping ch chs cs of
        Just c -> parse' chs (c:cs)
        Nothing -> error "illegal jump ([])"

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

