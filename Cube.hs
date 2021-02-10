import Data.List 

-- Basic types for cube
data Action = RO | RP | R2 | LO | LP | L2 | UO | UP | U2 | DO | DP | D2 | FO | FP | F2 | BO | BP | B2 | OO deriving (Eq, Read, Show, Enum)
data Colour = R | O | Y | G | B | W deriving (Eq, Read, Show, Enum)

type Row = [Colour]
type Side = [Row]
type Cube = [Side]


solvedR :: Row -> Bool
solvedR xs = all (== head xs) xs         -- | Row is solved if all elements are equal to first one

solvedS :: Side -> Bool
solvedS xs = and (zipWith (&&) ( map solvedR xs ) (map (== head xs) xs) )  -- | Side is solved if all rows are solved AND rows are the same

solvedC :: Cube -> Bool
solvedC = all solvedS                      -- | Cube is solved if all sides are solved AND rows are the same

-- Utility functions for creation/debugging etc.
plainRow :: Colour -> Row
plainRow = replicate 3 

plainSide :: Colour -> Side
plainSide x = replicate 3 (plainRow x)

plainCube :: Cube
plainCube = map plainSide [ R .. W ]

showCube :: Cube -> IO()
showCube xs = do 
    putStrLn ("        " ++ show (xs !! 0 !! 0))
    putStrLn ("        " ++ show (xs !! 0 !! 1))
    putStrLn ("        " ++ show (xs !! 0 !! 2))
    putStrLn (show (xs !! 1 !! 0) ++ " " ++ show (xs !! 2 !! 0) ++ " " ++ show (xs !! 3 !! 0) ++ " " ++ show (xs !! 4 !! 0))
    putStrLn (show (xs !! 1 !! 1) ++ " " ++ show (xs !! 2 !! 1) ++ " " ++ show (xs !! 3 !! 1) ++ " " ++ show (xs !! 4 !! 1))
    putStrLn (show (xs !! 1 !! 2) ++ " " ++ show (xs !! 2 !! 2) ++ " " ++ show (xs !! 3 !! 2) ++ " " ++ show (xs !! 4 !! 2))
    putStrLn ("        " ++ show (xs !! 5 !! 0))
    putStrLn ("        " ++ show (xs !! 5 !! 1))
    putStrLn ("        " ++ show (xs !! 5 !! 2))


f = [plainRow R, [R, G, B], plainRow R]
cc = [f] : map plainSide [ O .. W ]

-- Basic Functions for acting on cubes

-- | Rotate functions spin a side
rotateSP :: Side -> Side
rotateSP xs = reverse $ transpose xs

rotateS :: Side -> Side
rotateS xs = rotateSP $ rotateSP $ rotateSP xs

rotateS2 :: Side -> Side
rotateS2 xs = rotateSP $ rotateSP xs

-- | spin does the heavy lifting of shuffling the rows and columns "round the corner" when you turn a face
--      (shame, I can't seem to think of a more elegant way of doing this)
spin :: [Side] -> Action -> [Side]
spin xs UO = let ss0 = xs !! 0
                 ss1 = xs !! 1
                 ss2 = xs !! 2
                 ss3 = xs !! 3
    in [[ss1 !! 0, ss0 !! 1, ss0 !! 2], [ss2 !! 0, ss1 !! 1, ss1 !! 2], [ss3 !! 0, ss2 !! 1, ss2 !! 2], [ss0 !! 0, ss3 !! 1, ss3 !! 2]]
spin xs DO = let ss0 = xs !! 0
                 ss1 = xs !! 1
                 ss2 = xs !! 2
                 ss3 = xs !! 3
    in [[ss0 !! 0, ss0 !! 1, ss3 !! 2], [ss1 !! 0, ss1 !! 1, ss0 !! 2], [ss2 !! 0, ss2 !! 1, ss1 !! 2], [ss3 !! 0, ss3 !! 1, ss2 !! 2]]
spin xs FO = let ss0 = xs !! 0
                 ss1 = xs !! 1
                 ss2 = xs !! 2
                 ss3 = xs !! 3
    in  [[ss0 !! 0, ss0 !! 1, [ss1 !! 0 !! 2, ss1 !! 1 !! 2, ss1 !! 2 !! 2]], [[ss1 !! 0 !! 0, ss1 !! 0 !! 1, ss3 !! 0 !! 0], [ss1 !! 1 !! 0, ss1 !! 1 !! 1, ss3 !! 0 !! 1], [ss1 !! 2 !! 0, ss1 !! 2 !! 1, ss3 !! 0 !! 2]], [[ss0 !! 2 !! 0, ss2 !! 0 !! 1, ss2 !! 0 !! 2], [ss0 !! 2 !! 1, ss2 !! 1 !! 1, ss2 !! 1 !! 2], [ss0 !! 2 !! 2, ss2 !! 2 !! 1, ss2 !! 2 !! 2]], [[ss2 !! 2 !! 0, ss2 !! 1 !! 0, ss2 !! 0 !! 0], ss3 !! 1, ss3 !! 2]]
spin xs BO = let ss0 = xs !! 0
                 ss1 = xs !! 1
                 ss2 = xs !! 2
                 ss3 = xs !! 3
    in  [[[ss2 !! 0 !! 2, ss2 !! 1 !! 2, ss2 !! 2 !! 2], ss0 !! 1, ss0 !! 2 ], [[ss0 !! 0 !! 2, ss1 !! 0 !! 1, ss1 !! 0 !! 2], [ss0 !! 0 !! 1, ss1 !! 1 !! 1, ss1 !! 1 !! 2], [ss0 !! 0 !! 2, ss1 !! 2 !! 1, ss1 !! 2 !! 2]], [[ss2 !! 0 !! 0, ss2 !! 0 !! 1, ss3 !! 2 !! 2], [ss2 !! 1 !! 0, ss2 !! 1 !! 1, ss3 !! 2 !! 2], [ss2 !! 2 !! 0, ss2 !! 2 !! 1, ss3 !! 2 !! 0]], [ ss3 !! 0, ss3 !! 1, [ss1 !! 0 !! 0, ss1 !! 1 !! 0, ss1 !! 2 !! 0]]]

-- Transform a cube by performing an action or a set of actions
actAll :: [Action] -> Cube -> Cube
actAll xs cube = foldr act cube xs

act :: Action -> Cube -> Cube
act RO cube = actRO cube
act RP cube = actRP cube
act R2 cube = actR2 cube
act LO cube = actLO cube
act LP cube = actLP cube
act L2 cube = actL2 cube
act UO cube = actUO cube
act UP cube = actUP cube
act U2 cube = actU2 cube
act DO cube = actDO cube
act DP cube = actDP cube
act D2 cube = actD2 cube
act FO cube = actFO cube
act FP cube = actFP cube
act F2 cube = actF2 cube
act BO cube = actBO cube
act BP cube = actBP cube
act B2 cube = actB2 cube
act x cube = cube

actRO :: Cube -> Cube
actRO xs = xs

actRP :: Cube -> Cube
actRP c = actRO $ actRO $ actRO c

actR2 :: Cube -> Cube
actR2 c = actRO $ actRO c

actLO :: Cube -> Cube
--actLO xs = [(xs !! 0)] ++ (spin (take 4 (tail xs)) 3) ++ [rotateSP (xs !! 5)]
actLO xs = xs

actLP :: Cube -> Cube
actLP c = actLO $ actLO $ actLO c

actL2 :: Cube -> Cube
actL2 c = actLO $ actLO c

actUO :: Cube -> Cube
actUO xs = [rotateS (xs !! 0)] ++ spin (take 4 (tail xs)) UO ++ [xs !! 5] ++ [xs !! 5]

actUP :: Cube -> Cube
actUP c = actUO $ actUO $ actUO c

actU2 :: Cube -> Cube
actU2 c = actUO $ actUO c

actDO :: Cube -> Cube
actDO xs = [[xs !! 0]] ++ spin (take 4 (tail xs)) DO ++ [rotateSP (xs !! 5)] ++ [rotateSP (xs !! 5)]

actDP :: Cube -> Cube
actDP c = actDO $ actDO $ actDO c

actD2 :: Cube -> Cube
actD2 c = actDO $ actDO c

actFO :: Cube -> Cube
actFO xs = 
    let spinSet = spin ( take 2 xs ++ [xs !! 3] ++ [xs !! 5] ++ [xs !! 3] ++ [xs !! 5]) FO
    in take 2 spinSet ++ [rotateS (xs !! 2)] ++ [ spinSet !! 2 ] ++ [ xs !! 4 ] ++ [ spinSet !! 3 ]

actFP :: Cube -> Cube
actFP c = actFO $ actFO $ actFO c

actF2 :: Cube -> Cube
actF2 c = actFO $ actFO c

actBO :: Cube -> Cube
actBO xs = 
    let spinSet = spin ( take 2 xs ++ [xs !! 3] ++ [xs !! 5] ++ [xs !! 3] ++ [xs !! 5]) BO
    in take 2 spinSet ++ [ xs !! 2 ] ++ [ spinSet !! 2 ] ++ [rotateSP (xs !! 4)] ++ [ spinSet !! 3 ]

actBP :: Cube -> Cube
actBP c = actBO $ actBO $ actBO c

actB2 :: Cube -> Cube
actB2 c = actBO $ actBO c
