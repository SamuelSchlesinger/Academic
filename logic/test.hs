data T = T { x :: Int, y :: Int } deriving (Show)

main = do
    let p = T { x = 10, y = 20 }
    let q = p { y = 50 }
    (putStrLn . show) q
