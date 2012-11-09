foldingFunction :: [Float] -> String -> [Float]
foldingFunction (x:y:ys) "*" = (x * y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (y - x):ys
foldingFunction (x:y:ys) "/" = (y / x):ys
foldingFunction (x:y:ys) "^" = (y ** x):ys
foldingFunction (x:xs) "ln" = log x:xs
foldingFunction (xs) "sum" = [sum xs]
foldingFunction (xs) numberString = read numberString:xs

foldingFunctionM :: ([Float], Float) -> String -> ([Float], Float)
foldingFunctionM (x:xs, m) "M+" = (x:xs, m + x)
foldingFunctionM (x:xs, m) "M-" = (x:xs, m - x)
foldingFunctionM (xs, _) "MC" = (xs, 0)
foldingFunctionM (xs, m) "MR" = (m:xs, m)
foldingFunctionM (x, m) s = (foldingFunction x s, m)

solveRPN :: String -> Float
solveRPN = head . fst . foldl foldingFunctionM ([], 0.0) . words
