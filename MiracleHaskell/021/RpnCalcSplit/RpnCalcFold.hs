solveRPN :: String -> Float
solveRPN = head . fst . foldl foldingFunction ([], 0.0) . words
  where foldingFunction (x:y:ys, m) "*" = ((x * y):ys, m)
        foldingFunction (x:y:ys, m) "+" = ((x + y):ys, m)
        foldingFunction (x:y:ys, m) "-" = ((y - x):ys, m)
        foldingFunction (x:y:ys, m) "/" = ((y / x):ys, m)
        foldingFunction (x:y:ys, m) "^" = ((y ** x):ys, m)
        foldingFunction (x:xs, m) "ln" = (log x:xs, m)
        foldingFunction (xs, m) "sum" = ([sum xs], m)
        foldingFunction (x:xs, m) "M+" = (x:xs, m + x)
        foldingFunction (x:xs, m) "M-" = (x:xs, m - x)
        foldingFunction (xs, _) "MC" = (xs, 0)
        foldingFunction (xs, m) "MR" = (m:xs, m)
        foldingFunction (xs, m) numberString = (read numberString:xs, m)
