import PA1Helper
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.


--remove
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (\v -> v/=x)

freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)

iscombinator :: Lexp -> Bool
iscombinator e = freevars e == []

-- we alpha-rename everything in the very start
reduce_setup :: Lexp -> IO Lexp
reduce_setup lexp = do
    let lexp' = (alpha_rename lexp 0)
    lexp'' <- reduce lexp'
    putStrLn ((show lexp) ++ " => " ++ (show lexp') ++ " => " ++ (show lexp''))
    return lexp''

-- Cases
-- beta reduction (application with first member as lambda)
-- eta conversion (lambda with expression as application)
reduce :: Lexp -> IO Lexp
reduce lexp@(Atom _) = do 
    putStrLn("a1 (atom) " ++ (show lexp)); 
    return lexp -- can't reduce if it's an atom
reduce lexp@(Apply (Lambda x e) e2) = do 
    exp' <- reduce_helper lexp (beta_reduce x e2 e)
    putStrLn("a2 (beta reducible call) " ++ show (lexp) ++ " => " ++ (show exp'))
    return exp'
reduce lexp@(Apply e1 e2) = do 
    a <- reduce e1
    b <- reduce e2
    putStrLn("a3 (simple call) " ++ (show lexp) ++ " => (" ++ (show a) ++ " " ++ (show b) ++ ")")
    reduce_helper lexp (Apply a b)
reduce lexp@(Lambda x (Apply e m)) = do
    exp' <- reduce_helper lexp (eta_convert lexp x e m)
    putStrLn("a4 (eta expression) " ++ (show lexp) ++ " => " ++ (show exp'))
    return exp'
reduce lexp@(Lambda x e) = do 
    e' <- reduce e
    e'' <- reduce_helper lexp (Lambda x e')
    putStrLn("a5 (simple lambda) " ++ (show lexp) ++ " => " ++ (show e') ++ " => " ++ (show e''))
    return e''


reduce_helper :: Lexp->Lexp->IO Lexp
reduce_helper expr1 expr2 = if expr1 == expr2 then return expr2 else do
        exp' <- reduce expr2
        return exp'

beta_reduce :: String -> Lexp -> Lexp -> Lexp
beta_reduce var val e@(Atom v)
            | var == v = val
            | otherwise = e
beta_reduce var val (Apply expr1 expr2) = Apply (beta_reduce var val expr1) (beta_reduce var val expr2)
beta_reduce var val expr@(Lambda var' expr')
            | var' /= var = Lambda var' (beta_reduce var val expr')
            | otherwise = expr 

eta_convert :: Lexp->String->Lexp->Lexp->Lexp
eta_convert lexp x e m@(Atom v)
            | x /= v = lexp -- no change if variables don't match
            | x `notElem` (freevars e) = e
            | otherwise = lexp
eta_convert lexp x e m = lexp


alpha_rename :: Lexp->Integer->Lexp
alpha_rename lexp@(Atom _) idx = lexp
alpha_rename (Apply e1 e2) idx = (Apply (alpha_rename e1 idx) (alpha_rename e2 (idx+10) )) -- this is kind of  a hack, what if alpha_renaming expression 1 uses up more than 10 of indices? but for this assignment, it should work fine
alpha_rename (Lambda x e) idx = (Lambda 
    (x ++ (show idx)) 
    (beta_reduce 
        x 
        (Atom (x++(show idx))) 
        (alpha_rename e (idx+1)) 
        ) 
    )

alpha_rename_simple :: Lexp->String->Lexp
alpha_rename_simple (Lambda x e) y = (Lambda y (beta_reduce x (Atom y) e)) 
alpha_rename_simple lexp y = lexp


-- Entry point of program
main = do
    args <- getArgs
    let filename = case args of { [x] -> x; _ -> "input.lambda" }
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram filename reduce_setup

-- main = do
--     args <- getArgs
--     let expr =  (case args of
--                     [] -> "(\\x.x y)"
--                     [s]-> s)
    
    
--     runProgram expr reduce_setup
