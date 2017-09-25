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


-- NOTE: The following code does what's needed of PA1, However 
-- it is terribly written, primarily because I am still trying to
-- get over the "imperitive thinking".
--
-- TODO:
-- * reduce should not return IO Lexp. Change it back to the way
--   PA1Helper expects (Lexp -> Lexp)
-- * get rid of all the nested parens, use $ and . as appropriate
--
-- * Fixes to alpha_rename_all:
-- NOTE: This function is not used anymore so it is okay even if we 
-- don't fix it.
--      * Simplify alpha_rename_all to use functional thinking, currently
--        it is essentially an impertive routine (specially for the case 
--        of an Apply expression).
--      * Currently idx variable to alpha_rename assumes no more than 10
--        rename operations in a sub expression of an Apply. This should 
--        be fixed. One way to do it would be to have the alpha_rename 
--        function return the value of next idx. This should fix the issue
--        but it is still imperitive. Need to figure out a functional way

-- remove and free vars are from lecture sample, and are straight forward.
--remove
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (\v -> v/=x)

-- freevars
freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)

-- reduce
-- We have the following cases for reduction
--    * v or Atom: can't reduce it further so we just return it
--    * (\x.e m) or Apply with Lambda: We can apply beta reduction here
--    * (e1 e2) or Apply without Lambda: We recursively reduce e1 and e2 in Apply
--    * \x.(e m) or Lambda with Apply: We can apply an eta conversion
--    * \x.e or Lambda without Apply: We recursively reduce e in Lambda
reduce :: Lexp -> Lexp
reduce lexp@(Atom _) = lexp -- can't reduce if it's an atom
reduce lexp@(Apply (Lambda x e) e2) = reduce_helper lexp (beta_reduce x e2 e)
reduce lexp@(Apply e1 e2) = reduce_helper lexp (Apply (reduce e1) (reduce e2))
reduce lexp@(Lambda x (Apply e m)) = reduce_helper lexp (eta_convert lexp x e m)
reduce lexp@(Lambda x e) = reduce_helper lexp (Lambda x (reduce e))

-- reduce_helper
-- Arguments:   expr1: expression before reduction
--              expr2 expression after reduction
-- sometimes calling reduce on an expression results in another reducible expression,
-- this helper takes care of this scenario, it keeps calling reduce on an expression
-- until no further reductions are possible
reduce_helper :: Lexp->Lexp->Lexp
reduce_helper expr1 expr2
    | expr1 == expr2 = expr2
    | otherwise = reduce expr2  -- here it seems as if there's no further recursion, 
                                -- but recall that 'reduce' calls 'reduce_helper' 
                                -- internally. Therefore this code is actually correct

-- beta_reduce
-- replaces all instances of 'var' with 'val' in 'e'
-- Arguments:   var: variable name
--              val: value of the var
--              third argument: expression in which we want to replace var with val
-- We have the following cases for e:
--  * v or Atom: in this case if e matches var, we replace e with val
--  * (expr1 expr2) or Apply: we recursively beta_reduce instances of var in e1 and e2 with val
--  * \var'.expr' or Lambda with var' /= var: We replace instances of var in expr' with val 
--    and reconstruct the lambda with changed changed expr'
--  * \var'.expr' or Lambda with var' == var:: We don't make any changes since in this case all 
--    instance of var in expr' are bound to var'
beta_reduce :: String -> Lexp -> Lexp -> Lexp
beta_reduce var val e@(Atom v)
            | var == v = val
            | otherwise = e
beta_reduce var val (Apply expr1 expr2) = Apply (beta_reduce var val expr1) (beta_reduce var val expr2)
beta_reduce var val expr@(Lambda var' expr')
            | var' /= var && var' `notElem` (freevars val) = Lambda var' (beta_reduce var val expr')
            | var' /= var && var' `elem` (freevars val) = beta_reduce var val (alpha_rename expr (freevars val))
            | otherwise = expr 

-- eta_convert
-- Arguments:   lexp: The full expression i.e. \x.(E M)
--              x: x in \x.(E M)
--              e: E in \x.(E M)
--              m: M in \x.(E M)
-- For eta conversion we have following cases for M, E and x:
--  * x and M don't match, no changes can't be made
--  * x is not free in E, we replace the whole expression with just E
eta_convert :: Lexp->String->Lexp->Lexp->Lexp
eta_convert lexp x e m@(Atom v)
            | x /= v = lexp -- no change if variables don't match
            | x `notElem` (freevars e) = e
            | otherwise = lexp
eta_convert lexp x e m = lexp

-- alpha_rename
-- Given a lambda expression \x.e, changes x to something that does not appear in bad_names
-- Arguments:   first argument: the lambda expression
--              bad_names: list of names that we want to avoid (these are the free variables)
alpha_rename:: Lexp->[String]->Lexp
alpha_rename (Lambda x e) bad_names = (Lambda (new_name x bad_names) (beta_reduce x (Atom (new_name x bad_names)) e)) 
alpha_rename lexp bad_names = lexp

-- new_name
-- returns new name for a lambda variable
-- Arguments:   x: original name (the one that already apperas in 'bad_names')
--              bad_names: the names that we want to avoid (freevars)
new_name :: String->[String]->String
new_name x bad_names = head (dropWhile (`elem` bad_names) [(x ++ (show i)) | i <- [1..]])


-- Entry point of program
main = do
    args <- getArgs
    let filename = case args of { [x] -> x; _ -> "input.lambda" }
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram filename reduce

















-- UNUSED FUNCTIONS: You can ignore anything after this line
-- These functions are not used anymore, these are here because they served
-- us well in the past. "Honour the dead"

-- We alpha rename everything in the very start, this is wasteful and should be changed.
reduce_setup :: Lexp -> IO Lexp
reduce_setup lexp = do
    let lexp' = (alpha_rename_all lexp 0)
    let lexp'' = reduce lexp'
    putStrLn ((show lexp) ++ " => " ++ (show lexp') ++ " => " ++ (show lexp''))
    return lexp''

-- alpha_rename_all
-- Performs alpha renaming recursively, renaming all lambdas weather needed or not
-- Arguments:   lexp: the expression to alpha_rename
--              idx: index to appent to the variables of lambdas
-- Following cases:
--  * v or Atom: no alpha renaming possible
--  * (e1 e2) or Apply: recursively alpha_rename e1 and e2
--  * \x.e or Lambda: recursively alpha_rename e and then change instances of x in e with (x ++ (show idex))
-- NOTE: This is not used anywhere right now, initially I used to rename
-- all lambdas whether they needed renaming or not. But I am leavin git here
-- because "respect the dead".
alpha_rename_all :: Lexp->Integer->Lexp
alpha_rename_all lexp@(Atom _) idx = lexp
alpha_rename_all (Apply e1 e2) idx = (Apply (alpha_rename_all e1 idx) (alpha_rename_all e2 (idx+10) )) -- this is kind of  a hack, what if alpha_renaming expression 1 uses up more than 10 of indices? but for this assignment, it should work fine
alpha_rename_all (Lambda x e) idx = (Lambda 
    (x ++ (show idx)) 
    (beta_reduce 
        x 
        (Atom (x++(show idx))) 
        (alpha_rename_all e (idx+1)) 
        ) 
    )

-- main = do
--     args <- getArgs
--     let expr =  (case args of
--                     [] -> "(\\x.x y)"
--                     [s]-> s)
    
    
--     runProgram expr reduce_setup
