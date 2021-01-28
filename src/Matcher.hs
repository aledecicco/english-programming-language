module Matcher where

import Control.Monad ( when, void, filterM )
import Control.Monad.Trans.State ( get, gets, put, modify, liftCatch, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, catchE, runExcept, Except )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find )
import Data.Maybe ( fromJust )
import Data.Char ( toLower )

import Types
import PreludeDefs

--


-- Environment

type Error = String

type FunEnv = [Function]
type VarEnv = [(Name, Type)]
type StructEnv = [(Name, Name, [(Name, Type)])]
type Env = (FunEnv, VarEnv, StructEnv)

type MatcherState a = StateT Env (Except Error) a

initialEnv :: Env
initialEnv = (operators ++ procedures, [], [])

getVarEnv :: MatcherState VarEnv
getVarEnv = gets (\(_, vE, _) -> vE)

getFunEnv :: MatcherState FunEnv
getFunEnv = gets (\(fE, _, _) -> fE)

getStructEnv :: MatcherState StructEnv
getStructEnv = gets (\(_, _, sE) -> sE)

setVarEnv :: VarEnv -> MatcherState ()
setVarEnv vE = modify (\(fE, _, sE) -> (fE, vE, sE))

getVarType :: Name -> MatcherState (Maybe Type)
getVarType vn = do
    vE <- getVarEnv
    let r = find (\vd -> fst vd == vn) vE
    return $ snd <$> r

setVarType :: Name -> Type -> MatcherState ()
setVarType vn t' = do
    r <- getVarType vn
    case r of
        Just t -> when (t /= t') $ missmatchingTypeAssign vn t t'
        Nothing -> modify (\(fE, vE, sE) -> (fE, (vn, t'):vE, sE))

removeVarType :: Name -> MatcherState ()
removeVarType vn = do
    vE <- getVarEnv
    let vE' = filter ((vn /=) . fst) vE
    setVarEnv vE'


getOperatorType :: Title -> [Value] -> MatcherState (Maybe Type)
getOperatorType t vs = do
    fE <- getFunEnv
    case find funFinder fE of
        Just (Operator _ tF) -> do
            vTs <- mapM getValueType vs
            return $ Just (tF vTs)
        Nothing -> return Nothing
    where
        funFinder (Operator t' _) = t == t'
        funFinder (Procedure _) = False

getStructFields :: Name -> MatcherState (Maybe [(Name, Type)])
getStructFields n = do
    sE <- getStructEnv
    let r = find (\(n', _, fs) -> n == n') sE
    case r of
        Just (_, _, fs) -> return $ Just fs
        Nothing -> return Nothing

getStructFieldType :: Name -> Name -> MatcherState (Maybe Type)
getStructFieldType sn fn = do
    fs <- getStructFields sn
    case fs of
        Just fs -> return $ snd <$> find (\(fn', t) -> fn == fn') fs
        Nothing -> return Nothing

varIsDefined :: Name -> MatcherState Bool
varIsDefined vn = do
    r <- getVarType vn
    case r of
        Just _ -> return True
        Nothing -> return False

-- Tries the first action and, if it fails, reverts the state and tries the second action
(<|>) :: MatcherState a -> MatcherState a -> MatcherState a
ma <|> mb = liftCatch catchE ma (const mb)

-- Tries each action in a list until one of them succeeds
try :: [MatcherState a] -> String -> MatcherState a
try actions msg = foldr (<|>) (customError msg) actions

-- Empties the variables environment
resetVariables :: MatcherState ()
resetVariables = setVarEnv []

--


-- Errors

customError :: String -> MatcherState a
customError = lift . throwE

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
missmatchingTypeAssign :: Name -> Type -> Type -> MatcherState a
missmatchingTypeAssign vn t t' = customError $ "Can't assign value of type " ++ show t' ++ " to variable " ++ concat vn ++ " with type " ++ show t

-- Error that occurs when two parameters with the same name are used in a title
alreadyDefinedParameter :: Name -> MatcherState a
alreadyDefinedParameter vn = customError $ "Parameter name " ++ concat vn ++ " can't be used more than once"

-- Error that occurs when a loop uses an iterator with the same name as an already defined variable
alreadyDefinedIterator :: Name -> MatcherState a
alreadyDefinedIterator vn = customError $ "Variable " ++ concat vn ++ " is already in use"

-- Error that occurs when a value of the wrong type is used in a sentence
wrongTypeValue :: Value -> Type -> MatcherState a
wrongTypeValue v t = customError $ "Value " ++ show v ++ " should be of type " ++ show t

-- Error that occurs when a matchable can't be matched
unmatchable :: [MatchablePart] -> MatcherState a
unmatchable ps = customError $ "\"" ++ show ps ++ "\" couldn't be matched"

--


-- Auxiliary

-- Adds the parameters in a title to the variables environment
registerTitleParameters :: Title -> MatcherState ()
registerTitleParameters [] = return ()
registerTitleParameters (TitleWords _:ts) = registerTitleParameters ts
registerTitleParameters (TitleParam n t:ts) = do
    r <- varIsDefined n
    if r then alreadyDefinedParameter n else setVarType n t
    registerTitleParameters ts

-- Validates a sentence and persists its changes to the state, assuming that its arguments are correctly typed
commitSentence :: Sentence -> MatcherState ()
commitSentence (VarDef ns v) = do
    t <- getValueType v
    mapM_ (`setVarType` t) ns
commitSentence (If v ss) = commitSentences ss
commitSentence (IfElse v ssTrue ssFalse) = commitSentences $ ssTrue ++ ssFalse
commitSentence (For n vFrom vTo ss) = do
    isDef <- varIsDefined n
    when isDef $ alreadyDefinedIterator n
    setVarType n IntT
    commitSentences ss
    removeVarType n
commitSentence (ForEach n v ss) = do
    isDef <- varIsDefined n
    when isDef $ alreadyDefinedIterator n
    ~(ListT eT) <- getValueType v
    setVarType n eT
    commitSentences ss
    removeVarType n
commitSentence (Until v ss) = commitSentences ss
commitSentence (While v ss) = commitSentences ss
commitSentence (Result t v) = return ()
commitSentence (ProcedureCall t vs) = return ()

-- Commits the sentences in a list in order
commitSentences :: [Sentence] -> MatcherState ()
commitSentences = mapM_ commitSentence

getValueType :: Value -> MatcherState Type
getValueType (IntV _) = return IntT
getValueType (BoolV _) = return BoolT
getValueType (StringV _) = return StringT
getValueType (StructV n _) = return $ StructT n
getValueType (ListV t _) = return $ ListT t
getValueType (VarV n) = fromJust <$> getVarType n
getValueType (PossessiveV (StructV n _) f) = fromJust <$> getStructFieldType n f
getValueType (OperatorCall t vs) = fromJust <$> getOperatorType t vs

--


-- Matchers

matchAsName :: [MatchablePart] -> MatcherState (Maybe Name)
matchAsName [WordP s] = return $ Just [s]
matchAsName (WordP s : ps) = do
    r <- matchAsName ps
    case r of
        Just ns -> return $ Just (s:ns)
        Nothing -> return Nothing
matchAsName _ = return Nothing

matchAsInt :: [MatchablePart] -> MatcherState [Value]
matchAsInt [IntP n] = return [IntV n]
matchAsInt _ = return []

matchAsBool :: [MatchablePart] -> MatcherState [Value]
matchAsBool [WordP s]
    | s == "true" = return [BoolV True]
    | s == "false" = return [BoolV False]
matchAsBool _ = return []

matchAsString :: [MatchablePart] -> MatcherState [Value]
matchAsString [LiteralP n] = return [StringV n]
matchAsString _ = return []

matchAsVar :: [MatchablePart] -> MatcherState [Value]
matchAsVar ps = do
    r <- matchAsName ps
    case r of
        Just (w:ws) -> do
            let pos = if w == "the" then [w:ws, ws] else [w:ws]
            pos' <- filterM varIsDefined pos
            return $ map VarV pos'
        Nothing -> return []

matchAsPossessive :: [MatchablePart] -> MatcherState [Value]
matchAsPossessive _ = return []

matchAsOperatorCall :: [MatchablePart] -> MatcherState [Value]
matchAsOperatorCall _ = return []

matchAsProcedureCall :: [MatchablePart] -> MatcherState [Sentence]
matchAsProcedureCall ps = return []


-- Matches a value matchable as any type of value
matchValue :: Value -> MatcherState [Value]
matchValue (ValueM ps) = do
    asInt <- matchAsInt ps
    asBool <- matchAsBool ps
    asString <- matchAsString ps
    asVar <- matchAsVar ps
    asPossessive <- matchAsPossessive ps
    asOperatorCall <- matchAsOperatorCall ps
    return $ asInt ++ asBool ++ asString ++ asVar ++ asPossessive ++ asOperatorCall
matchValue (StructV n fs) = return [] -- ToDo: match field values, checking their integrity
matchValue (ListV t es) = return [] -- ToDo: match list elements, checking that they have the same type
matchValue v = return [v]

matchSentence :: Sentence -> MatcherState [Sentence]
matchSentence (SentenceM ps) = matchAsProcedureCall ps
matchSentence (If (ValueM ps) ss) = do
    vs <- matchAsBool ps
    ssCombinations <- sequence <$> mapM matchSentence ss
    return $ concatMap (\v -> map (If v) ssCombinations) vs
matchSentence s = return [s]

-- Matches a list of sentences, trying all possible combinations
matchSentences :: [Sentence] -> MatcherState [Sentence]
matchSentences [] = return []
matchSentences (s:rest) = do
    ss <- matchSentence s
    try (map trySentence ss) "Couldn't match sentence"
    where
        trySentence s' = do
            commitSentence s'
            rest' <- matchSentences rest
            return $ s':rest'

-- Matches the matchables in each sentence of a block
matchBlock :: Block -> MatcherState Block
matchBlock b = case b of
    StructDef {} -> return b
    FunDef t ss -> do
        registerTitleParameters t
        ss' <- matchSentences ss
        return $ FunDef t ss'

-- Matches the matchables in each block of a program
matchBlocks :: Program -> MatcherState Program
matchBlocks [] = return []
matchBlocks (b:bs) = do
    b' <- matchBlock b
    resetVariables
    bs' <- matchBlocks bs
    return $ b':bs'

--


-- Main

-- Returns the given program with its matchables matched
matchProgram :: Program -> Either Error (Program, Env)
matchProgram p = runExcept $ runStateT matchProgram' initialEnv
    where
        matchProgram' = do
            --p1 <- registerStructs p
            --p2 <- matchTitles p1
            --p3 <- registerFunctions p2
            matchBlocks p

--
