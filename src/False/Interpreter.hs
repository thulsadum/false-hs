{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module False.Interpreter where


import Control.Monad.RWS.Strict
import Control.Monad.Trans.Either

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Bits
import Data.Char ( ord
                 , chr
                 )
import System.IO ( hFlush
                 , stdin
                 , stdout
                 )

import Utils
import False.Parser


type FIEnv = ()
data FIState i = FIState { stack :: [i]
                         , variables :: Map Char i
                         , lambdas :: Map i [FalseToken i]
                         , currentLambda :: i
                         }
type FIWriter = ()


newtype FalseInterpreter i a =
  FalseInterpreter { unwrap
                     :: RWST FIEnv FIWriter (FIState i) (EitherT String IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadState (FIState i), MonadReader FIEnv, MonadWriter FIWriter
           , MonadIO)

type FI i = FalseInterpreter i i



emptyState :: (Integral i) => FIState i
emptyState = FIState { stack = []
                     , variables = Map.empty
                     , lambdas = Map.empty
                     , currentLambda = 0
                     }
defaultEnv :: FIEnv
defaultEnv = ()





interpretingError :: String -> FalseInterpreter i a
interpretingError msg = FalseInterpreter $ lift $ left msg


pop :: FI i
pop = do
  FIState { stack = stack } <- get
  case stack of
    [] -> interpretingError "stack underflow"
    (x:xs) -> do
      modify $ \s -> s { stack = xs}
      return x

push :: i -> FalseInterpreter i ()
push x = do
  modify $ \s@(FIState {stack = xs}) -> s {stack = (x:xs)}


-- | returns the nth element of the stack
peek :: Int -> FI i
peek n = do
  FIState {stack = stack } <- get
  case subscript stack n of
    Nothing -> interpretingError "stack underflow"
    Just x -> return x


top :: FI i
top = peek 0


getVar :: (Num i) => Char -> FI i
getVar var = do
  FIState { variables = vars } <- get
  return $ Map.findWithDefault 0 var vars

putVar :: Char -> i -> FalseInterpreter i ()
putVar var val =  modify $ \(st@FIState {variables = vars}) ->
                             st { variables = Map.alter f var vars }
  where f _ = return val



addLambda :: (Num i, Ord i) => [FalseToken i] -> FalseInterpreter i ()
addLambda ts = do
  st@(FIState { lambdas = ls }) <- get
  let id = fromIntegral . Map.size $ ls
  put $ st { lambdas = Map.insert id ts ls }
  push id

getLambda :: (Ord i) => i -> FalseInterpreter i [FalseToken i]
getLambda i = do
  FIState { lambdas = ls } <- get
  return $ Map.findWithDefault [] i ls


evalBinOp :: (i->i->i) -> FalseInterpreter i ()
evalBinOp rel = do
  op1 <- pop
  op2 <- pop
  push $ op2 `rel` op1



evalOp :: (i->i) -> FalseInterpreter i ()
evalOp f = pop >>= push . f



-- TODO improve error handling
evalStep :: (Integral i, Bits i, Show i) => FalseToken i -> FalseInterpreter i ()

evalStep Add = evalBinOp (+)
evalStep Sub = evalBinOp (-) -- NOTE: is this the correct operand order?
evalStep Mul = evalBinOp (*)
evalStep Div = evalBinOp div
evalStep Neg = evalOp (0-)
  
evalStep Eq = evalBinOp $ \a b -> if a == b then -1 else 0
evalStep Gt = evalBinOp $ \a b -> if a >  b then -1 else 0

evalStep And = evalBinOp (.&.)
evalStep Or  = evalBinOp (.|.)
evalStep Not = evalOp complement

evalStep (IntLit n) = push n
evalStep (CharLit ch) = push . fromIntegral . ord $ ch

evalStep (PutVar v) = pop >>= putVar v
evalStep (GetVar v) = getVar v >>= push

evalStep (Lambda ts) = addLambda ts
evalStep Apply = pop >>= getLambda >>= eval_

evalStep Dup = top >>= push
evalStep Drop = pop >> return ()
evalStep Swap = do
  val1 <- pop
  val2 <- pop
  push val1
  push val2
evalStep Rot = do
  val1 <- pop
  val2 <- pop
  val3 <- pop
  push val2
  push val1
  push val3
evalStep Pick = do
  n <- pop
  val <- peek . fromIntegral $ n
  push val

evalStep When = do
  l <- pop
  cond <- pop
  if cond == 0
    then return ()
    else getLambda l >>= eval_
evalStep While = do
  lbody <- pop
  lcond <- pop
  getLambda lcond >>= eval_
  cond <- pop
  if cond == 0
    then return ()
    else do
    getLambda lbody >>= eval_
    push lcond >> push lbody
    evalStep While

evalStep (Print msg) = liftIO . putStr $ msg
evalStep PrintInt = pop >>= (liftIO . putStr . show)
evalStep Putch = pop >>= (liftIO . putChar . chr . fromIntegral)
evalStep Getch = liftIO getChar >>= (push . fromIntegral . ord)
evalStep Flush = (liftIO $ hFlush stdin) >> (liftIO $ hFlush stdout)




runFalseInterpreter :: FalseInterpreter i a -> FIEnv -> (FIState i) 
                    -> IO (Either String a)
runFalseInterpreter interp env st =
  runEitherT $ extract <$> (runRWST . unwrap) interp env st
  where extract = \(a,_,_) -> a

runFalseInterpreter' :: (Integral i) => FalseInterpreter i a -> IO (Either String a)
runFalseInterpreter' interp = runFalseInterpreter interp defaultEnv emptyState


eval :: (Integral i, Bits i, Show i) => [FalseToken i] -> FalseInterpreter i [i]
eval ts = do
  mapM_ evalStep ts
  FIState { stack = stack } <- get
  return stack


eval_ :: (Integral i, Bits i, Show i) => [FalseToken i] -> FalseInterpreter i ()
eval_ ts = eval ts >> return ()
  


runEval :: (Integral i, Bits i, Show i) => [FalseToken i] -> IO (Either String [i])
runEval = runFalseInterpreter' . eval

runEval_ :: (Integral i, Bits i, Show i) => [FalseToken i] -> IO (Either String ())
runEval_ = runFalseInterpreter' . eval_



                                 
