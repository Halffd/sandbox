import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
type Stack a = [(Tree a, Int)]
type Log = [String]

preOrderWithLogging :: Show a => Tree a -> (Log, [a])
preOrderWithLogging tree = runWriter $ 
    evalStateT (go []) [(tree, 1)]
  where
    go :: Show a => [a] -> StateT (Stack a) (Writer Log) [a]
    go acc = do
      stack <- get
      tell ["\n** Current Stack: " ++ showStack stack]
      tell ["\n** Current Result: " ++ show acc]
      
      case stack of
        [] -> do
          tell ["\n** Stack empty! Traversal complete."]
          return (reverse acc)
        
        (Empty, _):rest -> do
          tell ["\n** Popped Empty node, skipping"]
          put rest
          go acc
        
        (Node val left right, 1):rest -> do
          tell ["\n** Moment 1: Visiting node " ++ show val]
          tell ["\n   - Adding value to result"]
          tell ["\n   - Pushing same node with moment 2"]
          put ((Node val left right, 2):rest)
          
          when (left /= Empty) $ do
            tell ["\n   - Pushing left child with moment 1"]
            modify (\s -> (left, 1):s)
          
          tell ["\n   - Updated stack: " ++ showStack ((Node val left right, 2):rest)]
          go (val:acc)
        
        (Node val _ right, 2):rest -> do
          tell ["\n** Moment 2: Processing node " ++ show val]
          tell ["\n   - Pushing same node with moment 3"]
          put ((Node val Empty right, 3):rest)
          
          when (right /= Empty) $ do
            tell ["\n   - Pushing right child with moment 1"]
            modify (\s -> (right, 1):s)
            
          stack' <- get
          tell ["\n   - Updated stack: " ++ showStack stack']
          go acc
        
        (Node val _ _, 3):rest -> do
          tell ["\n** Moment 3: Finishing node " ++ show val]
          tell ["\n   - Nothing to do in pre-order, just removing from stack"]
          put rest
          go acc

showStack :: Show a => Stack a -> String
showStack stack = intercalate ", " 
                 $ map (\(node, moment) -> 
                         case node of 
                           Empty -> "(Empty," ++ show moment ++ ")"
                           Node v _ _ -> "(" ++ show v ++ "," ++ show moment ++ ")") 
                 $ stack

-- Example usage:
exampleTree :: Tree Int
exampleTree = Node 10 
                (Node 8 
                  (Node 2 Empty Empty) 
                  (Node 12 Empty Empty))
                (Node 7 Empty Empty)

runExample :: IO ()
runExample = do
  let (logs, result) = preOrderWithLogging exampleTree
  putStrLn "Pre-order traversal with logging:"
  mapM_ putStrLn logs
  putStrLn $ "\nFinal result: " ++ show result