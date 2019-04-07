module Hw1 where

type Mapping = [(String, String, String)]
data AST = EmptyAST | ASTNode String AST AST deriving (Show, Read)

writeExpression :: (AST, Mapping) -> String
evaluateAST :: (AST, Mapping) -> (AST, String)
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT writeExpression and evaluateAST FUNCTION ACCORDING TO GIVEN SIGNATURES -- 

writeExpression ((ASTNode x left right), y) | y == [] = writeASTExpression(ASTNode x left right)
                                            | otherwise = "let " ++ mapVariables(y) ++ " in " ++ writeASTExpression(ASTNode x left right)

writeASTExpression (ASTNode x left right) | x == "plus" = "(" ++ writeASTExpression(left) ++ "+" ++ writeASTExpression(right) ++ ")"
                                          | x == "times" = "(" ++ writeASTExpression(left) ++ "*" ++ writeASTExpression(right) ++ ")"
                                          | x == "negate" = "(-" ++ writeASTExpression(left) ++")"
                                          | x == "cat" = "(" ++ writeASTExpression(left) ++ "++" ++ writeASTExpression(right) ++ ")"
                                          | x == "len" = "(length " ++ writeASTExpression(left) ++ ")"
                                          | x == "num" = writeExpressionNum(left)
                                          | x == "str" = writeExpressionStr(left)
                                          | otherwise = x
                                            

mapVariables y | length(y) == 1 = mapVariable(y!!0)
               | otherwise = mapVariable(y!!0) ++ ";" ++ mapVariables(tail(y))
               
mapVariable (a, b, c) | b == "str" = a ++ "=\"" ++ c ++ "\""
                       | b == "num" = a ++ "=" ++ c

writeExpressionNum ((ASTNode x left right)) = x
writeExpressionStr ((ASTNode x left right)) = "\"" ++ x ++ "\""

evaluateAST ((ASTNode x left right), y) = (mapAST((ASTNode x left right), y), evaluate(mapAST((ASTNode x left right), y)))
                                        
                                        
mapAST ((ASTNode x left right),y) | x == "plus" = (ASTNode x (mapAST(left, y)) (mapAST(right, y)))
                                  | x == "times" = (ASTNode x (mapAST(left, y)) (mapAST(right, y)))
                                  | x == "negate" = (ASTNode x (mapAST(left, y)) EmptyAST)
                                  | x == "cat" = (ASTNode x (mapAST(left, y)) (mapAST(right, y)))
                                  | x == "len" = (ASTNode x (mapAST(left, y)) EmptyAST)
                                  | x == "num" = (ASTNode x (mapASTLeaf(left)) EmptyAST)
                                  | x == "str" = (ASTNode x (mapASTLeaf(left)) EmptyAST)
                                  | otherwise = switchVariables((ASTNode x left right),y)
                                  
mapASTLeaf (ASTNode x left right) = (ASTNode x left right)

getthird (_,_,a) = a
getsecond (_,a,_) = a
getfirst (a,_,_) = a

switchVariables((ASTNode x left right), y) | x == getfirst(head y) = (ASTNode (getsecond(head y)) (ASTNode (getthird(head y)) EmptyAST EmptyAST) EmptyAST)
                                           | otherwise = switchVariables((ASTNode x left right), (tail(y)))

evaluate (ASTNode x left right) | x == "plus" =  show( read(evaluate(left)) + read(evaluate(right)))
                                | x == "times" = show( read(evaluate(left)) * read(evaluate(right)))
                                | x == "negate" = show(- (read(evaluate(left))))
                                | x == "cat" = evaluate(left) ++ evaluate(right)
                                | x == "len" = show( length(evaluate(left)))
                                | x == "num" = evaluateASTLeaf(left)
                                | x == "str" = evaluateASTLeaf(left)
                                | otherwise = x
                                             
                                        
evaluateASTLeaf (ASTNode x left right) = x

