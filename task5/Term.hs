module Term where

data Operation = Plus | Minus | Mult deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }
            | NegatedTerm{ srcTerm :: Term } -- Позволяет описать унарный минус
            | BinaryTerm{ lhv :: Term, op :: Operation, rhv :: Term } deriving(Show,Eq)

infixl 6 <+>
(IntConstant x) <+> (IntConstant y) = IntConstant (x + y)
x <+> y = BinaryTerm x Plus y

infixl 6 <->
(IntConstant x) <-> (IntConstant y) = IntConstant (x - y)
x <-> y = BinaryTerm x Minus y

infixl 7 <*>
(IntConstant x) <*> (IntConstant y) = IntConstant (x * y)
x <*> y = BinaryTerm x Mult y

-- Унарный минус
um (IntConstant intValue) = IntConstant (-intValue)
um x = NegatedTerm x 

replaceVar :: Term {- что заменить -} -> String {- где -} -> Term {- на что -} -> Term
replaceVar src @ (IntConstant intValue) _ _ = src
replaceVar src @ (Variable varName) name term = if name == varName then term else src
replaceVar (NegatedTerm srcTerm) name term = 
    NegatedTerm (replaceVar srcTerm name term)
replaceVar (BinaryTerm lhv op rhv) name term = 
    BinaryTerm (replaceVar lhv name term) op (replaceVar rhv name term)