module Solutions where

import Control.Applicative (Alternative (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (find)
import Prelude hiding (not)

newtype State s a = MkState {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (MkState fs) =
    MkState $ \s ->
      let (x, s') = fs s
       in (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = MkState $ \s -> (x, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  MkState state1 <*> MkState state2 =
    MkState $ \s ->
      let (f, s') = state1 s
          (x, s'') = state2 s'
       in (f x, s'')

  liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
  liftA2 op (MkState state1) (MkState state2) =
    MkState $ \s ->
      let (x, s') = state1 s
          (y, s'') = state2 s'
       in (x `op` y, s'')

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  MkState state1 >>= f =
    MkState $ \s ->
      let (x, s') = state1 s
          MkState state2 = f x
       in state2 s'

get :: State s s
get = MkState $ \s -> (s, s)

put :: s -> State s ()
-- put s = MkState $ \_ -> ((), s)
put s = MkState $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = MkState $ \s -> ((), f s)

gets :: (s -> a) -> State s a
gets sa = MkState $ \s -> (sa s, s)

evalState :: State s a -> s -> a
evalState state initial = fst $ runState state initial

data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Eq, Ord)

leaf :: a -> BST a
leaf x = Node x Empty Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = leaf x
insert x (Node root l r)
  | x < root = Node root (insert x l) r
  | otherwise = Node root l (insert x r)

kthLargest :: Int -> BST a -> Maybe a
kthLargest k bst = if k <= 0 then Nothing else evalState (go bst) k
 where
  go :: BST a -> State Int (Maybe a)
  go Empty = pure Nothing
  go (Node x l r) = do
    lres <- go l

    case lres of
      Just y -> pure $ Just y
      Nothing -> do
        modify (subtract 1)
        cnt <- get

        if cnt == 0
          then pure $ Just x
          else go r

mkTree :: Ord a => [a] -> BST a
mkTree = go Empty
 where
  go :: Ord a => BST a -> [a] -> BST a
  go acc [] = acc
  go acc (x : xs) = go (insert x acc) xs

bst1 :: BST Int
bst1 = mkTree [5, 3, 7, 1, 3, 9, 12]

-- >>> bst1
-- Node 5 (Node 3 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 7 Empty (Node 9 Empty (Node 12 Empty Empty)))

-- >>> map (`kthLargest` bst1) [0..10]
-- [Nothing,Just 1,Just 3,Just 3,Just 5,Just 7,Just 9,Just 12,Nothing,Nothing,Nothing]

newtype ParseError = ParseError {getParseError :: String}
  deriving Show

newtype Parser a = MkParser {runParser :: String -> Either ParseError (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (MkParser p) =
    MkParser $ \s ->
      case p s of
        Left err -> Left err
        Right (x, s') -> Right (f x, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = MkParser $ \s -> Right (x, s)

  MkParser p1 <*> MkParser p2 =
    MkParser $ \s ->
      case p1 s of
        Left err1 -> Left err1
        Right (f, s') -> case p2 s' of
          Left err2 -> Left err2
          Right (x, s'') -> Right (f x, s'')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  MkParser p >>= f =
    MkParser $ \s ->
      case p s of
        Left err -> Left err
        Right (x, s') -> case f x of
          MkParser p2 -> p2 s'

instance Alternative Parser where
  empty :: Parser a
  empty = MkParser . const . Left . ParseError $ "unknown error"

  (<|>) :: Parser a -> Parser a -> Parser a
  MkParser p1 <|> MkParser p2 =
    MkParser $ \s -> case (p1 s, p2 s) of
      (Right lhs, _) -> Right lhs
      (_, rhs) -> rhs

item :: Parser Char
item = MkParser $ \s -> case s of
  [] -> Left $ ParseError "empty stream"
  h : t -> pure (h, t)

sat :: (Char -> Bool) -> Parser Char
sat p = MkParser $ \s -> case s of
  h : t
    | p h -> pure (h, t)
    | otherwise -> Left $ ParseError $ "unexpected character '" ++ [h] ++ "'"
  [] -> Left $ ParseError "empty stream"

eof :: Parser ()
eof = MkParser $ \s -> case s of
  [] -> pure ((), [])
  _ -> Left $ ParseError "input not exhausted"

data Term
  = Variable String
  | Not Term
  | Term :&: Term
  | Term :|: Term
  | Term :=>: Term
  deriving (Show, Eq)

-- няма да се интересуваме от приоритета/асоциативността на операторите

char :: Char -> Parser Char
char c = sat (== c)

-- >>> runParser (char 'a') "abcd"
-- Right ('a',"bcd")

lower :: Parser Char
lower = sat isAsciiLower

-- >>> runParser lower "abcd"
-- Right ('a',"bcd")

-- >>> runParser lower "Abcd"
-- Left (ParseError {getParseError = "unexpected character 'A'"})

upper :: Parser Char
upper = sat isAsciiUpper

digit :: Parser Char
digit = sat isDigit

underscore :: Parser Char
underscore = char '_'

-- разпознава непразни низове, които започват с главна буква,
-- последвана от 0 или повече срещания на малка или голяма буква,
-- цифра или долна черта
variable :: Parser Term
variable = Variable <$> liftA2 (:) upper (many (lower <|> upper <|> digit <|> underscore))

not' :: Parser Term
not' = char '!' >> Not <$> term

-- >>> runParser not' "!Foo"
-- Right (Not (Variable "Foo"),"")

-- >>> runParser not' "!Foo & Bar"
-- Right (Not (Variable "Foo" :&: Variable "Bar"),"")

-- по горния начин правим оператора ! с най-нисък приоритет.
-- няма да искаме нещо такова в общия случай, така че просто
-- бихме могли да прочетем някакъв "атомарен" обект
not :: Parser Term
not = char '!' >> Not <$> token (variable <|> not <|> paren)

-- >>> runParser not "!Foo & Bar"
-- Right (Not (Variable "Foo"),"& Bar")

-- горното е очакван ефект, не сме извиквали парсер,
-- който да прочете останалата част от низа

paren :: Parser Term
paren = char '(' *> term <* char ')'

-- >>> runParser paren "(Some_variable_here)"
-- Right (Variable "Some_variable_here","")

-- тъй като граматиката е ляво рекурсивна,
-- бихме забили по парсването на левия операнд
-- (ако имахме lhs <- term). затова първо ще
-- се опитаме да парснем друг израз,
-- правилото в граматиката за който не води
-- до лява рекурсия (това е именно base в
-- парсера term), и след това ще се опитаме
-- да приложим по-комплексен парсер

-- следните 3 парсерса се различават единствено
-- по оператора, който разпознават, и са различни
-- начини за написването на един и същи парсер
conj :: Term -> Parser Term
conj lhs = do
  _ <- char '&'
  rhs <- term
  pure $ lhs :&: rhs

disj :: Term -> Parser Term
disj lhs = char '|' >> (lhs :|:) <$> term

implies :: Term -> Parser Term
implies lhs = fmap (lhs :=>:) $ char '=' >> char '>' >> term

-- по-обобщена версия на комбинатора, който иначе би ни трябвал,
-- за да осъществим по-горната идея. приемаме списък от функции,
-- всяка от които приема някакъв аргумент (това ще бъде левия операнд)
-- и връща парсер за останалата част от входния низ (такива елементи
-- на списъка могат да бъдат горните 3 комбинатора), и стойност по
-- подразбиране (отново левия операнд), понеже може нито един от
-- парсерите в списъка да не успее - тогава връщаме тази стойност
lr :: [a -> Parser a] -> a -> Parser a
lr pfs x = foldr (<|>) (pure x) ps
 where
  ps = map ($ x) pfs

-- прочита всички интервали, като не ги добавя в
-- резултата от парсера. в общия случай бихме искали
-- да не добавяме и нови редове, табулации и т.н.
token :: Parser a -> Parser a
token p = many (char ' ') *> p <* many (char ' ')

term :: Parser Term
term = do
  base <- token $ variable <|> not <|> paren
  lr [conj, disj, implies] base

-- >>> runParser term "Abcd"
-- Right (Variable "Abcd","")

-- >>> runParser term "A | B"
-- Right (Variable "A" :|: Variable "B","")

-- >>> runParser term "A & (B | C)"
-- Right (Variable "A" :&: (Variable "B" :|: Variable "C"),"")

-- >>> runParser term "(A & B) | C"
-- Right ((Variable "A" :&: Variable "B") :|: Variable "C","")

-- >>> runParser term "Xyz_123 => Abc_789"
-- Right (Variable "Xyz_123" :=>: Variable "Abc_789","")

-- >>> runParser term "   xyz  "
-- Left (ParseError {getParseError = "unexpected character 'x'"})

-- >>> runParser term "& T => U"
-- Left (ParseError {getParseError = "unexpected character '&'"})

-- какво правим с останалата част от входния поток?
-- жокер: дефинирайте парсер комбинатор, който проверява дали сме
-- прочели целия входен поток
-- >>> runParser term "A => B 123"
-- Right (Variable "A" :=>: Variable "B","123")

-- примера от по-горе
-- >>> runParser term "!Foo & Bar"
-- Right (Not (Variable "Foo") :&: Variable "Bar","")

type Context = [(String, Term)]

-- този тип на функция не е най-добрият вариант,
-- понеже бихме искали да фиксираме коя е "схемата",
-- на която искаме да напаснем даден израз, но
-- това не става ясно от самия тип, понеже и двата
-- параметъра са от тип Term и нямаме ясен начин да
-- разграничим схемата от конкретната формула.
-- за целта имаме няколко варианта:
-- 1. създаваме изцяло нов тип за схемата,
--    който е напълно идентичен на Term,
--    само че с друго име - така няма как да
--    подадем конкретна формула на някаква схема;
-- 2. можем да направим Term фантомов тип (phantom type) -
--    иначе казано, ще добавим типов параметър (data Term a = ...),
--    който няма да използваме никъде в конструкторите и ще
--    съществува само на ниво тип. тогава можем да дефинираме
--    други два типа (data Meta, data Concrete - без конструктори),
--    и да имаме тип match :: Term Meta -> Term Concrete -> Bool
match :: Term -> Term -> Bool
match lhs' rhs' = evalState (go lhs' rhs') []
 where
  go :: Term -> Term -> State Context Bool
  go (Variable x) rhs = do
    ctx <- get
    case find ((== x) . fst) ctx of
      Just (_, y) -> pure $ rhs == y
      Nothing -> modify ((x, rhs) :) >> pure True
  go (Not lhs) (Not rhs) = go lhs rhs
  go (lhs1 :&: rhs1) (lhs2 :&: rhs2) = do
    res1 <- go lhs1 lhs2
    res2 <- go rhs1 rhs2
    pure $ res1 && res2
  go (lhs1 :|: rhs1) (lhs2 :|: rhs2) =
    liftA2 (&&) (go lhs1 lhs2) (go rhs1 rhs2)
  go (lhs1 :=>: rhs1) (lhs2 :=>: rhs2) =
    liftA2 (&&) (go lhs1 lhs2) (go rhs1 rhs2)
  go _ _ = pure False

parse :: String -> Either ParseError Term
parse = (fst <$>) . runParser (term <* eof)

-- >>> parse "A => B 123"
-- Left (ParseError {getParseError = "input not exhausted"})

-- >>> liftA2 match (parse "A") (parse "X & Y")
-- Right True

-- >>> liftA2 match (parse "A & B") (parse "X & Y")
-- Right True

-- >>> liftA2 match (parse "A & B") (parse "X & X")
-- Right True

-- >>> liftA2 match (parse "A & A") (parse "X & X")
-- Right True

-- >>> liftA2 match (parse "A & A") (parse "X & Y")
-- Right False
