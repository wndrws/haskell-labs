import Data.Char (toUpper) -- Для примера использования

data FunMonad a = FunMonad{ fun :: () -> a }

instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad (f . a)

instance Applicative FunMonad where
    pure a = FunMonad (\() -> a)
    mfa <*> ma = fmap (fun mfa ()) ma

instance Monad FunMonad where
    ma >>= fmb = fmb $ fun ma ()
    return a = FunMonad (\() -> a)
    fail = error

-- Пример использования
mGreet = FunMonad (\() -> "Hello")
mText = do
        greet <- return (map toUpper) <*> mGreet
        let text = greet ++ ", world"
        return text
mGreeting = fmap (++ "!") mText
test = fun mGreeting () -- Ожидаемый результат: "HELLO, world!"