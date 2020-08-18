# test
Hi everyone, I'm currently writing a html parser in haskell. I'm using the parsec library. For that I'm using a code snippet I found on the internet. At this time the parser just considers standard elements with an opening and closing tag and no attributes. The code looks like this:


``data Html = Element String [Html] | Content String
    deriving Show

element :: Parser Html
element = do
    name <- char '<' *> many1 letter <* char '>'
    children <- many $ (try element) <|> content
    string "</" >> string name >> char '>'
    return $ Element name children

content :: Parser Html
content = fmap Content $ many1 $ satisfy (\x -> x /='<')``
