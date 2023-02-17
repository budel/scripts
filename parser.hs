split str = case break (==',') str of
                (a, _comma:b) -> a : split b
                (a, _empty)   -> [a]


