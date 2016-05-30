class Searchable b where
    score :: b -> Integer
    neighbors :: b -> [b]


