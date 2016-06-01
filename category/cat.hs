class Arr arr where
  (<.) :: arr b c -> arr a b -> arr a c -- backward composition
  (>.) :: arr a b -> arr b c -> arr a c -- forward composition
  f >. g = g <. f
  f <. g = g >. f

class (Arr arr) => Cat arr where
  _id :: arr a a -- identity arrow

instance Arr (->) where
  f <. g = \x -> f (g x)

instance Cat (->) where
  _id = id


