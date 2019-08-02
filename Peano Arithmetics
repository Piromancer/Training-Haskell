data Nat = Zero | Suc Nat deriving (Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc $ toNat (x - 1)

add :: Nat -> Nat -> Nat
add Zero b = b
add (Suc a) b = add a (Suc b)

mul :: Nat -> Nat -> Nat
mul a b = helper a b
				where
					helper acc Zero = Zero
					helper acc (Suc Zero) = acc
					helper acc (Suc t) = helper (add acc a) t

fac :: Nat -> Nat
fac a = helper (Suc Zero) a
				where 
					helper acc Zero = Suc Zero
					helper acc (Suc Zero) = acc
					helper acc (Suc f) = helper (mul acc (Suc f)) f
