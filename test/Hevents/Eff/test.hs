{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes          #-}

class QueryParam a where
  qp :: a -> (String, String)

instance QueryParam a => QueryParam [a] where
  qp = map qp

instance QueryParam (String, String) where
  qp (s,s') = (s, s')

instance QueryParam (Int, Int) where
  qp (s,s') = (show s, show s')


poop1 :: forall qp. QueryParam qp => [qp] -> String
poop1 [] = "empty"
poop1 _  = "full"


poop3 :: Show a => [a] -> String
poop3 [] = "empty"
poop3 _ = "full"

poop4 :: QueryParam qp => [qp] -> String
poop4 [] = "empty"
poop4 _  = "full"
