{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, RecordWildCards, TypeApplications #-}

module Math.Pederson where

import System.Random (RandomGen(..), Random(..), mkStdGen, StdGen)
import Control.Exception (assert)

-- | p, q are prime, q divides p - 1, g and h are generators for
-- the cyclic subgroup of Z*_p of order q.
data PublicQuadruple = PublicQuadruple
  { p :: Integer
  , q :: Integer
  , g :: Integer
  , h :: Integer
  }
  deriving (Eq, Show, Read)

newtype Commitment = Commitment { unCommitment :: Integer }
  deriving (Eq, Show, Read)

newtype Proof = Proof { unProof :: Integer }
  deriving (Eq, Show, Read)

newtype Message = Message { unMessage :: Integer }
  deriving (Eq, Show, Read)

commit :: RandomGen gen => PublicQuadruple -> Message -> gen -> (Commitment, Proof, gen)
commit PublicQuadruple { p, q, g, h } msg gen = (commitment, proof, gen') where
  (r, gen') = randomR (0, q - 1) gen
  commitment = Commitment (g ^ unMessage msg * h ^ r)
  proof = Proof r

verify :: PublicQuadruple -> Commitment -> Message -> Proof -> Bool
verify PublicQuadruple { p, q, g, h } commitment msg proof = unCommitment commitment == g ^ unMessage msg * h ^ unProof proof

test :: IO ()
test = do
  let gen :: StdGen = mkStdGen 100
  let p = 61
  let q = 5
  let (g, h) = findGH p q
  let publicQuadruple = PublicQuadruple {..}
  print publicQuadruple
  let message = Message 100
  print message
  let (commitment, proof, gen) = commit @StdGen publicQuadruple message gen
  print (commitment, proof, gen)
  if verify publicQuadruple commitment message proof then putStrLn "Success!" else putStrLn "Failure"
  

findGH :: Integer -> Integer -> (Integer, Integer)
findGH p q = go 2 where
  go i = if (i * q) `mod` p == 1 then (i, go' (i + 1)) else go (i + 1)
  go' i = if (i * q) `mod` p == 1 then i else go' (i + 1)
