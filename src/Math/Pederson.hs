{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module Math.Pederson where

import System.Random (RandomGen(..), Random(..))

-- | p, q are prime, q divides p - 1, g and h are generators for
-- the cyclic subgroup of Z*_p of order q.
data PublicQuadruple = PublicQuadruple
  { p :: Integer
  , q :: Integer
  , g :: Integer
  , h :: Integer
  }

newtype Commitment = Commitment { unCommitment :: Integer }

newtype Proof = Proof { unProof :: Integer }

newtype Message = Message { unMessage :: Integer }

commit :: RandomGen gen => PublicQuadruple -> Message -> gen -> (Commitment, Proof, gen)
commit PublicQuadruple { p, q, g, h } msg gen = (commitment, proof, gen') where
  (r, gen') = randomR (0, q - 1) gen
  commitment = Commitment (g ^ unMessage msg * h ^ r)
  proof = Proof r

verify :: PublicQuadruple -> Commitment -> Message -> Proof -> Bool
verify PublicQuadruple { p, q, g, h } commitment msg proof = unCommitment commitment == g ^ unMessage msg * h ^ unProof proof
