module Utils.Record where

import Prelude
import Data.Symbol          (class IsSymbol, SProxy)
import Record               as Rec
import Prim.Row             (class Lacks, class Cons
                            ,class Nub, class Union)
import Timestamp            (Timestamp(..)
                            ,formatToDateTimeShortStr
                            ,defaultTimestamp)

-- Converts a record field from Timestamp
-- to a string
fromTimestampField :: forall r1 r2 r l
                    . IsSymbol l
                   => Lacks l r
                   => Cons l Timestamp r r1
                   => Cons l String r r2
                   => SProxy l
                   -> Record r1
                   -> Record r2
fromTimestampField sym rec = 
  Rec.insert sym val $ Rec.delete sym rec
  where
    val = formatToDateTimeShortStr 
        $ Rec.get sym rec

