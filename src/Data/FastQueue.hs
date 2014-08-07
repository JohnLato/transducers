module Data.FastQueue(module Data.Interface.TSequence, FastQueue) where

import Data.Interface.TSequence
import Data.RTQueue
import Data.CTQueue

type FastQueue =  CTQueue RTQueue
