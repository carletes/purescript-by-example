module FileOperations where

import Data.Array (concatMap, (:))
import Data.Path (Path, ls)


allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)
