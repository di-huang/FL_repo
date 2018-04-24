module SnocLists where

data SList a = Snil | Scons (SList a) a
  deriving Show

sappend :: SList a -> SList a -> SList a
sappend Snil e = e
sappend (Scons t h) e = Scons (sappend t e) h

slength :: SList a -> Int
slength Snil = 0
slength (Scons t h) = 1 + slength t

smap :: (a -> b) -> SList a -> SList b
smap _ Snil = Snil
smap f (Scons t h) = Scons (smap f t) (f h)

sfilter :: (a -> Bool) -> SList a -> SList a
sfilter _pred Snil = Snil
sfilter pred (Scons t h)
  | pred h       = Scons (sfilter pred t) h
  | otherwise  = sfilter pred t

sintersperse :: a -> SList a -> SList a
sintersperse _ Snil = Snil
sintersperse sep (Scons t h) = Scons (sprependToAll sep t) h

sprependToAll :: a -> SList a -> SList a
sprependToAll _ Snil = Snil
sprependToAll sep (Scons t h) = Scons (Scons (sprependToAll sep t) h) sep

-- given a list of lists l1 ... lk , return the concatenation of l1 through lk.
sconcat :: SList (SList a) -> SList a
sconcat Snil = Snil
sconcat (Scons Snil (Scons t d)) = Scons t d
sconcat (Scons t1 (Scons t0 d)) = sappend (Scons t0 d) (sconcat t1)

-- some testLists
testList = Scons (Scons (Scons Snil 3) 2) 1
testList2 = Scons (Scons Snil 5) 4
testList3 = Scons (Scons Snil True) True
testList4 = Scons Snil testList
testList5 = Scons (Scons Snil testList2) testList
