-- StableMatch.hs
-- stable matching algorithm

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Debug.Trace

type Person   = String
type Pref     = (Person, [Person]) -- a preference list (in descending order for a person)
type Match    = [(Person,Person)]
type MatchMap = M.Map Person Person

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

-- * women pick their first preference for a man
-- * unmatched men have to accept the offer;
-- * matched men can accept the offer if it is better than their current one
stableMatch :: [Pref] -> [Pref] -> Match
stableMatch women men = M.toList $ stableMatch_ women' men M.empty
  where women' = map (\(women,prefs) -> (women,0,prefs)) women

stableMatch_ :: [(Person,Int,[Person])] -> [Pref] -> MatchMap -> MatchMap
stableMatch_ women men partial
  -- everyone has been matched (we assume |women| = |men|)
  | length partial == length women = partial
  | otherwise = 
    -- unmatched women propose
    let (women',proposals) = unzip $ map propose (filter unmatched women) in
    let women'' = updateWomen women' in
    -- men only accept new proposals if they're unmatched or
    -- it's better than their current match
    let newMatches = filter accept proposals in
    -- update partial solution
    let partial' = foldr updateMatch partial newMatches in
    stableMatch_ women'' men partial'

  where matchedWomen = map snd $ M.toList partial
        unmatched (woman,_,_) = not $ woman `L.elem` matchedWomen
        propose (woman,i,prefs) = ((woman,i+1,prefs),(prefs !! i,woman))
        updateWomen w' =
          let womenWhoProposed = map fst3 w' in
          w' ++ filter (not . flip elem womenWhoProposed . fst3) women
        betterMatch man w1 w2 =
          let manPref = maybe [] id $ man `L.lookup` men in
          let i1 = L.findIndex (== w1) manPref in
          let i2 = L.findIndex (== w2) manPref in
          maybe False id $ (<) <$> i1 <*> i2
        accept (man,woman) = maybe True (betterMatch man woman) (M.lookup man partial)
        updateMatch (man,woman) = M.insert man woman . M.delete man

main = do
  let women = [("Jane",["Bob","Man","Guy"]),
              ("Mary",["Bob","Guy","Man"]),
              ("Sia",["Guy","Man","Bob"])]
  let men = [("Guy",["Jane","Mary","Sia"]),
            ("Man",["Jane","Sia","Mary"]),
            ("Bob",["Jane","Sia","May"])]

  -- stable match: Bob-Jane, Guy-Mary, Man-Sia
  print $ stableMatch women men
