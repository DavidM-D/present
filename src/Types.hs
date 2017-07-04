{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, RankNTypes #-}
module Types () where

import Time.Types
import Control.Applicative
import Data.Maybe

data Contract where
 Zero :: Contract
 One :: Currency -> Contract
 Give :: Contract -> Contract
 And :: Contract -> Contract -> Contract
 Or :: Contract -> Contract -> Contract
 Truncate :: DateTime -> Contract -> Contract
 Then :: Contract -> Contract -> Contract
 Scale :: Obs Double -> Contract -> Contract
 Get :: Contract -> Contract
 Anytime :: Contract -> Contract

zero = Zero
one = One
give = Give
and = And
orC = Or
truncateC = Truncate
thenC = Then
scale = Scale
get = Get
anytime = Anytime

traverseC :: forall a . (a -> Contract -> a) -> a -> Contract -> a
traverseC f i c = case c of
  Zero -> end
  One _ -> end
  Give c1 -> continue c1
  And c1 c2 -> branch c1 c2
  Or c1 c2 -> branch c1 c2
  Then c1 c2 -> branch c1 c2
  Get c1  -> continue c1
  Anytime c1 -> continue c1
  Truncate _ c1 -> continue c1
  Scale _ c1 -> continue c1
  where end = f i c
        continue c1 = f end c1
        branch c1 c2 = f (continue c1) c2

fxSpot :: Currency -> Currency -> Obs Double
fxSpot = undefined

base :: Currency
base = undefined


obsBase :: Currency -> Obs Double
obsBase c = fxSpot c base

toBase :: Currency -> Obs Double
toBase c = obsBase c

maturity :: Contract -> Maybe DateTime
maturity c = case c of
  Zero -> Nothing
  One _ -> Nothing
  Give c1 -> maturity c1
  And c1 c2 -> maxM c1 c2
  Or c1 c2 -> maxM c1 c2
  Then c1 c2 -> maxM c1 c2
  Get c1 -> maturity c1
  Anytime c1 -> maturity c1
  Truncate d c1 -> Just $ maybe d (min d) $ maturity c1
  Scale _ c1 -> maturity c1
  where maxM c1 c2 = liftA2 max (maturity c1) (maturity c2)


class Applicative m => Model m where
  discrete :: Contract -> DateTime -> m a
  snell    :: Contract -> DateTime -> m a
  fromObs  :: Obs a    -> DateTime -> m a

value :: Model f
      => Maybe DateTime -- ^ start
      -> Contract
      -> f Double
value start c = case c of
  Zero          -> pure 0
  One c1        -> fo (toBase c1) start
  Give c1       -> negate <$> v c1
  And c1 c2     -> liftA2 (+) (v c1) (v c2)
  Or c1 c2      -> liftA2 max (v c1) (v c2)
  Then c1 c2    -> liftA2 (+) (v c1) (value (maturity c1) c2)
  Get c1        -> discrete c1 $ notInf (maturity c1)
  Anytime c1    -> snell c1 $ notInf start
  Truncate _ c1 -> v c1
  Scale obs c1  -> liftA2 (*) (fromObs obs $ notInf start) $ v c1
  where v = value start
        fo o = fromObs o . notInf
        notInf = fromMaybe
                (error "attempted to observe a value at the end of time")


data Obs a = FromMarket { _observationDate :: DateTime
                        , _observationID :: String}
           | Pure a

data Currency = Currency { _unCurrency :: String }

constObs :: a -> Obs a
constObs = Pure

depends :: Contract -> [(DateTime, String)]
depends = traverseC f []
  where f xs c = case c of
          Scale (FromMarket dt ob) _ -> (dt,ob) : xs
          _ -> xs

getOn :: DateTime -> Contract -> Contract
getOn maturity = get . truncateC maturity

scaleConst :: Double -> Contract -> Contract
scaleConst = scale . constObs

payment :: DateTime -> Double -> Currency -> Contract
payment maturity n currency =
  getOn maturity $ scaleConst n $ one currency

european :: DateTime -> Contract -> Contract
european d c = getOn d (c `Or` zero)

usd :: Currency
usd = undefined

gbp :: Currency
gbp = undefined


christmas :: DateTime
christmas = undefined

pay :: Double -> Currency -> Contract
pay m = Give . recieve m

recieve :: Double -> Currency -> Contract
recieve m = scaleConst m . one

a = european christmas (recieve 100 usd) `And` (pay 90 gbp)

american :: DateTime -> DateTime -> Contract -> Contract
american start finish c = zeroUntil start $ anytime $ perhaps finish c

perhaps :: DateTime -> Contract -> Contract
perhaps mat c = Truncate mat (c `Or` Zero)

swap :: [DateTime] -> Contract -> Contract
swap = undefined

zeroUntil :: DateTime -> Contract -> Contract
zeroUntil start c = get (Truncate start Zero) `Then` c

