# A Derivative Approach to Pricing Derivatives
## David Millar-Durrant

---
class: middle


# Derivatives
## A financial product which has a value derived from an underlying variable asset

---
class: middle

# Future
## Next week I will give you $110 and you will give me £100

---
class: middle

# Option
## I would like to be able to buy £100 for $110 if I want

---
class: middle

# Swap
## I will give you £10 for $11 every day this week


---
class: middle

# These Derivatives are Composable
* An option to buy a swap is called a Swaption
* You can promise to buy a option in the future 


---
# The Combinators 

```haskell
Zero :: Contract
```
Zero is a contract that may be acquired at any time. It has no rights and no obligations, and has an infinite horizon

---
# The Combinators

```haskell
One :: Currency -> Contract
```
One is a contract that immediately pays the holder one unit of the currency k.  The contract has no maturity

--

```haskell
One USD
```
means I expect to be paid $1 immediately

---
# The Combinators

```haskell
Give :: Contract -> Contract
```
give inverts whatever contract it is applied to

--

```haskell
Give (One USD)
```
means I will pay $1 immediately

---
# The Combinators 
```haskell
And :: Contract -> Contract -> Contract
```

And  has the effect of both contracts contained with in it

--

```haskell 
Give (One USD) `And` (One GBP)
```
means I will give $1 and receive £1

---
# The Combinators 

```haskell
Or :: Contract -> Contract -> Contract
```
Or has the effect of either contract

--

```haskell
One USD `Or` One GBP
```
is the contract to have the choice to either receive $1 or £1

---
# The Combinators

```haskell
Truncate :: DateTime -> Contract -> Contract
```

Truncate adds an expiry date to a contract

---
# The Combinators

```haskell
Then :: Contract -> Contract -> Contract
```
Then is the first contract until it's expiry then it is the second contract until expiry

---
# Observables

```haskell
constObs :: a -> Obs a

libor3M :: Obs Double

usdGBPRate :: Obs Double

varietiesOfBeerAtLevel33 :: Obs Int
```

---
# The Combinators

```haskell
Scale :: Obs Double -> Contract -> Contract
```
Scale multiplies whatever the effect of the contract is by the value of the observable

--

```haskell
Scale (constObs 5 :: Obs Double) (One USD)
```
means I will get paid $5

--

```haskell
Scale libor3M (One USD)
```
means I will get paid $1 times by the London Lending Rate

---
# The Combinators

```haskell
Get :: Contract -> Contract
```
Get forces you to exercise the contract at it's expiry date

--

```haskell
Get (Truncate (read "10-07-2017" :: DateTime) (One USD))
```
means you will get paid $1 on the 10th of July

---
# The Combinators

```haskell
Anytime :: Contract -> Contract
```

Anytime forces you to exercise the contract any time between now and the contracts maturity

--

```haskell
Anytime (Truncate (read "10-07-2017" :: DateTime) (One USD))
```
means you will receive $1 at a time of your choosing between now and the 10th of July

---
# The Combinators

```haskell
data Contract where
   Zero     :: Contract
   One      :: Currency -> Contract
   Give     :: Contract -> Contract
   And      :: Contract -> Contract -> Contract
   Or       :: Contract -> Contract -> Contract
   Truncate :: DateTime -> Contract -> Contract
   Then     :: Contract -> Contract -> Contract
   Scale    :: Obs Double -> Contract -> Contract
   Get      :: Contract -> Contract
   Anytime  :: Contract -> Contract
```

---

# Building Contracts
## A payment

```haskell
payment :: DateTime -> Double -> Currency -> Contract
payment maturity n currency =
  Get $ Truncate maturity $ Scale (constObs n) $ One currency
```

---

# Building Contracts
## A payment

```haskell
getOn :: DateTime -> Contract -> Contract
getOn maturity = Get . Truncate maturity

scaleConst :: Double -> Contract -> Contract
scaleConst = Scale . constObs

payment :: DateTime -> Double -> Currency -> Contract
payment maturity n currency =
  getOn maturity $ scaleConst n $ One currency
```

---

```haskell
zero = Zero
one = One
give = Give
and = And
or = Or
truncateC = Truncate
thenC = Then
scale = Scale
get = Get
anytime = Anytime
```

---

# Building Contracts
## A payment

```haskell
getOn :: DateTime -> Contract -> Contract
getOn maturity = get . truncateC maturity

scaleConst :: Double -> Contract -> Contract
scaleConst = scale . constObs

payment :: DateTime -> Double -> Currency -> Contract
payment maturity n currency =
  getOn maturity $ scaleConst n $ one currency
```

---

# Building Contracts
## European Options

```haskell
european :: DateTime -> Contract -> Contract
european d c = getOn d (c `Or` zero)
```

```haskell
c1 :: Contract
c1 = european christmas ((scaleConst 100 (one usd)) 
       `And` (give $ scaleConst 90 (one gbp)))
```
you have the option to buy $100 for £90 on christmas day

---

# Building Contracts
## European Options

```haskell
european :: DateTime -> Contract -> Contract
european d c = getOn d (c `Or` zero)
```

```haskell
pay :: Double -> Currency -> Contract
pay m = give . recieve m

recieve :: Double -> Currency -> Contract
recieve m = scaleConst m . one

c1 :: Contract
c1 = european christmas (recieve 100 usd) `And` (pay 90 gbp)
```
you have the option to buy $100 for £90 on christmas day

---
# Building Contracts
## American Options

```haskell
american :: DateTime -> DateTime -> Contract -> Contract
american start finish c = get (Truncate start Zero) `Then` opt
  where
    opt :: Contract
    opt = anytime (Truncate finish (c `Or` Zero))
```
---
# Building Contracts
## American Options

```haskell
zeroUntil :: DateTime -> Contract -> Contract
zeroUntil start c = get (Truncate start Zero) `Then` c

perhaps :: DateTime -> Contract -> Contract
perhaps mat c = Truncate mat (c `Or` Zero)

american :: DateTime -> DateTime -> Contract -> Contract
american start finish c = zeroUntil start $ anytime $ perhaps finish c
```

---
