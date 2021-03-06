import Test.QuickCheck

powers n = n : map (* n) (powers n)
supertwos = powers 2

isPowerNaive base n = n `elem` pows
    where pows = takeWhile (<=n) (powers base)

-- Identifies whether Number `n` is an exponent of Number `base`
--
--  The strategy here is based on `n`'s division properties with `base`,
--  `d` for division result and `m` for modulo result:
--      1. If `d` is smaller than `base` the  result is `False`
--      2. If `d` is exactly `base` and `m` is zero `n` is an exponent
--         of `base`.
--      3. If `d` is bigger than `base` the result is based on recursing
--         switching `n` with `d`
--
-- Failure scenarios: * remove `&& m==0`
--                    * last guard: `d < n` or `d < base`
--                    * See optimization 1.
--                    * Divide by zero: base, implementation does not
--                      account for zero as n being wrong.
--                    * `n` equaling one is an automatic True (0 exp)
--                    * Impl does not account for negative bases, we could
--                      add `abs` usage to the implementation but for now
--                      brevity adjust the generators
--                    * Does not check for identity of `base` and `n`
--
-- Failures in Spec: * We only want to check for positive exponents.
--                     Modify properties accordingly and return Property
--                     type.
--                   * Negative spec generates faulty cases
--
--
-- Optimization scenarios:
--  1. if `m` is anything other than zero there is no chance of `n`
--     being an exponent od `base`
--
-- FINAL
isPower :: (Integral a, Ord a) => a -> a -> Bool
isPower base n
                | base == 0           = True
                | n == 1              = True
                | n == 0              = False
                | m /= 0              = False
                | base == n           = True
                | d < base            = False
                | d == base && m == 0 = True
                | d > base            = isPower base d
                where   d = div n base
                        m = mod n base

-- isPower should detect all exponents
prop_isPowerDetectsExponents (Positive base) (NonNegative exponent) =
    let exp = base^exponent
    in isPower base exp == True

-- isPower should return False for all non exponents
prop_isPowerIsFalseOtherwise (Positive base) (NonNegative exponent) =
    let maxOffset = base - 1
        exp = base^exponent
        range | exponent == 0 = []
              | maxOffset > 1 = [1..maxOffset]
              | otherwise = []
        falses = [False | x <- range]
    in  [isPower base (exp + x) | x <- range] == falses











-- V2 identity bug
isPower' :: (Integral a, Ord a) => a -> a -> Bool
isPower' base n
                | m /= 0              = False
                | base == n           = True
                | d < base            = False
                | d == base && m == 0 = True
                | d > base            = isPower' base d
                where   d = div n base
                        m = mod n base

-- isPower should detect all exponents
prop_isPowerDetectsExponents' (Positive base) (NonNegative exponent) =
    let exp = base^exponent
    in isPower' base exp == True

-- isPower should return False for all non exponents
prop_isPowerIsFalseOtherwise' (Positive base) (NonNegative exponent) =
    let maxOffset = base - 1
        exp = base^exponent
        range | exponent == 0 = []
              | maxOffset > 1 = [1..maxOffset]
              | otherwise = []
        falses = [False | x <- range]
    in  [isPower' base (exp + x) | x <- range] == falses

























-- V1 modulo bug
isPower'' :: (Integral a, Ord a) => a -> a -> Bool
isPower'' base n
                | m /= 0              = False
                | d < base            = False
                | d == base           = True
                | d > base            = isPower'' base d
                where   d = div n base
                        m = mod n base

-- isPower should detect all exponents
prop_isPowerDetectsExponents'' (Positive base) (NonNegative exponent) =
    let exp = base^exponent
    in isPower'' base exp == True

-- isPower should return False for all non exponents
prop_isPowerIsFalseOtherwise'' (Positive base) (NonNegative exponent) =
    let maxOffset = base-1 -- Optimistic
--        maxOffset = (base^(exponent+1)) - exp - 1 -- Naive, full
        exp = base^exponent
        range | exponent == 0 = []
              | maxOffset > 1 = [1..maxOffset]
              | otherwise = []
        falses = [False | x <- range]
    in  [isPower'' base (exp + x) | x <- range] == falses




















-- V-Naive
isPower''' :: (Integral a, Ord a) => a -> a -> Bool
isPower''' base n
                | d < base   = False
                | d == base  = True
                | d > base   = isPower''' base d
                where   d = div n base
                        m = mod n base

-- isPower should detect all exponents
prop_isPowerDetectsExponents''' (Positive base) (NonNegative exponent) =
    let exp = base^exponent
    in isPower''' base exp == True

-- isPower should return False for all non exponents
prop_isPowerIsFalseOtherwise''' (Positive base) (NonNegative exponent) =
    let maxOffset = base-1 -- Optimistic
--        maxOffset = (base^(exponent+1)) - exp - 1 -- Naive, full
        exp = base^exponent
        range | exponent == 0 = []
              | maxOffset > 1 = [1..maxOffset]
              | otherwise = []
        falses = [False | x <- range]
    in  [isPower''' base (exp + x) | x <- range] == falses

