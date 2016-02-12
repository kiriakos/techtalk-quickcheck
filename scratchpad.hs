import Test.QuickCheck

powers n = n : map (* n) (powers n)

supertwos = powers 2

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
--                    * Divide by zero: base
--
-- Optimization scenarios:
--  1. if `m` is anything other than zero there is no chance of `n`
--     being an exponent od `base`
--
isPower :: (Integral a, Ord a) => a -> a -> Bool
isPower base n  | base == 0           = False
                | m /= 0              = False
                | d < base            = False
                | d == base && m == 0 = True
                | d > base            = isPower base d
                where   d = div n base
                        m = mod n base

-- isPower should detect all exponents
prop_isPowerDetectsExponents :: Int -> Int -> Bool
prop_isPowerDetectsExponents base exponent
        = isPower base (base^exponent) == True

-- isPower should return False for all non exponents
prop_isPowerIsFalseOtherwise :: Int -> Int -> Bool
prop_isPowerIsFalseOtherwise base exponent =
    let maxOffset = base-1
        exp = base^exponent
        falses = [False | x <- [1..maxOffset]]
    in [isPower base (exp + x) | x <- [1..maxOffset]] == falses
