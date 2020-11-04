{- Functional Programming 1
   Assignment 2
 -}
-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module Assignment2(sumSquareDiff,
            isMatch) where

import Prelude hiding (product)
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Exercise 1: Specification, Variant, Evaluation
   ==============================================

   Consider the following function declaration.

   Provide a complete function specification (in accordance with our Coding
   Convention).

   Give a variant for `product n' that proves termination for all n >= 1.

   Give (in a Haskell comment) a detailed step-by-step evaluation of
   `product 2', using the same style as in the lecture slides.   
 -}

{- product n
   Computes the product of the first n natural numbers
   PRE: n >= 2
   RETURNS: n * (n - 1) * (n - 2)... * (n - (n - 1))
   EXAMPLES: product 2 == 2 
             product 10 == 3628800
 -}

-- VARIANT: n
product 1 = 1
product n = n * product (n - 1)

 {- Step-by-step evaluation:

   product 2
   --> n * product (n - 1) (where n = 2)
   --> 2 * product (n - 1) (where n - 1 = 2 - 1)
   --> 2 * product (2 - 1) (where 2 - 1 = 1)
   --> 2 * product (1) (where product 1 = 1)
   --> 2 * 1
   --> 2
 -}

{- Exercise 2: Sum-Square-Difference
   =================================

   The sum of the squares of the first ten natural numbers is

     1^2 + 2^2 + ... + 10^2 = 385

   The square of the sum of the first ten natural numbers is

     (1+2+...+10)^2 = 3025

   Hence the difference between the square of the sum of the first ten natural
   numbers and the sum of the squares is 3025 - 385 = 2640.

   Declare a function `sumSquareDiff n' that returns the difference between the
   square of the sum and the sum of the squares of all natural numbers from 1 to
   n.  (You must decide how to handle the case when n < 1.)

   (This exercise was inspired by Problem 6 at Project Euler
   [ https://projecteuler.net/ ], a web site that offers hundreds of challenging
   programming problems.)
 -}

{- sumOfSquares n
   Computes the sum of squares of first n natural numbers
   PRE: n >= 1
   RETURNS: (n)^2 + (n - 1)^2 + .... + (n - (n - 1))^2
   EXAMPLES: sumOfSquares 3 == 14
             sumOfSquares 10 == 385
 -}
-- VARIANT: n
sumOfSquares :: Integer -> Integer
sumOfSquares 1 = 1
sumOfSquares n = (n ^ 2) + sumOfSquares (n - 1)

{- sumOfIntegers n
   Computes the sum of the first n natural numbers
   PRE: n >= 1
   RETURNS: n + (n - 1) + (n - 2) + .... + (n - (n - 1))
   EXAMPLES: sumOfIntegers 5 == 15
 -}
-- VARIANT: n
sumOfIntegers :: Integer -> Integer
sumOfIntegers 1 = 1
sumOfIntegers n = n + sumOfIntegers (n - 1)

{- sumSquareDiff n
   The Sum-Square-Difference function
   RETURNS: The Sum-Square-Difference of first n natural numbers OR 0 which indicates the user has input a value <= 1
   EXAMPLES: sumSquareDiff 5 == 170
             sumSquareDiff 10 == 2640
             sumOfSquares (-10) == 0
             sumOfSquares (1) == 0
 -}
sumSquareDiff :: Integer -> Integer
sumSquareDiff n | n <= 1 = 0
sumSquareDiff n = ((sumOfIntegers n) ^ 2) - (sumOfSquares n)


{- Exercise 3: Wildcard Matching
   =============================

   For this exercise, we define a /string pattern/ to be a string. String
   patterns may contain '?', '*' and any other character. (Note that this
   definition of string patterns is not related to patterns for values of type
   String in the Haskell language.)

   String patterns can /match/ a string: '?' matches any single character, and
   '*' matches any sequence of characters (including the empty sequence). Other
   characters in a string pattern only match themselves. The empty pattern only
   matches the empty string.

   Declare a function `isMatch' such that `isMatch s p' returns `True' if (and
   only if) the string pattern p matches the entire string s.

   Examples:
   - isMatch "aa" "a" == False because "a" does not match the entire string "aa"
   - isMatch "aa" "*" == True because "*" matches any sequence of characters
   - isMatch "cb" "?a" == False because '?' matches 'c' but 'a' does not match 'b'
   - isMatch "adceb" "*a*b" == True because the first '*' matches the empty
     sequence, and the second '*' matches "dce"

   (This exercise was inspired by Problem 44 at LeetCode
   [ https://leetcode.com/problemset/all/ ], another web site that offers
   hundreds of challenging programming problems.)
 -}

{- isMatch s p
   String matching function
   RETURNS: True if the strings are equal and False otherwise
   EXAMPLES: isMatch "aa" "a" == False
             isMatch "abc" "abc" == True
             isMatch "hi" "h?" == True
             isMatch "adceb" "*a*b" == True
             isMatch "hello" "*hell" == False
 -}
 
-- for matching the "*" case
isMatch _ "*" = True
-- for handling exceptions
isMatch "" "" = True
isMatch "" p | length p >= 1 = False
isMatch s "" | length s >= 1 = False
-- for matching the '?' character in the p string
isMatch (x:xs) ('?':ys) = isMatch (xs) (ys)
-- for matching the "adceb" "*a*b" in the p string
isMatch (x:xs) ('*':y:ys) = if (x == y) then isMatch (xs) (ys) else isMatch (xs) ('*':y:ys)
--main function/entry point
isMatch s p = if (head s == head p) then isMatch (tail s) (tail p) else False