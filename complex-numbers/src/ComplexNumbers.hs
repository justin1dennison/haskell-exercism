module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, exp, abs)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) =  Complex r (-i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = P.sqrt (r ^ 2 + i ^ 2)

real :: Num a => Complex a -> a
real (Complex r i) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex r i) = i

exp :: Floating a => Complex a -> Complex a
exp (Complex a b) =  Complex r i
     where r = P.cos b * P.exp a
           i = P.sin b * P.exp a

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex r i
     where r = a * c - b * d
           i = a * d + b * c

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex r i
     where r = a + c
           i = b + d

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex r i
     where r = a - c
           i = b - d

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = Complex r i
    where denom =  c ^ 2 + d ^ 2
          r = (a * c + b * d) / denom
	  i = (b * c - a * d ) / denom
