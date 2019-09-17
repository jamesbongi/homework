{- 
1. The library file also defines a parser `int :: Parser Int` for an integer.
    Without looking at this definition, define `int`.  Hint: an integer is
    either a minus symbol followed by a natural number, or a natural number.
-}
int :: Parser Int
int = do char '-'
          xs <- nat
          return ((-1) * xs)
       +++ nat
{- 
3. Using our second grammar for arithmetic expressions, draw the two possible
    parse trees for the expression 2 + 3 + 4.
    
(drawn from)
a.
 expr ::= expr = expr | expr * expr | (expr) | nat
 nat ::= 0 | 1 | 2 | ...

b.

expr ::= expr + expr | term
term ::= term * term | factor
factor ::= expr | nat
nat ::= 0 | 1 | 2...
-}
{-
5. Explain why the final simplification of the grammar for arithmetic expressions has a dramatic effect on the efficiency of the resulting parser.
Hint: begin by considering how an expression comprising a single number would be parsed if this step had not been made.

The first grammar for arithmetic expressions is more efficient because it takes care of and solidifies the first item before going on to
the next. This makes it mroe compatible and efficient with simpler expressions and numbers.
-}
{-
6. Extend the parser for arithmetic expressions to support subtraction and division, based upon the following extensions to the grammar: expr ::= term (+ expr | − expr | )
term ::= factor (∗ term | / term | )

factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...
-}
