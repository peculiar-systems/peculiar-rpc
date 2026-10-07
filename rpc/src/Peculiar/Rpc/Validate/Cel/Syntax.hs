module Peculiar.Rpc.Validate.Cel.Syntax
  ( Expr (..)
  , Operator (..)
  , Quantifier (..)
  , Compiled (..)
  ) where

import Data.Text qualified as T
import Peculiar.Rpc.Validate.Cel.Value (Value)
import Regex.RE2 (Pattern)

data Operator
  = Equals
  | NotEquals
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Within
  | Plus
  | Minus
  | Times
  | Divide
  | Remainder
  deriving stock (Eq, Show)

data Quantifier
  = Every
  | Some
  | ExactlyOne
  | Keeping
  deriving stock (Eq, Show)

data Compiled = Compiled
  { source :: T.Text
  , regex :: Pattern
  }

data Expr
  = Literal Value
  | Ident T.Text
  | Select Expr T.Text
  | Present Expr T.Text
  | Call T.Text [Expr]
  | Member Expr T.Text [Expr]
  | Index Expr Expr
  | List [Expr]
  | Entries [(Expr, Expr)]
  | Construct T.Text [(T.Text, Expr)]
  | Not Expr
  | Negate Expr
  | And Expr Expr
  | Or Expr Expr
  | Conditional Expr Expr Expr
  | Binary Operator Expr Expr
  | Loop Quantifier T.Text Expr Expr
  | Transform T.Text Expr (Maybe Expr) Expr
  | Matching Expr Compiled
