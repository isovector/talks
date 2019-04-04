module Polysemy.Utils where

import Polysemy

type InterpreterOf r e = ∀ a. Semantic (e ': r) a -> Semantic r a


