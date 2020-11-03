-- Put your Preprocessor implementation in this file.
module PreprocessorImpl where

import Types
-- Probably more imports here

clausify :: Program -> Either ErrMsg IDB
clausify = undefined

stratify :: IDB -> [PSpec] -> Either ErrMsg [[PSpec]]
stratify = undefined
