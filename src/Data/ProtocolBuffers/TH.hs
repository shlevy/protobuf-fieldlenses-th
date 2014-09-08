{-# LANGUAGE TemplateHaskell #-}

module Data.ProtocolBuffers.TH where

import Control.Monad
import Data.Maybe

-- template haskell
import Language.Haskell.TH

-- lens package
import Control.Lens

-- protobuf package
import Data.ProtocolBuffers

makeFieldLenses :: Name -> DecsQ
makeFieldLenses name = do

  -- generate "normal" lenses using the lens package
  lenses <- makeLenses name

  -- generate "normal . field" lens
  fields <- toFields lenses

  return fields

 where

  toFields :: [Dec] -> Q [Dec]
  toFields decs = fmap catMaybes . forM decs $ \dec -> do
    mkSig dec
    >>= maybe (mkFun        dec ) (return . Just)
    >>= maybe (return (Just dec)) (return . Just)

  mkSig :: Dec -> Q (Maybe Dec)
  mkSig (SigD n (ForallT a b (AppT (AppT _iso tyT) aT))) = do
    ty <- [t| Lens' $(return tyT) (FieldType $(return aT)) |]
    return $ Just $
      SigD n (ForallT a b ty)
  mkSig _ = return Nothing

  mkFun :: Dec -> Q (Maybe Dec)

  -- the simple case for one type constructor
  mkFun (FunD n [Clause p (NormalB f) []]) = do
    body <- [| $(return (LamE p f)) . field |]
    return $ Just $
      FunD n [Clause [] (NormalB body) []]

  -- multiple constructors
  mkFun (FunD _ clauses) = do
    if length clauses > 1
      then error "Field lenses do not support multiple data type constructors."
      else error "Unexpected error generating field lenses."

  mkFun _ = return Nothing
