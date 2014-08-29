{-# LANGUAGE TemplateHaskell #-}

module Data.ProtocolBuffers.TH where

import Control.Monad

import Data.Maybe

-- template haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- lens package
import Control.Lens

import Data.ProtocolBuffers

makeFieldLenses :: Quasi m => Name -> m [Dec]
makeFieldLenses name = runQ $ do

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
  mkFun (FunD n [Clause [] (NormalB f) []]) = do
    body <- [| $(return f) . field |]
    return $ Just $
      FunD n [Clause [] (NormalB body) []]
  mkFun _ = return Nothing
