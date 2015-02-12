{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

import Data.Text (Text)
import Data.Int (Int64)

import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

import GHC.Generics

data TestMessage = TestMessage
  { _testString :: Optional 1 (Value Text)
  , _testInt    :: Optional 2 (Value Int64)
  }
  deriving (Eq, Show, Generic)

instance Encode TestMessage
instance Decode TestMessage

makeFieldLenses ''TestMessage

main :: IO ()
main = putStrLn "Build successful"
