module Hevents.Eff.StoreSpec where

import           Data.Serialize
import           Debug.Trace
import           Hevents.Eff.TestStore
import           Test.Hspec
import           Test.QuickCheck

prop_roundtripSerialization :: StoredEvent SomeString -> Property
prop_roundtripSerialization e = let bs  = runPut $ put e
                                    res = runGet get bs
                                in counterexample ("fail to roundtrip, bytes are " ++ show bs) $
                                   case res of
                                     Right decoded -> decoded == e
                                     Left err      -> error err

spec :: Spec
spec = describe "Store Effect" $ do

  it "should roundtrip event given payload is serializable" $ property $ verbose $ prop_roundtripSerialization
