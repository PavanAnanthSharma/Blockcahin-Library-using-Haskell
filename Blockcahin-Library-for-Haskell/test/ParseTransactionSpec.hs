module ParseTransactionSpec (test) where

import Data.ByteString.Base16.Lazy (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get
import Control.Applicative ((<|>))
import ParserUtil

import Transaction

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

testVector = [
  (
    "02000000032e0c96da80852566acc19593241280864ae1d7e58c59be615b924148a8e0448d000000004847304402204b4bec8414b91b511b67d57da80bd97d3bf7d48e84b858d78a423496478e2f0202204f80802b95d847941bb15deaa960a91b74a3a987b6e1e14f4f96b365acca951c01fdffffff711a78ac566f7a6de2987d2be08a12966ea4742ece6ce0db49ba0186c4427d9e0000000048473044022055bfd2f65be0412fdac8f39165e99640f863dcaeae689bef45a6127e02387c8a02201836378b31061b9fdb6c4fdd3617a45a454a770632942ec47c5be091d66785d801fdffffff8ec4f91b0a6e9cb2498b5e3e5576d1b1842d8685fce0bc56da09a935bc6c63bc00000000484730440220102b7eb7c2d20809cace8882c4da2dc42963945ddbd7088306ebaf1a316a7850022044e490a0a230adf41a8e046761569efc132899b68e7191801d2d5045f91cbe2001fdffffff02e4d1052a01000000160014d1ad7293ccf5aff68be90d2da6ed0e840fa98ae500e40b5402000000160014ee31c6c262423c09f715c9d21aa1381a8e127ae0cb000000",
    Transaction {
      transactionIsSegwit = False,
      transactionVersion = 2,
      transactionMarker = Nothing,
      transactionFlag = Nothing,
      transactionTxins = [
        Txin {
          txinHash = decodeHex $ "2e0c96da80852566acc19593241280864ae1d7e58c59be615b924148a8e0448d",
          txinIndex = 0,
          txinScript = decodeHex $ "47304402204b4bec8414b91b511b67d57da80bd97d3bf7d48e84b858d78a423496478e2f0202204f80802b95d847941bb15deaa960a91b74a3a987b6e1e14f4f96b365acca951c01",
          txinSequenceNo = 4294967293
        },
        Txin {
          txinHash = decodeHex $ "711a78ac566f7a6de2987d2be08a12966ea4742ece6ce0db49ba0186c4427d9e",
          txinIndex = 0,
          txinScript = decodeHex $ "473044022055bfd2f65be0412fdac8f39165e99640f863dcaeae689bef45a6127e02387c8a02201836378b31061b9fdb6c4fdd3617a45a454a770632942ec47c5be091d66785d801",
          txinSequenceNo = 4294967293
        },
        Txin {
          txinHash = decodeHex $ "8ec4f91b0a6e9cb2498b5e3e5576d1b1842d8685fce0bc56da09a935bc6c63bc",
          txinIndex = 0,
          txinScript = decodeHex $ "4730440220102b7eb7c2d20809cace8882c4da2dc42963945ddbd7088306ebaf1a316a7850022044e490a0a230adf41a8e046761569efc132899b68e7191801d2d5045f91cbe2001",
          txinSequenceNo = 4294967293
        }
      ],
      transactionTxouts = [
        Txout {
          txoutAmount = 4999991780,
          txoutScript = decodeHex $ "0014d1ad7293ccf5aff68be90d2da6ed0e840fa98ae5"
        },
        Txout {
          txoutAmount = 10000000000,
          txoutScript = decodeHex $ "0014ee31c6c262423c09f715c9d21aa1381a8e127ae0"
        }
      ],
      transactionLocktime = 203,
      transactionWitness = Nothing
    }
  ),
  (
    "01000000000101b04613c21ede2fb0ae2a1e9c28f0ce165df76e927a39cf2ffba9b4573f0a1c910000000000fdffffff020000000000000000166a14e381a6e38199e381a8e381a0e382882121212121269c00000000000016001481eab72a9b738962f178dc833cc56ca9b210151e02473044022079cc2e04889802b4623813232fa3075929dab3395f4f7a0f0ab6e032c84412ca02200c8e51fae2a8aa56642f272ccb8655a6e26dc28aa36c5b9e237ee51fa8fff1df012102383da0568d9536aa96c9d0514578c1a75e14c95fee4a9330f1fee7591640e51ea5650800",
    Transaction {
      transactionIsSegwit = True,
      transactionVersion = 1,
      transactionMarker = Just 0,
      transactionFlag = Just 1,
      transactionTxins = [
        Txin {
          txinHash = decodeHex $ "b04613c21ede2fb0ae2a1e9c28f0ce165df76e927a39cf2ffba9b4573f0a1c91",
          txinIndex = 0,
          txinScript = decodeHex $ "",
          txinSequenceNo = 4294967293
        }
      ],
      transactionTxouts = [
        Txout {
          txoutAmount = 0,
          txoutScript = decodeHex $ "6a14e381a6e38199e381a8e381a0e382882121212121"
        },
        Txout {
          txoutAmount = 39974,
          txoutScript = decodeHex $ "001481eab72a9b738962f178dc833cc56ca9b210151e"
        }
      ],
      transactionLocktime = 550309,
      transactionWitness = Just $ Witness [[decodeHex $ "3044022079cc2e04889802b4623813232fa3075929dab3395f4f7a0f0ab6e032c84412ca02200c8e51fae2a8aa56642f272ccb8655a6e26dc28aa36c5b9e237ee51fa8fff1df01", decodeHex $ "02383da0568d9536aa96c9d0514578c1a75e14c95fee4a9330f1fee7591640e51e"]]
    }
  )
  ]

test :: IO ()
test = do
  print testVector
  hspec $ do
    describe "ParseTransaction.parseTransaction" $ do
      it "can correctly parse test vectors" $
        all (\(h, t) -> getTransactionFromHexString h == t) testVector
      it "can correctly serialize test vectors" $
        all (\(h, t) -> putTransactionToHexString t == h) testVector
      it "for all test vectors, get . put = id" $
        all (\(h, _) -> putTransactionToHexString (getTransactionFromHexString h) == h) testVector
      it "for all test vectors, put . get = id" $
        all (\(_, t) -> getTransactionFromHexString (putTransactionToHexString t) == t) testVector