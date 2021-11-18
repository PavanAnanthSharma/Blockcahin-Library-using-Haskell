# Blockchain Library for Haskell

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```
ED/DV: Pavan Ananth Sharma
```

A blockchain library for haskell.

## Reference

### Parse a raw bitcoin transaction
Use a haskell library [binary](https://hackage.haskell.org/package/binary) to parse.

```haskell
getTransaction :: Get Transaction
```

```haskell
getTransactionFromHexString :: String -> Transaction
```
Both functions will automatically detect whether the given transaction is segwit format or not.

An example:

```haskell
import Data.ByteString.Base16.Lazy (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get
import Control.Applicative ((<|>))
import ParserUtil
import Transaction

main :: IO ()
main = print $ getTransactionFromHexString "01000000000101....."
```

See <https://github.com/aidatorajiro/blockchain-haskell/blob/master/test/ParseTransactionSpec.hs> for test vectors.

### Create a raw bitcoin transaction from `Transaction`

```haskell
putTransaction :: Transaction -> Put
```

```haskell
putTransactionToHexString :: Transaction -> String
```

Set `transactionIsSegwit` True to include `transactionMarker`, `transactionFlag` and `transactionWitness` to the output.

An example:

```haskell
import Data.ByteString.Base16.Lazy (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get
import Control.Applicative ((<|>))
import ParserUtil
import Transaction

main :: IO ()
main = print $ putTransactionToHexString $ Transaction {
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
```

## Features

### implemented
- Parse a Bitcoin transaction
- Serialize a Bitcoin transaction

### not implemented
- Blocks
- Verification
- Signing
- Simulate a Bitcoin system monadically
- 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> ```0x592eF244F8924be9F3F8803f8d639392f465Ac51```
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



