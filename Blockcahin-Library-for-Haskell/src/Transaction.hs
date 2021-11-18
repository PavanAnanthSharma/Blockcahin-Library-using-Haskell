module Transaction (
  Txin(..),
  Txout(..),
  Witness(..),
  Transaction(..),
  getTransaction,
  getTxin,
  getTxout, 
  getWitness,
  getTransactionFromHexString,
  putTransaction,
  putTxin,
  putTxout, 
  putWitness,
  putTransactionToHexString
  ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Base16.Lazy
import Data.List
import Data.Maybe (fromJust)
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import GHC.Word
import Control.Applicative ((<|>), Alternative, optional)
import Control.Monad (replicateM, when, unless, guard)
import ParserUtil

-----------------------------------------
-- DEFINITION OF TRANSACTION DATA TYPE --
-----------------------------------------

data Txin = Txin {
  txinHash       :: B.ByteString,  -- | hash of the to-be-used transaction
  txinIndex      :: Int,           -- | index of the to-be-used transaction
  txinScript     :: B.ByteString,  -- | unlock script
  txinSequenceNo :: Int            -- | sequence no
} deriving Eq

data Txout = Txout {
  txoutAmount :: Int,              -- | amount of btc to use
  txoutScript :: B.ByteString      -- | lock script
} deriving Eq

-- | A witness field is an array of stack items.
-- | A witness consists of an array of witness fields, and each of them is associated with a txin.
-- | Since a witness is not a script, serialization of stack items will be done out of script layer.
-- | This is why Witness has a two dimentional array of ByteString.
data Witness = Witness {
  witnessFields :: [[B.ByteString]]
} deriving Eq

data Transaction = Transaction {
  transactionIsSegwit :: Bool,          -- | whether segwit or not
  transactionVersion  :: Int,           -- | version of the transaction
  transactionMarker   :: Maybe Int,     -- | transaction marker (if segwit)
  transactionFlag     :: Maybe Int,     -- | transactio flag (if segwit)
  transactionTxins    :: [Txin],        -- | list of unlock scripts
  transactionTxouts   :: [Txout],       -- | list of lock scripts
  transactionWitness  :: Maybe Witness, -- | transaction witness (if segwit)
  transactionLocktime :: Int            -- | locktime of the transaction
} deriving Eq

listLines :: (a -> [String]) -> [a] -> [String]
listLines f l =
  concatMap
    (\(i, x) ->
      (show i ++ ": ") : (indent 2) (f x)
    ) (zip (take (length l) [0..]) l)

txinLines :: Txin -> [String]
txinLines (Txin h i s n) = [
  "Txin [",
  "  hash: "        ++ (show . encode) h,
  "  index: "       ++ show i,
  "  script: "      ++ (show . encode) s,
  "  sequence_no: " ++ show n,
  "]"]

txoutLines :: Txout -> [String]
txoutLines (Txout a s) = [
  "Txout [",
  "  amount: " ++ show a,
  "  script: " ++ (show . encode) s,
  "]"]

witnessLines :: Witness -> [String]
witnessLines (Witness f) =
  "Witness [" :
  "  fields:" : (indent 4) (listFields f) ++ [
  "]"]

transactionLines :: Transaction -> [String]
transactionLines (Transaction True v m f i o w l) = [
  "Transaction [",
  "  version: " ++ show v,
  "  marker: " ++ (show . fromJust) m,
  "  flag: " ++ (show . fromJust) f ] ++
  "  txins: " : (indent 4 . listLines txinLines) i ++
  "  txouts: " : (indent 4 . listLines txoutLines) o ++
  "  witness: " : (indent 4 . witnessLines) (fromJust w) ++ [
  "  locktime:  " ++ show l,
  "]" ]

transactionLines (Transaction False v _ _ i o _ l) = [
  "Transaction [",
  "  version: "  ++ show v ] ++
  "  txins: "  : (indent 4 . listLines txinLines) i ++
  "  txouts: " : (indent 4 . listLines txoutLines) o  ++ [
  "  locktime: " ++ show l,
  "]"]

listFields :: [[B.ByteString]] -> [String]
listFields x = ["["] ++ map (("  "++) . unwords . map (show . encode)) x ++ ["]"]

indent :: Int -> [String] -> [String]
indent n = map (replicate n ' ' ++)

join :: [String] -> String
join = intercalate "\n"

instance Show Txin where
  show x = join (txinLines x)

instance Show Txout where
  show x = join (txoutLines x)

instance Show Witness where
  show x = join (witnessLines x)

instance Show Transaction where
  show x = join (transactionLines x)

-------------------------
-- PARSING TRANSACTION --
-------------------------

-- | Parse a raw transaction.
-- | First argument represents wheather use Segwit-style transaction format (BIP 144) or not.
getTransaction :: Get Transaction
getTransaction = do
  version <- cast getWord32le

  isSegwit <- (==0) <$> lookAhead getWord8

  -- If segwit
  marker <- opt isSegwit (cast getWord8)
  flag   <- opt isSegwit (cast getWord8)

  -- Run getTxin <numTxins> times
  numTxins <- getVI
  txins <- replicateM numTxins getTxin

  -- Run getTxout <numTxouts> times
  numTxouts <- getVI
  txouts <- replicateM numTxouts getTxout

  -- Parse witness if segwit.
  witness <- opt isSegwit (getWitness numTxins)

  locktime <- cast getWord32le

  flip unless (fail "get failed") =<< isEmpty

  return $ Transaction isSegwit version marker flag txins txouts witness locktime

getTransactionFromHexString :: String -> Transaction
getTransactionFromHexString = runGet getTransaction . decodeHex

getTxin :: Get Txin
getTxin = do
  hash   <- getLazyByteString 32
  index  <- cast getWord32le
  script <- getLazyByteString =<< getVI
  seqno  <- cast getWord32le
  
  return $ Txin hash index script seqno

getTxout :: Get Txout
getTxout = do
  amount <- cast getWord64le
  script <- getLazyByteString =<< getVI

  return $ Txout amount script

-- | get a witness whose number of fields is given int
getWitness :: Int -> Get Witness
getWitness numFields =
  fmap Witness $
    replicateM numFields $ do
      numItems <- getVI
      replicateM numItems $ 
        getLazyByteString =<< getVI

----------------------------
-- GENERATING TRANSACTION --
----------------------------

putTransaction :: Transaction -> Put
putTransaction (Transaction isSegwit version marker flag txins txouts witness locktime) = do
  putWord32le (fromIntegral version)

  -- If segwit
  opt isSegwit (putWord8 $ fromIntegral $ fromJust $ marker)
  opt isSegwit (putWord8 $ fromIntegral $ fromJust $ flag)

  -- Run putTxin for every txins
  putVI (length txins)
  mapM putTxin txins

  -- Run putTxout for every txouts
  putVI (length txouts)
  mapM putTxout txouts

  -- Parse witness if segwit.
  opt isSegwit (putWitness $ fromJust $ witness)

  putWord32le $ fromIntegral locktime

  return ()

putTxin :: Txin -> Put
putTxin (Txin hash index script seqno) = do
  putLazyByteString hash
  putWord32le (fromIntegral index)
  putVI (B.length script)
  putLazyByteString script
  putWord32le (fromIntegral seqno)

putTxout :: Txout -> Put
putTxout (Txout amount script) = do
  putWord64le (fromIntegral amount)
  putVI (B.length script)
  putLazyByteString script

putWitness :: Witness -> Put
putWitness (Witness fields) = do
  mapM_ (\field -> do
    putVI (length field)
    mapM_ (\item -> do
        putVI (B.length item)
        putLazyByteString item
      ) field) fields

putTransactionToHexString :: Transaction -> String
putTransactionToHexString = encodeHex . snd . runPutM . putTransaction

----------------------------
-- CONVERTING TRANSACTION --
----------------------------

toUnsignedTransaction :: Transaction -> Transaction
toUnsignedTransaction (Transaction isSegwit version marker flag txins txouts witness locktime) =
  Transaction
    isSegwit
    version
    marker
    flag
    (map (\(Txin hash index script seqno) -> Txin hash index mempty seqno) txins) -- nullify all scripts
    txouts
    (Witness [] <$ witness) -- nullify witness
    locktime
