import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Data.Word
import Data.List

type FDomain = [String]
type SDomain = [String]
data Domain = FD FDomain | SD SDomain deriving (Show,Ord,Eq)
type IP = (Word8,Word8,Word8,Word8)
data Record = A Domain IP | NS Domain Domain | CNAME Domain Domain | PTR Domain Domain | SOA FDomain String deriving (Show,Ord,Eq)


-- | Filter records matching domain in A record type
resolveA :: FDomain -> [Record] -> [Record]
resolveA dom recs = do
  let (SOA sdom _) = head recs
  filter (f sdom dom) (tail recs)
  where
    f soadom dom (A (FD fdom) _) = fdom == dom
    f soadom dom (A (SD sdom) _) = concat (sdom ++ soadom) == concat dom
    f _ _ _ = False

-- | Filter records matching domain in PTR record type
resolvePTR :: FDomain -> [Record] -> [Record]
resolvePTR dom recs = do
  let (SOA sdom _) = head recs
  filter (f sdom dom) (tail recs)
  where
    f soadom dom (PTR (FD fdom) _) = fdom == dom
    f soadom dom (PTR (SD sdom) _) = concat (sdom ++ soadom) == concat dom
    f _ _ _ = False

-- | Filter records matching domain in A record type
resolveCNAME:: FDomain -> [Record] -> [FDomain]
resolveCNAME dom recs = do
  let (SOA soadom _) = head recs
  map (g soadom) $ filter (f soadom dom) (tail recs)
  where
    f soadom dom (CNAME (FD fdom) _) = fdom == dom
    f soadom dom (CNAME (SD sdom) _) = concat (sdom ++ soadom) == concat dom
    f _ _ _ = False
    g soadom (CNAME _ (FD fdom)) = fdom
    g soadom (CNAME _ (SD sdom)) = sdom ++ soadom

-- | Filter the NS record. Depending on the SOA record it will try to
-- figure out the correct NS. So if SOA is say for ac.in, then
-- www.iitk.ac.in will try to find NS for iitk.ac.in.
resolveNS :: FDomain -> [Record] -> [FDomain]
resolveNS dom recs = do
  let (SOA soadom _) = head recs
  sortBy len $ map (g soadom ) $ filter (f soadom dom) (tail recs)
  where
    f soadom dom (NS (FD fdom) _) = isSuffixOf fdom dom
    f soadom dom (NS (SD sdom) _) = isSuffixOf (concat (sdom ++ soadom)) $ concat dom
    f _ _ _ = False
    g soadom (NS _ (FD fdom)) = fdom
    g soadom (NS _ (SD sdom)) = sdom ++ soadom
    len a b  = compare (length a) (length b)


root :: IP
root = (172,31,1,1)

resolve :: IP -> FDomain -> IO [Record]
resolve ip@(ip1,ip2,ip3,ip4) fd = do
  content <- readFile (show ip1 ++ "." ++ show ip2 ++ "." ++ show ip3 ++ "." ++ show ip4)
  putStrLn $ "Looking for " ++ intercalate "." fd ++ " in " ++ show ip
  case parse parseZoneFile "" content of
    Right cont -> do
      case resolveA fd cont of
        [] -> case resolveCNAME fd cont of
          [] -> case resolveNS fd cont of
            [] -> putStrLn "No records Found" >> return []
            (dom:_) -> do
              putStrLn $ "Looking for nameserver " ++ intercalate "." dom
              rNS <- if inSameZone dom (head cont) then  resolve ip dom else resolve root dom
              case rNS of
                [] -> putStrLn "No records Found" >> return []
                ((A _ iNS):_) -> resolve iNS fd
          cnames -> do
            w <- mapM (resolve root) cnames
            return $ concat w
        recs -> do
          putStrLn $ show recs
          return recs
    Left _ -> do
      putStrLn $ "Parse Error in zonefile for " ++ show ip
      return []

resolveReverse :: IP -> FDomain -> IO [Record]
resolveReverse ip@(ip1,ip2,ip3,ip4) fd = do
  content <- readFile (show ip1 ++ "." ++ show ip2 ++ "." ++ show ip3 ++ "." ++ show ip4)
  putStrLn $ "Looking for " ++ intercalate "." fd ++ " in " ++ show ip
  case parse parseZoneFile "" content of
    Right cont -> do
      case resolvePTR fd cont of
        [] -> case resolveNS fd cont of
            [] -> putStrLn "No records Found" >> return []
            (dom:_) -> do
              putStrLn $ "Looking for nameserver " ++ intercalate "." dom
              rNS <- if inSameZone dom (head cont) then  resolve ip dom else resolve root dom
              case rNS of
                [] -> putStrLn "No records Found" >> return []
                ((A _ iNS):_) -> resolveReverse iNS fd
        recs -> do
          putStrLn $ show recs
          return recs
    Left _ -> do
      putStrLn $ "Parse Error in zonefile for " ++ show ip
      return []

inSameZone :: FDomain -> Record -> Bool
inSameZone fd (SOA dom _) = isSuffixOf dom fd

-- | Change in standard from original zone file. Here we assume that SOA
-- is only one line to allow easy parsing
pSOA :: GenParser Char () Record
pSOA = do
  char '@'
  spaces
  string "SOA"
  spaces
  domain <- pFDomain
  spaces
  str <- many (noneOf "\n")
  return $ SOA domain str

-- | Parses A record
pA :: GenParser Char () Record
pA = do
  dom <- pDomain
  spaces
  char 'A'
  spaces
  ip <- pIP
  spaces
  return $ A dom ip

-- | Parses NS record
pNS :: GenParser Char () Record
pNS = do
  dom1 <- pDomain
  spaces
  string "NS"
  spaces
  dom2 <- pDomain
  spaces
  return (NS dom1 dom2)

-- | parses a CNAME record
pCNAME :: GenParser Char () Record
pCNAME = do
  dom1 <- pDomain
  spaces
  string  "CNAME"
  spaces
  dom2 <- pDomain
  spaces
  return (CNAME dom1 dom2)

-- | parses a PTR record
pPTR :: GenParser Char () Record
pPTR = do
  dom1 <- pDomain
  spaces
  string  "PTR"
  spaces
  dom2 <- pDomain
  spaces
  return (PTR dom1 dom2)

-- | Parses an IP. Strange things happen when IP is not valid. ie if
-- you want to parse 172.31.1.999, then 999 will be rounded off to a
-- suitable word8 and will not give any error while parsing
pIP :: GenParser Char () IP
pIP = do
  i1 <- many1 digit
  char '.'
  i2 <- many1 digit
  char '.'
  i3 <- many1 digit
  char '.'
  i4 <- many1 digit
  return (read i1,read i2,read i3,read i4)

-- | Parses a domain which is like www.google.com. , It represents the
-- complete domain name.
pFDomain :: GenParser Char () FDomain
pFDomain = do
  first <- many (alphaNum <|> char '-')
  char '.'
  rest <- option [] pFDomain
  return (first:rest)

-- | Parses a partial domain name like ns1 . It represents the domain
-- name with respect to domain of the SOA record.
pSDomain :: GenParser Char () SDomain
pSDomain = do
  first <- many (alphaNum <|> char '-')
  rest <- option [] $ do
    char '.'
    pSDomain
  return (first:rest)

-- | Parses Domain.
pDomain :: GenParser Char () Domain
pDomain = do
  try ( do
    dom <- pFDomain
    return $ FD dom)
   <|> (
    do
      dom <- pSDomain
      return $ SD dom)


parseZoneFile :: GenParser Char () [Record]
parseZoneFile = do
  soa <- pSOA
  newline
  rest <- parseRecords
  return (soa:rest)

parseRecords :: GenParser Char () [Record]
parseRecords = do
  out <- try pA <|> try pNS <|> try pCNAME <|> pPTR
  rest <- option [] parseRecords
  return (out:rest)
