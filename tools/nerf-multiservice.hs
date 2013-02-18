{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import System.CPUTime (getCPUTime)
import qualified Data.Binary as Binary
import qualified System.IO as IO
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Control.Monad.RWS.Strict as RWS

-- Thrift library
import Thrift.Server

-- Thrift generated modules
import qualified Types_Types as TT
import qualified AnnotatingService as Ann
import qualified AnnotatingService_Iface as Iface

import qualified Data.Named.Tree as Named
import qualified NLP.Nerf as Nerf
import qualified NLP.Nerf.Tokenize as Tok

---------------------------
-- Interface implementation
---------------------------

-- | Implementation of the annotating service.
instance Iface.AnnotatingService_Iface Nerf.Nerf where
    annotate nerf (Just ttext) _ =
        case V.toList <$> TT.f_TText_paragraphs ttext of
            Nothing -> return ttext
            Just ps -> do
                beg <- getCPUTime
                ps' <- V.fromList <$> mapM (annPar nerf) ps
                end <- getCPUTime
                let coef = 10 ^ (9 :: Integer)
                let diff = end - beg `div` coef
                return $ ttext
                    { TT.f_TText_paragraphs = Just ps'
                    , TT.f_TText_annotationHeaders = Just $
                        addHeader diff (getHeaders ttext) }
      where
        getHeaders = maybe H.empty id . TT.f_TText_annotationHeaders

instance Tok.Word (TT.TToken) where
    word tok = maybe "" Tok.word (TT.f_TToken_orth tok)

-- | Add appropriate header.
addHeader
    :: Integer
    -> H.HashMap TT.TAnnotationLayer TT.THeader
    -> H.HashMap TT.TAnnotationLayer TT.THeader
addHeader procTime = H.insert TT.NAMES $ TT.THeader
    { f_THeader_id = Just ""
    , f_THeader_title = Just ""
    , f_THeader_distributor = Just "Nerf"
    , f_THeader_publicationTime = Just 0
    , f_THeader_processingDuration = Just (fromIntegral procTime)
    , f_THeader_sourceDescText = Just ""
    , f_THeader_retrievedFrom = Just "" }

-- | Annotate paragraph.
annPar :: Nerf.Nerf -> TT.TParagraph -> IO TT.TParagraph
annPar nerf tpar = case V.toList <$> TT.f_TParagraph_sentences tpar of
    Nothing -> return tpar
    Just tp -> do
        sentences <- V.fromList <$> mapM (annSent nerf) tp
        return $ tpar {TT.f_TParagraph_sentences = Just sentences}

-- | Annotate sentence.
annSent :: Nerf.Nerf -> TT.TSentence -> IO TT.TSentence
annSent nerf tsent = case V.toList <$> TT.f_TSentence_tokens tsent of
    Nothing -> return tsent
    Just ts -> do
        putStrLn "# Sentence:"
        putStrLn origSent
        putStrLn "# Forest:"
        drawFst neForest
        names <- V.fromList <$> identify sentID neForest'
        return $ tsent {TT.f_TSentence_names = Just names}
      where
        drawFst   = putStrLn . Named.drawForest . Named.mapForest show
        origSent  = restoreOrigSent ts
        neForest  = Nerf.ner nerf origSent
        neForest' = Tok.moveNEs neForest ts
        sentID    = maybe "#" id (TT.f_TSentence_id tsent)

-- | Restore original sentence.
restoreOrigSent :: [TT.TToken] -> String
restoreOrigSent
    = dropWhile isSpace
    . concat
    . map tokStr
    . mapMaybe tokPair
  where
    tokPair tok = (,)
        <$> TT.f_TToken_orth tok
        <*> TT.f_TToken_noPrecedingSpace tok
    tokStr (orth, nps) = (if nps then "" else " ") ++ (L.unpack orth)

-- | Assign ID to individual NEs.
identify :: ID -> Named.NeForest Nerf.NE TT.TToken -> IO [TT.TNamedEntity]
identify sentID ft = snd <$> RWS.evalRWST (mapM_ identifyTree ft) sentID 0

-- | Identifier.
type ID = L.Text

-- | Named entity monad with:
-- * Sentence ID,
-- * List of NEs,
-- * Number of NEs on the list.
-- the list of named entities and the current number of assigned identifiers.
type NEM = RWS.RWST ID [TT.TNamedEntity] Int IO

-- | Get new identifier.
currID :: NEM ID
currID = do
    major <- RWS.ask
    minor <- L.pack . show <$> RWS.get
    let ix = L.concat ["ne.", major, ".", minor]
    return ix

-- | Record NE using the writer monad component.
record :: TT.TNamedEntity -> NEM ()
record ne = do
    n <- RWS.get
    RWS.put (n + 1)
    RWS.tell [ne]

identifyTree :: Named.NeTree Nerf.NE TT.TToken -> NEM ID
identifyTree (Named.Node (Left neType) xs) = do
    ixs <- mapM identifyTree xs
    ix  <- currID
    record $ TT.TNamedEntity
                (Just ix)
                (Just "")
                (Just "")
                (Just $ L.fromStrict neType)
                (Just "")
                (Just $ V.fromList ixs)
    return ix
identifyTree (Named.Node (Right tok) _) = case TT.f_TToken_id tok of
    Just ix -> return ix
    -- This should never happen, but...
    Nothing -> do
        let msg = "ERROR: Token with no ID:\n" ++ show tok
        RWS.lift $ IO.hPutStrLn IO.stderr msg
        return ""
    
----------------------------------
-- Command-line program definition
----------------------------------
        
data Service = Service
    { modelPath :: FilePath
    , port      :: Int }

service :: Parser Service
service = Service
    <$> argument str (metavar "MODEL")
    <*> option
         ( long "port"
        <> short 'p'
        <> help "Port number"
        <> value 10008 )

decodeModel :: FilePath -> IO Nerf.Nerf
decodeModel = Binary.decodeFile

runService :: Service -> IO ()
runService Service{..} = do
    putStr "Reading model..." >> IO.hFlush IO.stdout
    nerf <- decodeModel modelPath
    Nerf.schemaConf nerf `seq` Nerf.crf `seq` putStrLn " done"
    putStrLn "Start server"
    _ <- runBasicServer nerf Ann.process (fromIntegral port)
    putStrLn "Server stopped"

main :: IO ()
main =
    execParser opts >>= runService
  where
    opts = info (helper <*> service)
      ( fullDesc
     <> progDesc "Run multiservice Nerf component"
     <> header "nerf-multiservice" )
