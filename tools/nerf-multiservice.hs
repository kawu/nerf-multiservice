{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import qualified Data.Binary as Binary
import qualified System.IO as IO
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

import Debug.Trace (trace)

---------------------------
-- Interface implementation
---------------------------

-- | Implementation of the annotating service.
instance Iface.AnnotatingService_Iface Nerf.Nerf where
    annotate nerf (Just ttext) _ = return $
        let annParv = V.fromList . map (annPar nerf) . V.toList
        in  ttext { TT.f_TText_paragraphs =
                fmap annParv (TT.f_TText_paragraphs ttext) }

instance Tok.Word (TT.TToken) where
    word tok = maybe "" Tok.word (TT.f_TToken_orth tok)

-- | Annotate paragraph.
annPar :: Nerf.Nerf -> TT.TParagraph -> TT.TParagraph
annPar nerf tpar =
    let annSens = V.fromList . map (annSent nerf) . V.toList
    in  tpar { TT.f_TParagraph_sentences =
            fmap annSens (TT.f_TParagraph_sentences tpar) }

-- | Annotate sentence.
annSent :: Nerf.Nerf -> TT.TSentence -> TT.TSentence
annSent nerf tsent = case V.toList <$> TT.f_TSentence_tokens tsent of
    Nothing -> tsent
    Just ts -> 
        let neForest  = Nerf.ner nerf (restoreOrigSent ts)
            neForest' = Tok.moveNEs neForest ts
            sentID    = maybe "#" id (TT.f_TSentence_id tsent)
        in  tsent { TT.f_TSentence_names =
                (Just . V.fromList) (identify sentID neForest') }

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
identify :: ID -> Named.NeForest Nerf.NE TT.TToken -> [TT.TNamedEntity]
identify sentID ft = snd $ RWS.evalRWS (mapM_ identifyTree ft) sentID 0

-- | Identifier.
type ID = L.Text

-- | Named entity monad with:
-- * Sentence ID,
-- * List of NEs,
-- * Number of NEs on the list.
-- the list of named entities and the current number of assigned identifiers.
type NEM = RWS.RWS ID [TT.TNamedEntity] Int

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
identifyTree (Named.Node (Right tok) _) = return $
    case TT.f_TToken_id tok of
        Just ix -> ix
        -- This should never happen, but...
        Nothing ->
            let msg = "Token with no ID:\n" ++ show tok
            in  trace msg ""
    
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
        <> value 22023 )

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
