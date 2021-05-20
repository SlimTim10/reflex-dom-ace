{-|

Basic support for using the Ace editor with Reflex.

-}

module Reflex.Dom.Ace where

import Control.Lens ((^.))
import Control.Monad (unless, void, join)
import Data.Default (Default)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Monad.Trans as Trans
import qualified Data.Bifunctor as Bi

import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle.Object ((<#))
import qualified Reflex.Dom.Core as R


data AceTheme
  = AceTheme_Chrome
  | AceTheme_Clouds
  | AceTheme_CrimsonEditor
  | AceTheme_Dawn
  | AceTheme_Dreamweaver
  | AceTheme_Eclipse
  | AceTheme_Github
  | AceTheme_Iplastic
  | AceTheme_SolarizedLight
  | AceTheme_Textmate
  | AceTheme_Tomorrow
  | AceTheme_Xcode
  | AceTheme_Kuroir
  | AceTheme_Katzenmilch
  | AceTheme_Sqlserver
  | AceTheme_Ambiance
  | AceTheme_Chaos
  | AceTheme_CloudsMidnight
  | AceTheme_Cobalt
  | AceTheme_Gruvbox
  | AceTheme_IdleFingers
  | AceTheme_KrTheme
  | AceTheme_Merbivore
  | AceTheme_MerbivoreSoft
  | AceTheme_MonoIndustrial
  | AceTheme_Monokai
  | AceTheme_PastelOnDark
  | AceTheme_SolarizedDark
  | AceTheme_Terminal
  | AceTheme_TomorrowNight
  | AceTheme_TomorrowNightBlue
  | AceTheme_TomorrowNightBright
  | AceTheme_TomorrowNightEighties
  | AceTheme_Twilight
  | AceTheme_VibrantInk
  deriving (Eq,Ord,Enum,Bounded)

instance Show AceTheme where
    show AceTheme_Ambiance              = "ambiance"
    show AceTheme_Chaos                 = "chaos"
    show AceTheme_Chrome                = "chrome"
    show AceTheme_Clouds                = "clouds"
    show AceTheme_CloudsMidnight        = "clouds_midnight"
    show AceTheme_Cobalt                = "cobalt"
    show AceTheme_CrimsonEditor         = "crimson_editor"
    show AceTheme_Dawn                  = "dawn"
    show AceTheme_Dreamweaver           = "dreamweaver"
    show AceTheme_Eclipse               = "eclipse"
    show AceTheme_Github                = "github"
    show AceTheme_Gruvbox               = "gruvbox"
    show AceTheme_IdleFingers           = "idle_fingers"
    show AceTheme_Iplastic              = "iplastic"
    show AceTheme_Katzenmilch           = "katzenmilch"
    show AceTheme_KrTheme               = "kr_theme"
    show AceTheme_Kuroir                = "kuroir"
    show AceTheme_Merbivore             = "merbivore"
    show AceTheme_MerbivoreSoft         = "merbivore_soft"
    show AceTheme_MonoIndustrial        = "mono_industrial"
    show AceTheme_Monokai               = "monokai"
    show AceTheme_PastelOnDark          = "pastel_on_dark"
    show AceTheme_SolarizedDark         = "solarized_dark"
    show AceTheme_SolarizedLight        = "solarized_light"
    show AceTheme_Sqlserver             = "sqlserver"
    show AceTheme_Terminal              = "terminal"
    show AceTheme_Textmate              = "textmate"
    show AceTheme_Tomorrow              = "tomorrow"
    show AceTheme_TomorrowNight         = "tomorrow_night"
    show AceTheme_TomorrowNightBlue     = "tomorrow_night_blue"
    show AceTheme_TomorrowNightBright   = "tomorrow_night_bright"
    show AceTheme_TomorrowNightEighties = "tomorrow_night_eighties"
    show AceTheme_Twilight              = "twilight"
    show AceTheme_VibrantInk            = "vibrant_ink"
    show AceTheme_Xcode                 = "xcode"


data AceConfig = AceConfig
    { _aceConfigElemAttrs       :: Map Text Text
    , _aceConfigBasePath        :: Maybe Text
    , _aceConfigMode            :: Maybe Text
    , _aceConfigWordWrap        :: Bool
    , _aceConfigShowPrintMargin :: Bool
    }


data AceDynConfig = AceDynConfig
    { _aceDynConfigTheme :: Maybe AceTheme
    }


instance Default AceConfig where
    def = AceConfig R.def R.def R.def False False


newtype AceInstance = AceInstance { unAceInstance :: JS.JSVal }


data Ace t = Ace
    { aceRef   :: R.Dynamic t (Maybe AceInstance)
    , aceValue :: R.Dynamic t Text
    }


------------------------------------------------------------------------------
-- The type of editor session line annotation.
data AnnotationType = AnnotationError
                    | AnnotationWarning
                    deriving (Show, Read)

------------------------------------------------------------------------------
instance JS.ToJSVal AnnotationType where
  toJSVal AnnotationError   = JS.toJSVal ("error":: Text)
  toJSVal AnnotationWarning = JS.toJSVal ("warning" :: Text)


------------------------------------------------------------------------------
-- A line annotation for marking a specific line within the editor session as
-- an error or a warning.
data Annotation = Annotation { annotationRow    :: Int
                             , annotationColumn :: Int
                             , annotationText   :: Text
                             , annotationType   :: AnnotationType
                             } deriving (Show, Read)


------------------------------------------------------------------------------
instance JS.MakeObject Annotation where
  makeObject (Annotation row col txt typ) = do
    o <- JS.create
    (o <# ("row" :: Text)   ) row
    (o <# ("column" :: Text)) col
    (o <# ("text" :: Text)  ) txt
    (o <# ("type" :: Text)  ) typ
    return o


instance JS.ToJSVal Annotation where
  toJSVal = (JS.toJSVal =<<) . JS.makeObject


------------------------------------------------------------------------------
startAce :: JS.MonadJSM m => Text -> AceConfig -> m AceInstance
startAce containerId ac = JS.liftJSM $ do
  aceVal <- JS.jsg ("ace" :: Text)
  let
    (basePath, mode) = join Bi.bimap (fromMaybe (T.pack "") . ($ ac))
      (_aceConfigBasePath, _aceConfigMode)
  -- Set the base path if given
  unless (T.null basePath) $ do
    config <- aceVal ^. JS.js ("config" :: Text)
    void $ config ^. JS.js2 ("set" :: Text) ("basePath" :: Text) basePath
  -- Start and return an editing session
  editor <- aceVal ^. JS.js1 ("edit" :: Text) containerId
  let aceInst = AceInstance editor
  -- Set the mode if given
  unless (T.null mode) $ do
    setModeAce mode aceInst
  setUseWrapMode (_aceConfigWordWrap ac) aceInst
  setShowPrintMargin (_aceConfigShowPrintMargin ac) aceInst
  return aceInst


------------------------------------------------------------------------------
moveCursorToPosition :: JS.MonadJSM m => (Int, Int) -> AceInstance -> m ()
moveCursorToPosition (r, c) (AceInstance ace) =
  JS.liftJSM $ void $ ace ^. JS.js2 ("gotoLine" :: Text) r c


------------------------------------------------------------------------------
setThemeAce :: JS.MonadJSM m => Maybe AceTheme -> AceInstance -> m ()
setThemeAce Nothing      _                 = return ()
setThemeAce (Just theme) (AceInstance ace) =
  JS.liftJSM $ void $ ace ^. JS.js1 ("setTheme" :: Text) themeStr
  where themeStr = "ace/theme/" <> show theme


------------------------------------------------------------------------------
setModeAce :: JS.MonadJSM m => Text -> AceInstance -> m ()
setModeAce mode (AceInstance ace) = JS.liftJSM $ do
  session <- ace ^. JS.js ("session" :: Text)
  void $ session ^. JS.js1 ("setMode" :: Text) modeStr
  where modeStr = "ace/mode/" <> mode


------------------------------------------------------------------------------
setUseWrapMode :: JS.MonadJSM m => Bool -> AceInstance -> m ()
setUseWrapMode shouldWrap (AceInstance ace) = JS.liftJSM $ do
  session <- ace ^. JS.js0 ("getSession" :: Text)
  void $ session ^. JS.js1 ("setUseWrapMode" :: Text) shouldWrap


------------------------------------------------------------------------------
setShowPrintMargin :: JS.MonadJSM m => Bool -> AceInstance -> m ()
setShowPrintMargin shouldShow (AceInstance ace) =
  JS.liftJSM $ void $ ace ^. JS.js2 ("setOption" :: Text) ("showPrintMargin" :: Text) shouldShow


------------------------------------------------------------------------------
setUseWorker :: JS.MonadJSM m => Bool -> AceInstance -> m ()
setUseWorker shouldUse (AceInstance ace) =
  JS.liftJSM $ void $ ace ^. JS.js2 ("setOption" :: Text) ("useWorker" :: Text) shouldUse


------------------------------------------------------------------------------
setAnnotations :: JS.MonadJSM m => [Annotation] -> AceInstance -> m ()
setAnnotations as (AceInstance ace) = JS.liftJSM $ do
  session <- ace ^. JS.js0 ("getSession" :: Text)
  annotations <- JS.toJSValListOf as
  void $ session ^. JS.js1 ("setAnnotations" :: Text) annotations


------------------------------------------------------------------------------
setConfigAce :: JS.MonadJSM m => Text -> Text -> AceInstance -> m ()
setConfigAce t1 t2 (AceInstance ace) = JS.liftJSM $ do
  cfg <- ace ^. JS.js ("config" :: Text)
  void $ cfg ^. JS.js2 ("set" :: Text) t1 t2


------------------------------------------------------------------------------
getValueAce :: JS.MonadJSM m => AceInstance -> m Text
getValueAce (AceInstance ace) =
  JS.liftJSM $ ace ^. JS.js0 ("getValue" :: Text) >>= JS.fromJSValUnchecked


------------------------------------------------------------------------------
setValueAce :: JS.MonadJSM m => Text -> AceInstance -> m ()
setValueAce t (AceInstance ace) =
  JS.liftJSM $ void $ ace ^. JS.js2 ("setValue" :: Text) t (-1 :: Int)


------------------------------------------------------------------------------
setupValueListener
  :: ( JS.MonadJSM (R.Performable m)
     , R.DomBuilder t m
     , R.PostBuild t m
     , R.TriggerEvent t m
     , R.PerformEvent t m
     )
  => AceInstance
  -> m (R.Event t Text)
setupValueListener (AceInstance ace) = do
  pb  <- R.getPostBuild
  let act cb = JS.liftJSM $ do
        jscb <- JS.asyncFunction $ \_ _ _ ->
          getValueAce (AceInstance ace) >>= Trans.liftIO . cb
        void $ ace ^. JS.js2 ("on" :: Text) ("change" :: Text) jscb
  R.performEventAsync (act <$ pb)


------------------------------------------------------------------------------
-- | Main entry point
--
-- IMPORTANT NOTE:
--
-- This currently does not work if your app is using reflex-dom's
-- mainWidgetWithHead or mainWidgetWithCss.
aceWidget
    :: ( R.DomBuilder t m
       , R.PostBuild t m
       , R.MonadHold t m
       , JS.MonadJSM m
       , R.TriggerEvent t m
       , R.PerformEvent t m
       , JS.MonadJSM (R.Performable m)
       )
    => AceConfig -- ^ Ace editor configurations
    -> AceDynConfig -- ^ Ace editor theme
    -> R.Event t AceDynConfig -- ^ Updatable Ace editor theme
    -> Text -- ^ ID of desired container element
    -> Text -- ^ Initial Ace editor contents
    -> R.Event t Text -- ^ Updatable Ace editor contents
    -> m (Ace t)
aceWidget ac adc adcUps containerId initContents contentsUps = do
    aceInstance <- startAce containerId ac
    onChange <- setupValueListener aceInstance
    updatesDyn <- R.holdDyn initContents onChange

    let ace = Ace (R.constDyn $ pure aceInstance) updatesDyn
    setThemeAce (_aceDynConfigTheme adc) aceInstance
    void $ withAceInstance ace (setThemeAce . _aceDynConfigTheme <$> adcUps)
    R.performEvent_ $ R.ffor contentsUps $ \c -> setValueAce c aceInstance
    return ace


------------------------------------------------------------------------------
-- | Convenient helper function for running functions that need an AceInstance.
withAceInstance
    :: R.PerformEvent t m
    => Ace t
    -> R.Event t (AceInstance -> R.Performable m ())
    -> m (R.Event t ())
withAceInstance ace evt = withAceInstance' ace (f <$> evt)
  where
    f _ Nothing  = return ()
    f g (Just a) = g a


------------------------------------------------------------------------------
-- | More powerful function for running functions that need an AceInstance.
withAceInstance'
    :: R.PerformEvent t m
    => Ace t
    -> R.Event t (Maybe AceInstance -> R.Performable m a)
    -> m (R.Event t a)
withAceInstance' ace =
  R.performEvent . R.attachPromptlyDynWith (flip ($)) (aceRef ace)
