# reflex-dom-ace

## Overview

Basic support for using the Ace editor with Reflex.

This package provides a Reflex wrapper around the Ace editor.  It is somewhat incomplete and was derived from [code written for hsnippet](https://github.com/mightybyte/hsnippet/blob/64cc17d2bf2bcce219f3ab8e96b7fd6071d5b56b/frontend/src/ACE.hs).

This is also intended to serve as an example of how to structure FFI packages that rely on external JS packages.

## Example Usage with Obelisk

### Add Dependency to Obelisk

#### Hackage

As per the [Obelisk FAQ](https://github.com/obsidiansystems/obelisk/blob/master/FAQ.md#how-do-i-declare-a-new-haskell-dependency), modify `default.nix` to look like:

```
# ...
project ./. ({ pkgs, ... }: {
# ...
  overrides = self: super:
  {
    reflex-dom-ace = self.callHackageDirect {
      pkg = "reflex-dom-ace";
      ver = "0.3.0.1";
      sha256 = "0kbd3kqmsx4115a39a984m62kgc9s96586c5yx80nijman8j0zlw";
    } {};
  };
# ...
```

Change the `ver` to match the desired version and leave the `sha256`. Then let `ob run` fail with the expected `sha256` and update the value accordingly.

NOTE: It may be necessary to run `nix-collect-garbage` to clear the cache before `ob run`.

#### GitHub

As per the [Obelisk FAQ](https://github.com/obsidiansystems/obelisk/blob/master/FAQ.md#how-do-i-declare-a-new-haskell-dependency), modify `default.nix` to look like:

```
# ...
project ./. ({ pkgs, ... }: {
# ...
  overrides = self: super: let
    aceSrc = pkgs.fetchFromGitHub {
      owner = "SlimTim10";
      repo = "reflex-dom-ace";
      rev = "5d086c871892b9ebf7c66ad2d4d4f84023bea9b2";
      sha256 = "0vizlf691s1rbkilhf23c79vkb6pns2x4b6b94szn9wd8mjizxyc";
    };
  in
  {
    reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" aceSrc {};
  };
# ...
```

Change the `rev` to match the latest commit hash and leave the `sha256`. Then let `ob run` fail with the expected `sha256` and update the value accordingly.

NOTE: It may be necessary to run `nix-collect-garbage` to clear the cache before `ob run`.

### Get Ace Editor

Get the `src-noconflict` package from [ace-builds](https://github.com/ajaxorg/ace-builds) (tested with [package version 06.7.20](https://github.com/ajaxorg/ace-builds/tree/53be42342df216d2d25dc60a12dfcb263c6f0592/src-noconflict)). Move the `src-noconflict` contents to the project directory `static/ace/`.

E.g.,

```
$ cd ~
$ git clone https://github.com/ajaxorg/ace-builds
$ mv ~/ace-builds/src-noconflict ~/my-obelisk-project/static/ace
```

### Editor.hs

```
module Editor
  ( widget
  ) where

import Data.Text (Text)
import Control.Monad (void)
import Data.Functor ((<&>))

import qualified Language.Javascript.JSaddle.Types as JS
import qualified Reflex.Dom.Ace as Ace
import qualified Reflex.Dom.Core as R
import Reflex.Dom.Core ((=:))

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.TriggerEvent t m
     , JS.MonadJSM (R.Performable m)
     , JS.MonadJSM m
     , R.PerformEvent t m
     , R.PostBuild t m
     , R.MonadHold t m
     )
  => m (R.Dynamic t Text)
widget = do
  let containerId = "editor"
  void $ R.elAttr "div" (
    "id" =: containerId
    ) R.blank
  (script, _) <- R.elAttr' "script" (
    "src" =: "/static/ace/ace.js"
    <> "type" =: "text/javascript"
    <> "charset" =: "utf-8"
    ) R.blank
  let scriptLoaded = () <$ R.domEvent R.Load script
  let loading = R.el "p" $ R.text "Loading editor..." <&> const (R.constDyn "")
  dt :: R.Dynamic t (R.Dynamic t Text) <- R.widgetHold loading
    $ R.ffor scriptLoaded
    $ const $ do
      ace <- do
        let
          cfg = R.def
            { Ace._aceConfigMode = Just "latex"
            }
        Ace.aceWidget cfg (Ace.AceDynConfig (Just Ace.AceTheme_Clouds)) R.never containerId "" R.never
      return $ Ace.aceValue ace
  R.holdDyn "" . R.switchDyn $ R.updated <$> dt
```

### Frontend.hs

```
module Frontend where

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import qualified Editor

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      prerender_ blank $ do
        editorContents :: Dynamic t Text <- Editor.widget
        -- Do something with editorContents
        return ()
  }
```

## Important Notes

This currently does not work if your app is using reflex-dom's
mainWidgetWithHead or mainWidgetWithCss.