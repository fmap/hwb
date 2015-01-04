module Graphics.UI.Gtk.WebKit.WebSettings.Concrete (
  webSettingsEnableScripts,
  webSettingsEnablePrivateBrowsing
) where

import Graphics.UI.Gtk.WebKit.WebSettings (WebSettings)
import System.Glib.Attributes (Attr)
import qualified Graphics.UI.Gtk.WebKit.WebSettings as Attribute (webSettingsEnablePrivateBrowsing, webSettingsEnableScripts)

webSettingsEnableScripts :: Attr WebSettings Bool
webSettingsEnableScripts = Attribute.webSettingsEnableScripts

webSettingsEnablePrivateBrowsing :: Attr WebSettings Bool
webSettingsEnablePrivateBrowsing = Attribute.webSettingsEnablePrivateBrowsing

