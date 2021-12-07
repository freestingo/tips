{-# LANGUAGE OverloadedStrings #-}
module Theme where

import Control.Lens
import Monomer
import Monomer.Core.Themes.BaseTheme
import qualified Monomer.Lens as L

customDarkTheme :: Theme
customDarkTheme = baseTheme darkThemeColors
  { clearColor = backgroundColor
  --, sectionColor = rgbHex "#BD746F"

  , btnFocusBorder = borderColor
  , btnBgBasic = btnBgBasicColor
  , btnBgHover = btnBgHoverColor
  , btnBgFocus = btnBgFocusColor
  , btnBgActive = btnBgActiveColor
  , btnBgDisabled = btnBgDisabledColor
  , btnText = btnTextColor
  , btnTextDisabled = btnTextDisabledColor
  , btnMainFocusBorder = borderColor
  , btnMainBgBasic = btnMainBgBasicColor
  , btnMainBgHover = btnMainBgHoverColor
  , btnMainBgFocus = btnMainBgFocusColor
  , btnMainBgActive = btnMainBgActiveColor
  , btnMainBgDisabled = btnMainBgDisabledColor
  , btnMainText = btnTextColor
  , btnMainTextDisabled = btnTextDisabledColor

  --, dialogBg = gray01
  --, dialogBorder = gray01
  , dialogText = foregroundColor
  , dialogTitleText = foregroundColor
  --, emptyOverlay = gray05 & L.a .~ 0.8

  --, externalLinkBasic = blue07
  --, externalLinkHover = blue08
  --, externalLinkFocus = blue07
  --, externalLinkActive = blue06
  --, externalLinkDisabled = gray06

  --, iconBg = gray08
  --, iconFg = gray01

  --, inputIconFg = black
  , inputBorder = inputBorderColor
  , inputFocusBorder = borderColor
  , inputBgBasic = inputBgBasicColor
  , inputBgHover = inputBgHoverColor
  , inputBgFocus = inputBgFocusColor
  , inputBgActive = inputBgActiveColor
  --, inputBgDisabled = gray07
  --, inputFgBasic = gray06
  --, inputFgHover = blue08
  --, inputFgFocus = blue08
  --, inputFgActive = blue07
  --, inputFgDisabled = gray07

  --, inputSndBasic = gray05
  --, inputSndHover = gray06
  --, inputSndFocus = gray05
  --, inputSndActive = gray05
  --, inputSndDisabled = gray03

  --, inputHlBasic = gray07
  --, inputHlHover = blue08
  --, inputHlFocus = blue08
  --, inputHlActive = blue08
  --, inputHlDisabled = gray08

  --, inputSelBasic = gray06
  --, inputSelFocus = blue06

  , inputText = foregroundColor
  --, inputTextDisabled = gray02
  , labelText = foregroundColor

  --, scrollBarBasic = gray01 & L.a .~ 0.2
  --, scrollThumbBasic = gray07 & L.a .~ 0.6
  --, scrollBarHover = gray01 & L.a .~ 0.4
  --, scrollThumbHover = gray07 & L.a .~ 0.8

  --, slMainBg = gray00
  --, slNormalBgBasic = transparent
  --, slNormalBgHover = gray05
  , slNormalText = foregroundColor
  , slNormalFocusBorder = borderColor

  --, slSelectedBgBasic = gray04
  --, slSelectedBgHover = gray05
  , slSelectedText = foregroundColor
  , slSelectedFocusBorder = borderColor

  --, tooltipBorder = gray09
  --, tooltipBg = gray04
  , tooltipText = foregroundColor
  }
  & L.userColorMap . at "rowBgColor" ?~ rowHoverColor
    where backgroundColor = rgbHex "#121212"
          borderColor = rgbHex "#999999"
          btnBgBasicColor = rgbHex "#BB86FC"
          btnBgHoverColor = rgbHex "#C596FF"
          btnBgFocusColor = rgbHex "#CBA1FF"
          btnBgActiveColor = rgbHex "#CBA1FF"
          btnBgDisabledColor = rgbHex "#595959"
          btnMainBgBasicColor = rgbHex "#03DAC5"
          btnMainBgHoverColor = rgbHex "#10E8D3"
          btnMainBgFocusColor = rgbHex "#63F2E4"
          btnMainBgActiveColor = rgbHex "#63F2E4"
          btnMainBgDisabledColor = rgbHex "#595959"
          btnTextColor = rgbHex "#121212"
          btnTextDisabledColor = rgbHex "#757575"
          inputBorderColor = rgbHex "#292929"
          inputBgBasicColor = rgbHex "#212121"
          inputBgHoverColor = rgbHex "#252525"
          inputBgFocusColor = rgbHex "#282828"
          inputBgActiveColor = rgbHex "#2A2A2A"
          rowHoverColor = rgbHex "#212121"
          foregroundColor = rgbHex "#EBEBEB"

