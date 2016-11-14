{-# LANGUAGE TemplateHaskell #-}


module SERA.Finance.IO.Xlsx.Template {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  xlsxTemplate
) where


import Data.ByteString (ByteString)
import Data.FileEmbed (embedOneFileOf)


xlsxTemplate :: ByteString
xlsxTemplate = $(embedOneFileOf ["data/hrs-finance-template.xlsx", "../data/hrs-finance-template.xlsx"])
