{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, DeriveGeneric #-}
module HaskellOpenGLFunctions where
import ParseOpenGLFunction
import HaskellOpenGLFunctionsTH
import ResourceManager
import GLFunction
import Control.Applicative ((<$>))
import Language.Haskell.TH
import Control.Monad
import GLPrimitives
import GHC.Generics
import Command


$(do
    (Right fs) <- runIO $ load_and_parse_header_file "OpenGLFunctions.h" 
    make_haskell_interface' "to_run" fs)
   
