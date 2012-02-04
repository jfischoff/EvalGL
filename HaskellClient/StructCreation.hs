{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, ForeignFunctionInterface,
    OverloadedStrings #-}
module StructCreation where
import Data.Word
import Types
import Data.Data
import Control.Arrow
import Foreign
import Foreign.C.Types
import Debug.Trace
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import qualified Data.Binary as DB
import qualified Data.Binary.Put as DB
import qualified Data.Binary.Get as DB
import Data.Binary
import Data.DeriveTH
import GHC.Generics
import Control.Applicative
import Control.Monad.Identity
--The test here is how to handle the creation of a struct 

data GLPrimitive = PGLbitfield GLbitfield
                 | PGLboolean GLboolean
                 | PGLbyte GLbyte
                 | PGLchar GLchar
                 | PGLclampf GLclampf
                 | PGLenum GLenum
                 | PGLfloat GLfloat
                 | PGLint GLint
                 | PGLshort GLshort
                 | PGLsizei GLsizei
                 | PGLubyte GLubyte
                 | PGLuint GLuint
                 | PGLushort GLushort
                 deriving(Show, Eq, Generic)
                 
class GBinary f where
    gput :: f a -> DB.Put
    
instance GBinary U1 where
    gput x = return ()
    
instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gput (x :*: y) = do gput x; gput y
        
instance (GBinary a, GBinary b) => GBinary (a :+: b) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x
  
instance (GBinary a) => GBinary (M1 i c a) where
  gput (M1 x) = gput x

instance (DB.Binary a) => GBinary (K1 i a) where
  gput (K1 x) = DB.put x
  
instance DB.Binary GLPrimitive where
  put x = gput $ from x
  get = undefined
                 
data CValue = Struct [CValue]
            | Union CValue Int
            | Primitive GLPrimitive
            | Array [CValue]
            | Pointer CValue
            | Member CValue Int
            | Id Int
            deriving(Show)
            
type CEnv = [(Int, CValue)]
        
type CState = State CEnv
            
add_array value = do
    count <- gets length
    modify ((count, value):)
    return count
            
class ToCValue a where
    to_c :: a -> CState CValue
  
instance ToCValue GLPrimitive where
    to_c = return . Primitive 
    
instance ToCValue GLbitfield where
    to_c = to_c . PGLbitfield

instance ToCValue GLboolean where
    to_c = to_c . PGLboolean

instance ToCValue GLbyte where
    to_c = to_c . PGLbyte

instance ToCValue GLchar where
    to_c = to_c . PGLchar

instance ToCValue GLclampf where
    to_c = to_c . PGLclampf

instance ToCValue GLenum where
    to_c = to_c . PGLenum

instance ToCValue GLfloat where
    to_c = to_c . PGLfloat

instance ToCValue GLint where
    to_c = to_c . PGLint

instance ToCValue GLshort where
    to_c = to_c . PGLshort

instance ToCValue GLsizei where
    to_c = to_c . PGLsizei

instance ToCValue GLubyte where
    to_c = to_c . PGLubyte

instance ToCValue GLuint where
    to_c = to_c . PGLuint

instance ToCValue GLushort where
    to_c = to_c . PGLushort

pad_to amount x =  ((2 * amount) - (x `mod` amount)) `mod` amount
    
align :: CValue -> CValue
align x = Member x $ pad_to 4 . size_of $ x

size_of (Struct members) = sum . map size_of $ members
size_of (Union x pad)    = (size_of x) + pad
size_of (Primitive x)    = size_of_pgl x
size_of (Array xs)       = length xs * size_of (head xs)
size_of (Pointer x)      = 4
size_of (Member x pad)   = size_of x + pad

size_of_pgl (PGLbitfield x) = 4
size_of_pgl (PGLboolean x)  = 1
size_of_pgl (PGLbyte x)     = 1
size_of_pgl (PGLchar x)     = 1
size_of_pgl (PGLclampf x)   = 4
size_of_pgl (PGLenum x)     = 4
size_of_pgl (PGLfloat x)    = 4
size_of_pgl (PGLint x)      = 4
size_of_pgl (PGLshort x)    = 2
size_of_pgl (PGLsizei x)    = 4
size_of_pgl (PGLubyte x)    = 1
size_of_pgl (PGLuint x)     = 4
size_of_pgl (PGLushort x)   = 2

traceIt x = trace (show x) x
traceItNote note x = trace (note ++ " " ++ (show x) ++ "\n") x

type Offset = Word32
type Fixup = Offset

data TestObject = TestObject 
    {
        x :: GLint,
        y :: GLchar,
        z :: GLfloat
    }
    
instance ToCValue TestObject where    
    to_c (TestObject x y z) = do
        x_c <- to_c x 
        y_c <- to_c y 
        z_c <- to_c z
        return $ Struct $ map align [x_c, y_c, z_c]
    
data TestList = TestList 
    {
        test_list_x :: GLchar,
        list :: [TestObject]
    }
    
instance ToCValue TestList where
    to_c (TestList x y) = do
        x_c <- to_c x
        ys <- mapM to_c y
        id <- add_array (Array ys)
        z_c <- to_c (fromIntegral $ length y :: GLint) 
        return $ Struct $ map align [x_c, Pointer $ Id id, z_c]
        
{-
    the new idea for pack is
    write out the objects 
    the pointer pack should just add the id
    and add the location of the fixup array
    
    So pack takes CEnv
    
    it just blasted out the CValues and records the location of the fixups
    and it records the location of the ids
    later it replaces the ids with the location of the ids
    
-}

version = 1 :: Word32

mk_header fixups = DB.runPut $ do
    DB.putWord32le version
    DB.putWord32le (fromIntegral $ length fixups :: Word32) 
    mapM_ DB.putWord32le fixups

packWord32 = DB.runPut . DB.putWord32le
unpackWord32 = DB.runGet DB.getWord32le

type Objects = [(Int, Offset)]

type PackState = StateT Objects (StateT [Fixup] (StateT BS.ByteString Identity))



pack :: CEnv -> BS.ByteString
pack env = result where
    (((_, objects), fixups), bytes) = runIdentity $ runStateT (runStateT (runStateT (pack' env) []) []) BS.empty
    header = mk_header fixups
    new_fixups  = map ((fromIntegral $ BS.length header)+) fixups
    new_header  = mk_header $ traceIt new_fixups
    new_objects = offset_objects (BS.length new_header) objects
    new_bytes  = replace_fixups bytes fixups $ new_objects
    result = BS.append new_header new_bytes
    
replace_word32 :: BS.ByteString -> Word32 -> Word32 -> BS.ByteString
replace_word32 bytes offset index = result where
    --split at the start
    (start, end) = BS.splitAt (fromIntegral index) bytes
    --split after
    (word_bytes, end') = BS.splitAt 4 end
    --turn the 4 bytes into a word
    word = unpackWord32 word_bytes :: Word32
    --add the offset 
    offset_word = word + offset
    --convert back to bytes
    new_word_bytes = packWord32 offset_word
    --reconnect everything
    result = BS.concat [start, new_word_bytes, end']

replace_fixups bytes fixups offset = foldl (\b f -> offset_word32 b offset f) bytes fixups


pack' :: CEnv -> PackState ()
pack' = undefined    

        
{-
instance ToCValue ResourceMapper where
    to_c (ResourceMapper should_map ids) = return . Struct $ map align [to_c should_map, 
                    Array $ map to_c ids]

test = ResourceMapper (GLboolean True) [0,1,2]




    
add_pointer_array array = do
    array_count <- lift $ gets length
    lift $ modify ((array_count, array):) 
    
pack' :: CValue -> PackState () 
pack' (Struct members) = mapM_ pack' members
pack' (Union x count)  = pack_and_pad x count 
pack' (Primitive x)    = append_bytestring $ encode x
pack' (Array xs)       = mapM_ pack' xs
pack' (PointerValue x)      = do 
    index <- get_index
    add_fixup $ fromIntegral index
    append_bytestring $ encode $ index + 1
    pack' x
pack' (Pointer x)      = do 
        index <- get_index
        let pointer_count = length xs
            object_size = size_of $ head xs
            fixup_value fixup_index = fromIntegral $ (index+) $ fromIntegral $ (object_size * fixup_index) + 
                                        ((pointer_count - fixup_index) * 4) 
            fixup_location fixup_index =  (fixup_index * 4) + (traceIt index)
            add_fixup' index = add_fixup $ fromIntegral $ fixup_location index
            append_pointer index = append_bytestring $ packWord32 $ (fixup_value index :: Word32)
            add_pointer index = do
                                    add_fixup' index
                                    append_pointer $ fromIntegral index
        mapM_ add_pointer [0..fromIntegral pointer_count - 1]
        mapM_ pack' xs
pack' (Member x count) = pack_and_pad x count 

pack_and_pad x count = do pack' x; pad count

add_fixup :: Fixup -> PackState ()
add_fixup x = modify (x:)

get_index :: PackState Int32
get_index = lift $ gets $ fromIntegral . BS.length

append_bytestring x = lift $ modify $ (flip BS.append) x


pad count = append_bytestring $ BS.pack $ take count (cycle [0])

        
test_a = TestObject 438129054 (GLchar 'a') 1.5
test_b = TestObject 102 (GLchar 'b') 5.5

test_list = TestList (GLchar 't') [test_a, test_b]

test_list_g = to_c test_list
packed = pack test_list_g 

as_bytes = BS.unpack packed

as_bytes_a = BS.unpack $ pack $ to_c $ test_a
as_bytes_b = BS.unpack $ pack $ to_c $ test_b

--the test
--make a command list
--with two commands
--err
-- do the same test that I currently have
-- make the same structs

-}














