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
import Data.DeriveTH
import GHC.Generics
import Control.Applicative
import Control.Monad.Identity
import Data.Maybe
import GenericBinary
--The test here is how to handle the creation of a struct

instance DB.Binary GLPrimitive where
  put x = gput $ from x
  get = undefined 

data TGLPrimitive = TGLbitfield
                  | TGLboolean
                  | TGLbyte
                  | TGLchar
                  | TGLclampf
                  | TGLenum
                  | TGLfloat
                  | TGLint
                  | TGLshort
                  | TGLsizei
                  | TGLubyte
                  | TGLuint
                  | TGLushort
                 deriving(Show, Eq)

data CType = TStruct String [CType]
           | TUnion  String [CType]
           | TPrimitive TGLPrimitive
           | TArray Int CType
           | TPointer CType
           | TMember String CType
           deriving(Show, Eq)

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
                 
data Value = VStruct [Value]
           | VUnion String Value
           | VPrimitive GLPrimitive
           | VArray [Value]
           | VPointer [Value]
           | VMember Value
           
                 
data CValue = Struct [CValue]
             | Union CValue CType
             | Primitive GLPrimitive
             | Array [CValue]
             | Pointer CValue
             | Member CValue Word32
             | Id Word32
             | EmptyValue
             deriving(Show)
                       
            
type CEnv = [(Int, CValue)]
        
type CState = State CEnv
            
add_array value = do
    count <- gets length
    modify ((count, value):)
    return $ fromIntegral count
    
class ToCType a where
    to_c_type :: a -> CType
            
class ToCValue a where
    to_c :: a -> CState CValue
    
class ToValue a where
    to_v :: a -> Value
    
    
  
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

pad_to amount x =  (((2 * amount) - (x `mod` amount)) `mod` amount) + x

    
size_of_t (TStruct _ members)    = sum . map size_of_t $ members
size_of_t (TUnion x types)       = maximum $ map size_of_t types
size_of_t (TPrimitive x)         = size_of_tgl x
size_of_t (TArray count x)       = count * size_of_t x
size_of_t (TPointer _)           = 4
size_of_t (TMember _ x)          = pad_to 4 $ size_of_t x

size_of_tgl TGLbitfield = 4
size_of_tgl TGLboolean  = 1
size_of_tgl TGLbyte     = 1
size_of_tgl TGLchar     = 1
size_of_tgl TGLclampf   = 4
size_of_tgl TGLenum     = 4
size_of_tgl TGLfloat    = 4
size_of_tgl TGLint      = 4
size_of_tgl TGLshort    = 2
size_of_tgl TGLsizei    = 4
size_of_tgl TGLubyte    = 1
size_of_tgl TGLuint     = 4
size_of_tgl TGLushort   = 2

size_of (Struct members) = sum . map size_of $ members
size_of (Union x typ)    = size_of_t typ
size_of (Primitive x)    = size_of_pgl x
size_of (Array xs)       = (fromIntegral . length $ xs) * size_of (head xs)
size_of (Pointer x)      = 4
size_of (Member x pad)   = pad_to 4 $ size_of x

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

--write the function that combines a type with a value
--by recursing through and added the union types
add_type_info :: CType -> CValue -> CValue
add_type_info (TStruct _ members) (Struct s_members) = Struct $ zipWith add_type_info members s_members
add_type_info u@(TUnion _ members) (Union x _)       = Union x u
add_type_info (TArray _ typ) (Array xs)              = Array $ zipWith add_type_info (cycle [typ]) xs
add_type_info (TMember _ typ) (Member v _)           = Member (add_type_info typ v) 0
add_type_info _ x                                    = x
  


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
    deriving(Show, Eq, Generic)
    

    
instance ToCValue TestObject where    
    to_c (TestObject x y z) = do
        x_c <- to_c x 
        y_c <- to_c y 
        z_c <- to_c z
        return $ Struct [x_c, y_c, z_c]
  
data TestList = TestList 
    {
        test_list_x :: GLchar,
        list :: [TestObject]
    }
    deriving(Show, Eq, Generic)
    
instance ToCValue TestList where
    to_c (TestList x y) = do
        x_c <- to_c x
        ys <- mapM to_c y
        id <- add_array (Array ys)
        z_c <- to_c (fromIntegral $ length y :: GLint) 
        return $ Struct [x_c, Pointer $ Id id, z_c]
       
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

type Objects = [(Word32, Offset)]

type PackState = StateT Objects (StateT [Fixup] (StateT BS.ByteString Identity))

pack :: CEnv -> BS.ByteString
pack env = result where
    (((_, objects), fixups), bytes) = runIdentity $ runStateT (runStateT (runStateT (pack' env) []) []) BS.empty
    header = mk_header fixups
    new_fixups  = map ((fromIntegral $ BS.length header)+) fixups
    new_header  = mk_header $ traceIt new_fixups
    new_objects = offset_objects (fromIntegral $ BS.length new_header) objects
    new_bytes  = replace_fixups bytes fixups $ new_objects
    result = BS.append new_header new_bytes
    
offset_objects offset objects = map (second (offset+)) objects
    
replace_word32 :: BS.ByteString -> Objects -> Word32 -> BS.ByteString
replace_word32 bytes objects index = result where
    --split at the start
    (start, end) = BS.splitAt (fromIntegral index) bytes
    --split after
    (word_bytes, end') = BS.splitAt 4 end
    --turn the 4 bytes into a word
    word = unpackWord32 word_bytes :: Word32
    --add the offset 
    replacement_word = fromJust $ lookup word objects
    --convert back to bytes
    new_word_bytes = packWord32 replacement_word
    --reconnect everything
    result = BS.concat [start, new_word_bytes, end']

replace_fixups bytes fixups objects = foldl (\b f -> replace_word32 b objects f) bytes fixups



add_fixup :: Fixup -> PackState ()
add_fixup x = lift $ modify (x:)

get_index :: PackState Int32
get_index = lift $ lift $ gets $ fromIntegral . BS.length

append_bytestring :: BS.ByteString -> PackState ()
append_bytestring x = lift $ lift $ modify $ (flip BS.append) x

pad :: Word32 -> PackState ()
pad count = append_bytestring $ BS.pack $ take (fromIntegral count) (cycle [0])

pack' :: CEnv -> PackState ()
pack' env = mapM_ (\(i, v) -> pack_value i v) env

add_id i = do 
    index <- get_index
    modify ((fromIntegral i, fromIntegral index):)

pack_value :: Int -> CValue -> PackState () 
pack_value i v = do add_id i; pack_value' v

pack_value' :: CValue -> PackState ()
pack_value' (Struct members) = mapM_ pack_value' members
pack_value' (Union x typ)  = pack_and_pad x $ fromIntegral $ size_of_t typ 
pack_value' (Primitive x)    = append_bytestring $ DB.encode x
pack_value' (Array xs)       = mapM_ pack_value' xs
pack_value' (Pointer (Id i)) = do 
        index <- fromIntegral <$> get_index
        add_fixup index
        append_bytestring $ packWord32 $ i
pack_value' (Member x count) = pack_and_pad x count 
 
pack_and_pad :: CValue -> Word32 -> PackState ()
pack_and_pad x count = do pack_value' x; pad count

test_a = TestObject 438129054 (GLchar 'a') 1.5
test_b = TestObject 102 (GLchar 'b') 5.5

test_list = TestList (GLchar 't') [test_a, test_b]

 

encode :: ToCValue a => a -> CEnv
encode x = result where 
    (value, state) = runState (to_c x) []
    result = (fromIntegral $ length state, value):state 

test_list_g = encode test_list
packed = pack test_list_g 

as_bytes = BS.unpack packed

as_bytes_a = BS.unpack $ pack $ encode $ test_a
as_bytes_b = BS.unpack $ pack $ encode $ test_b

write_file = BS.writeFile "test_objects.bin" (BS.append (packWord32 $ fromIntegral $ BS.length packed) packed)
        

--instance ToCValue ResourceMapper where
--    to_c (ResourceMapper should_map ids) = return . Struct $ map align [to_c should_map, 
--                    Array $ map to_c ids] -- to do I need a function that fills in the array with empty
                                          -- data upto the maximum
                                          

                                          
                                          













