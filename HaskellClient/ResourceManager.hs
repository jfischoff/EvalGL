module ResourceManager where
import qualified Data.ByteString as BS
    
type Id   = Int
type Size = Int
type Offset = Int

  
data ResourceCommand a = Create Id Size
                       | Delete Id
                       | Get Id 
                       | Put Id BS.ByteString
                       | Run [Offset] a
               

             
