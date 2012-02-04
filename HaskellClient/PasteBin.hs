instance DB.Binary CUInt where
    get = DB.getWord32be
    put x = DB.putWord32be x
        
instance DB.Binary CUChar where
    get = DB.getWord8
    put = DB.putWord8

instance DB.Binary CChar where
    get = DB.getWord8
    put = DB.putWord8

instance DB.Binary CFloat where
    get = DB.getWord32be
    put = DB.getWord32be

instance DB.Binary CInt where
    get = DB.getWord32be
    put = DB.getWord32be

instance DB.Binary CShort where
    get = DB.getWord16be
    put = DB.getWord16be

instance DB.Binary CUShort where
    get = DB.getWord16be
    put = DB.getWord16be