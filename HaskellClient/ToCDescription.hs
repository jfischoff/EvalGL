module ToCDescription where


data TPrimitive = TInt
                | TChar

data Primitive = PInt Int
               | PChar Char
               
data Fixed  = Fixed{
       f_array :: [Primitive]
    }    

data Variable = Variable {
           v_array :: [Primitive]
        }

data Record = Record 
    {
        value :: Primitive
    }
    
data Expression = EFixed Fixed
                | EVariable Variable
                | ERecord Record
    


                  
                  
                  
                  
                  
                  
                  
                  
                  
                  