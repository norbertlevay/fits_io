
generic
type T is private;

with function "+"(V : in Float)   return T is <>; 
with function "+"(V : in T) return Float   is <>; 
with function Is_Undef  (V,U : in T) return Boolean is <>; 
with function To_BITPIX (V   : in T) return Integer is <>; 

package FITS_IO.Data_Unit is

   type T_Arr is array (Positive_Count range <>) of T;


   -- FIXME ? hide inside body and in Read/Write calls below,
   --  replace with Image_Data_Model used to read/write Header
   type Scaling_Rec(NAXIS_Last : Natural) is
      record
         Raw      : Image_Data_Model(NAXIS_Last);
         Physical : Image_Data_Model(NAXIS_Last);
      end record;


   -- Xfer params: READ IS FULLY SPECIFIED
   -- Physical.BITPIX = func(T) (choice: user application dictates)
   -- all Raw : from Header
   -- -- Physical.Undef (optional override)
  procedure Read
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec;  --   <- replace with Image_Data_Model ??
      Item : out T_Arr;
      Last : out Count);



   -- Xfer params: WRITE leaves free selection for triple Raw.[BITPIX; A,B]
   -- all Physical (choice: user data dictates) Phys.BITPIX = func(T)
   -- Raw.BITPIX
   -- Raw.[A,B]
   -- -- Raw.Undef (optional override)
  procedure Write
     (File    : SIO.File_Type;
      Scaling : Scaling_Rec; --  <- replace with Image_Data_Model ??
      Item : T_Arr);

  -- NOTE should [BITPIX A,B] :
  -- 1, use default Raw.BITPIX = Phys.BITPIX A=0.0 B=1.0
  -- 2, use Tab11 when UInt<->Int conversion
  -- 3, optionally parametrize to depend on Physical.Min/Max of the Data

end FITS_IO.Data_Unit;
