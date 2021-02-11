

--with V3_Types; use V3_Types;
--with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;


package FITS_IO.Serialize is

   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_SWrite
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
      Item : T_Arr);

   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_SRead
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
      Item : out T_Arr);


   -- basic types

   type SInt_Type_Arr is array (Positive_Count range <>) of Short_Integer;
   type LLFloat_Type_Arr is array (Positive_Count range <>) of Long_Long_Float;

   procedure SIntArr_Write
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : in SInt_Type_Arr);

   procedure LLFloatArr_Read
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : out LLFloat_Type_Arr);

   for LLFloat_Type_Arr'Read use LLFloatArr_Read;
   for SInt_Type_Arr'Write use SIntArr_Write;



end FITS_IO.Serialize;
