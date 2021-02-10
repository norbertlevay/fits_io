


package FITS_IO.Serialize is

   type SInt_Type_Arr is array (Positive_Count range <>) of Short_Integer;
   type LLFloat_Type_Arr is array (Positive_Count range <>) of Long_Long_Float;

   procedure SIntArr_Write
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : in SInt_Type_Arr);
   for SInt_Type_Arr'Write use SIntArr_Write;

   procedure LLFloatArr_Read
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : out LLFloat_Type_Arr);
   for LLFloat_Type_Arr'Read use LLFloatArr_Read;



end FITS_IO.Serialize;
