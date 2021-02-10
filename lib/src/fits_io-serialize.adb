
with Ada.Text_IO;

with HDU;
with FITS_IO; use FITS_IO;
with Ada.Tags;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

package body FITS_IO.Serialize is

   package TIO renames Ada.Text_IO;

   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_SRead
      (FFile : in out HDU_Stream_Access;
      Item : out T_Arr;
      Last : out Count);


   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_SWrite
      (FFile : in out HDU_Stream_Access;
       Item : T_Arr);


   procedure HDU_SRead
      (FFile    : in out HDU_Stream_Access;
      Item : out T_Arr;
      Last : out Count)
   is
      procedure iRead is new HDU.My_Read( T, T_Arr, "+", "+", Is_Undef,To_BITPIX);
   begin
      iRead(File_Type(FFile).all.SIO_File, File_Type(FFile).all.PHDU, Item, Last);
   end HDU_SRead;


   procedure HDU_SWrite
      (FFile : in out HDU_Stream_Access;
      Item : T_Arr)
   is
      File : File_Type := File_Type(FFile);
      procedure iWrite is new HDU.My_Write( T, T_Arr, "+", "+", Is_Undef,To_BITPIX);
   begin
      iWrite(File.SIO_File, File.PHDU, Item);
   end HDU_SWrite;


   -- instantiate for specific types

   procedure SIntArr_Write
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : in SInt_Type_Arr)
   is  
      procedure SIntArrWrite is new HDU_SWrite(Short_Integer, SInt_Type_Arr);
      FS : HDU_Stream_Access := HDU_Stream_Access(FFile);
      use type Ada.Tags.Tag;
   begin
      TIO.Put("SIntArr_Write" );
      if(FFile.all'Tag = FITS_Stream_Type'Tag)
      then
         TIO.Put(" FITS_Stream_Type ");
         SIntArrWrite(FS, Item);
         -- on FITS stream do scale and undef
      else
         SInt_Type_Arr'Write(FFile, Item);
         -- on other Stream just do what Ada offers
      end if;
   end SIntArr_Write;





   procedure LLFloatArr_Read
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : out LLFloat_Type_Arr)
   is  
      Last : Count;
      procedure LLFloatArrRead is new HDU_SRead(Long_Long_Float, LLFloat_Type_Arr);
      FS : HDU_Stream_Access := HDU_Stream_Access(FFile);
   begin
      TIO.Put("LLFloatArr_Read");
      LLFloatArrRead(FS, Item, Last);
      -- FIXME error if Last /= Item'Length,  or ?
   end LLFloatArr_Read;





end FITS_IO.Serialize;
