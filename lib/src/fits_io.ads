
-- All FitsFile is represented by File_Type and HDU after open is Primary (SIO_HDU_First = 1).
-- Call Set_Exte_Num(File) to position to other HDU. 

-- Then the follwoing rules apply:

   -- SIO_HDU_First = 1 for Primary HDU
   -- Set_Extension_Number(F) : initializes SIO_HDU_First to index of the given ExtHDU

   -- Write_Header: always writes at HDU First index and all Header
   -- Write_Cards : always writes at END_Card position, shifting it behind new cards

   -- Write_Data  : is sequential and each write checks offset,
   -- a, attempt to write over DU_Last raises End_Of_DU error
   -- b, not completing all DU will raise 'Ivalid Fits File' error at Close()

   -- Read_Header : parses any of known HDU-types (Image,Table.BinTable) and 
   --                always reads (parses) all Header starting from SIO_HDU_First
   -- Read_Cards  : as Read_Header but parses any cards given by Keys

   -- any attempt to call Write_Data Read_Data before calling Header read/write
   -- results in Programmin_Error: DU unknown, must access Header first

-- Rules for File_Mode IN Out In_Out:
-- Out cuts file at position File.Index
-- Append after Open positions at end...
-- Read access anywhere, size does not change
-- Update (In_Out) not allowed to change size
--


-- FIXME Tcalc in Scaling should be generic (now it is Float)

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

with Cache; use Cache; -- Access_Rec & Cache_Rec
with System.File_Control_Block; -- GNAT specific
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with FITS;
with HDU;

package FITS_IO is

   type HDU_Stream_Access is limited private;

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   subtype     Count          is FITS.Count;
   --type     Count          is new Ada.Streams.Stream_IO.Count;
   subtype  Positive_Count is FITS.Positive_Count;--Count range 1 .. Count'Last;
   --subtype  Positive_Count is Count range 1 .. Count'Last;

   -- Card

   subtype String_80 is FITS.String_80;
--   subtype String_80 is String(1 .. 80);
--   ENDCard   : constant String_80 := ('E','N','D', others => ' ');
--   EmptyCard : constant String_80 := (others => ' ');

   package BS_8 renames FITS.BS_8;
   package BS20 renames FITS.BS20;
   package BS70 renames FITS.BS70;
--   package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
--   package BS20 is new Ada.Strings.Bounded.Generic_Bounded_Length(20);
--   package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

   subtype BS_8_Array  is FITS.BS_8_Array;
   --type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;
   subtype String_80_Array is FITS.String_80_Array;
   --type String_80_Array is array (Positive_Count range <>) of String_80;

   -- Header

   subtype NAXIS_Index is FITS.NAXIS_Index;
   subtype NAXIS_Array is FITS.NAXIS_Array;
--   subtype NAXIS_Index is Integer range 1 .. 999;
--   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;


   type HDU_Info_Type(NAXIS : Positive) is
      record
         XTENSION : BS20.Bounded_String;
         CardsCnt : Positive_Count;
         BITPIX   : Integer;
         NAXISn   : NAXIS_Array(1..NAXIS);
      end record;

   -- Image metadata

   subtype DU_Type is FITS.DU_Type;

   subtype Image_Rec is HDU.Image_Rec;


   ---------------------
   -- File Management --
   ---------------------

   procedure Create
      (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := ""; 
      Form : String := "");

   procedure Open
      (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close  (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : File_Mode);
   function  Mode    (File : File_Type) return File_Mode;
   function  End_Of_File (File : File_Type) return Boolean;

   function Size  (File : File_Type) return Count;

   function Stream (File : File_Type) return Ada.Streams.Stream_IO.Stream_Access;


   -----------------------
   -- FITS File content --
   -----------------------

   procedure Set_Extension_Number (File : File_Type; To : Positive_Count) is null;
   -- 1 = first extension, 2 = second extension, etc...

   function Read_Content (FFile : File_Type) return HDU_Info_Type;


   -------------------------
   -- Metadata Operations --
   -------------------------

   function  Read_Header   -- Parse_Image_Header
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return Image_Rec;

   function  Read_Cards -- Parse_Cards
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return  String_80_Array;


   procedure Write_Header_Prim  -- Generate_Prim_Image_Header 
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array);

   procedure Write_Header_Ext  -- Generate_Ext_Image_Header 
      (File       : in out File_Type;
      Raw_Type    : DU_Type;     -- 
      NAXISn      : NAXIS_Array; -- FIXME later do: HDU'Class -> Image_Rec Table_Rec BinTable_Rec
      Optional_Cards : String_80_Array);


   procedure Write_Cards  -- Add_Cards
      (File       : in out File_Type;
      Cards : String_80_Array);



   -- Conversions, Scaling and Undefined Values

   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float);
   procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float);


   -----------------------------
   -- Data Unit random access --
   -----------------------------

   function  Index(File : File_Type) return Positive_Count;
   procedure Set_Index(File : File_Type; Ix : Positive_Count);
   -- Index range: 1 .. DU_Last


   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_Read
      (FFile : in out File_Type;
      Item : out T_Arr;
      Last : out Count);


   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_Write
      (FFile : in out File_Type;
       Item : T_Arr);

   -- via Stream

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

   -- FIXME instantiate for all FITSv3Types and do:
   -- for VrType'Write use VrType_HDU_SWrite;

   type SInt_Type_Arr is array (Positive_Count range <>) of Short_Integer;

procedure SIntArr_Write
      (FFile :  access  Ada.Streams.Root_Stream_Type'Class;
      Item : in SInt_Type_Arr);

   for SInt_Type_Arr'Write use SIntArr_Write;


--   procedure SIntArr_Write is new FITS_IO.HDU_Write(Short_Integer, SInt_Type_Arr);
   ----------------
   -- Exceptions --
   ----------------

   End_Error         : exception renames Ada.IO_Exceptions.End_Error;
   Programming_Error : exception;


   procedure Put_File_Type(File : File_Type; Prefix : String := "");
   -- FIXME for debug only

   function HDU_Stream(File : File_Type) return access Ada.Streams.Root_Stream_Type'Class;
-- HDU_Stream_Access;


   private

   package SIO renames Ada.Streams.Stream_IO;


   type FITS_Stream_Type is new Ada.Streams.Root_Stream_Type with record
   --type File_Type is record
      SIO_File  : SIO.File_Type;
      PHDU : HDU.HDU_Type;
   end record;
   type File_Type is access all FITS_Stream_Type;

--   type HDU_Stream_Access is access all File_Type'Class;
   type HDU_Stream_Access is access  all Ada.Streams.Root_Stream_Type'Class;
--   type HDU_Stream_Access is access all HDU_Stream_AFCB'Class

   overriding procedure Read
     (File : in out FITS_Stream_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Read operation used when Stream_IO file is treated directly as Stream

   overriding procedure Write
     (File : in out FITS_Stream_Type;
      Item : Ada.Streams.Stream_Element_Array);
   --  Write operation used when Stream_IO file is treated directly as Stream



end FITS_IO;

