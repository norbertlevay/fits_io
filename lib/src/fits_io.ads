
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

package FITS_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   type     Count          is new Ada.Streams.Stream_IO.Count;
   subtype  Positive_Count is Count range 1 .. Count'Last;

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
   function Mode    (File : File_Type) return File_Mode;
   function End_Of_File (File : File_Type) return Boolean;

   function Stream (File : File_Type) return Ada.Streams.Stream_IO.Stream_Access;

   -----------------------------
   -- Input-Output Operations --
   -----------------------------


   -- Cards

   subtype String_80 is String(1 .. 80);
   ENDCard   : constant String_80 := ('E','N','D', others => ' ');
   EmptyCard : constant String_80 := (others => ' ');

   package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
   package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

   function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80;

   type String_80_Array is array (Positive_Count range <>) of String_80;

   procedure Read
     (File : in out File_Type;
      Item : out String_80_Array;
      Last : out Count);

   procedure Write
     (File : in out File_Type;
      Item : String_80_Array);


   -- Image

   type DU_Type is
      (Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   procedure Write_Image
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array);

   procedure Write_End(File : in out File_Type);


   -- Header

   type Image_Rec(NAXIS : NAXIS_Index; Card_Count : Count) is
       record
            Data_Type   : DU_Type;
            NAXISn      : NAXIS_Array(1 .. NAXIS);
            Image_Cards : String_80_Array(1 .. Card_Count);
        end record;

   type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;

   function  Read_Header
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return Image_Rec;

   procedure Write_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array);


   -- Data

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count;

   procedure Write_Data_Padding(FFile : File_Type);


   -----------------------------------------------
   -- Conversions, Scaling and Undefined Values --
   -----------------------------------------------

   procedure Set_Raw_Type(File : in out File_Type; Raw_Type : DU_Type);
   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float);
   procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float);

   procedure Put_File_Type(File : File_Type; Prefix : String := "");
   -- FIXME for debug only - later Access_Rec to be hidden

   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

   procedure Set_Index (File : File_Type; To : Positive_Count);

   function Index (File : File_Type) return Positive_Count;
   function Size  (File : File_Type) return Count;


  ----------------
  -- Exceptions --
  ----------------

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;



   private

   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;

   -- RULE all Raw related params load from Write_Header/Write_Cards functions
   -- all Physical related params loaded by API funcs called by user (incl T_Arr generic T)
   -- Raw:  User -> Header   -> set Raw-value
   -- Phys: User -> API-call -> set Phys value

   -- RULE File.Scaling loaded/inited only when END-Card & Padding read/written
   -- without that Read/Write to Data Unit will fail (BITPIX = 0?)

   type File_Type is record
      SIO_File : Ada.Streams.Stream_IO.File_Type;
      Scaling  : Access_Rec;-- load it at Write_Header_End and Read_Header

      BITPIX : Integer; -- from Header BITPIX
      Aui, Ah, Bh, Au, Bu : Float;
      -- Aui - Tab11 shift Int-UInt conversions
      -- Ah Bh - values from Header BZERO BSCALE
      -- Au Bu - values from user calling Set_Linear_Scaling()

      Physical_Undef_Valid : Boolean;-- set from Set_Undefined_Physical()
      Physical_Undef_Value : Float;
      Raw_Undef_Valid : Boolean;-- set from Header-BLANK
      Raw_Undef_Value : Float;

   end record;

end FITS_IO;

