
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

   package FITS_IO is

      type File_Type is limited private;

      type File_Mode is (In_File, Out_File, Append_File);

      type     Count          is new Ada.Streams.Stream_IO.Count;
      subtype  Positive_Count is Count range 1 .. Count'Last;

      -- Metadata is stored in Cards

      subtype String_80 is String(1 .. 80);
      ENDCard   : constant String_80 := ('E','N','D', others => ' ');
      EmptyCard : constant String_80 := (others => ' ');

      package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
      package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

      type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;

      function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80;

      type String_80_Array is array (Positive_Count range <>) of String_80;

      -- Data: Image

      type DU_Type is
         (Int8, UInt16, UInt32, UInt64,
         UInt8,  Int16,  Int32,  Int64,
         F32, F64);

      subtype NAXIS_Index is Integer range 1 .. 999;
      type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

      type Image_Rec(NAXIS : NAXIS_Index; Card_Count : Count) is
          record
               Data_Type   : DU_Type;
               NAXISn      : NAXIS_Array(1 .. NAXIS);
               Image_Cards : String_80_Array(1 .. Card_Count);
           end record;


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

      procedure Set_Extension_Number (File : File_Type; To : Positive_Count) is null;
      -- 1 = first extension, 2 = second extension, etc...

      procedure Close  (File : in out File_Type);
      procedure Reset  (File : in out File_Type; Mode : File_Mode);
      function  Mode    (File : File_Type) return File_Mode;
      function  End_Of_File (File : File_Type) return Boolean;

      function Stream (File : File_Type) return Ada.Streams.Stream_IO.Stream_Access;


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


      procedure Write_Header  -- Generate_Image_Header 
         (File       : in out File_Type;
         Raw_Type    : DU_Type;
         NAXISn      : NAXIS_Array;
         Image_Cards : String_80_Array);

      procedure Write_Cards  -- Add_Cards
         (File       : in out File_Type;
         Cards : String_80_Array);

      -- Conversions, Scaling and Undefined Values

      procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float);
      procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float);


      -- Data Unit access

   generic
      type T is private;
      type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
  procedure Read
     (File : File_Type;
      Item : out T_Arr;
      Last : out Count);

   generic
      type T is private;
      type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
  procedure Write
     (File : File_Type;
      Item : T_Arr);




  ----------------
  -- Exceptions --
  ----------------

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Programming_Error : exception;


   -- FIXME end of API ------------------------------------------------
   -- below should be private or hidden in body
   procedure Put_File_Type(File : File_Type; Prefix : String := "");
   -- FIXME for debug only - later Access_Rec to be hidden
   procedure Write_Data_Padding(FFile : File_Type);
   -- FIXME take out of API - needed because Data-padding in Data_Unit not implemented yet



   private



   -- RULE all Raw related params load from Write_Header/Write_Cards functions
   -- all Physical related params loaded by API funcs called by user (incl T_Arr generic T)
   -- Raw:  User -> Header   -> set Raw-value
   -- Phys: User -> API-call -> set Phys value

   -- RULE File.Scaling loaded/inited only when END-Card & Padding read/written
   -- without that Read/Write to Data Unit will fail (BITPIX = 0?)


   -- Access Data Unit

   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;
   -- used by Data_Unit.Read/Write
   -- if not initialized, DU access not possible
   -- it is initialized from Cache after Header is completed (created or read)

   -- Cache

   -- cache of values filled in during manipulating the Header (reading or creating it)
   -- and by User API calls Set_*(File : in out File_Type, <param to set>)
   -- Cache is initialized at Create/Open and reset at Close.

   type Cache_Rec is
      record
         BITPIX : Integer; -- from Header BITPIX
         Aui, Ah, Bh, Au, Bu : Float;
         -- Aui - Tab11 shift Int-UInt conversions
         -- Ah Bh - values from Header BZERO BSCALE
         -- Au Bu - values from User calling Set_Linear_Scaling()
         Physical_Undef_Valid : Boolean;-- set from Set_Undefined_Physical()
         Physical_Undef_Value : Float;
         Raw_Undef_Valid : Boolean;-- set from Header-BLANK
         Raw_Undef_Value : Float;
      end record;


   type File_Type is record
      SIO_File : Ada.Streams.Stream_IO.File_Type;
      ENDCard_Pos : Ada.Streams.Stream_IO.Positive_Count;-- keep track where is END-card
      Scaling  : Access_Rec;-- load it at Write_Header_End and Read_Header
      Cache    : Cache_Rec;
   end record;

end FITS_IO;

