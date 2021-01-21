
-- API approach question: what does FITS_IO.File_Type represent ?
--
-- a, file of concatinated HDU's (then internally stores HDU sizes positions)
-- b, one HDU (then internally stores Metadata/attributes of the opened HDU)
--
-- b, case: Open Create... refer HDU and after Open we have acces to DataUnit directly
-- (file name uses extended syntax: someFitsFile.fits[4] opens 4th HDU
--
-- a, case: after Open we should see array of HDUs and Set_HDU_Number should move
-- to "current HDU"
--
-- NOTE seems cfitsio took 'mixed' approach, the FitsFile-handle represents both, an HDU
-- and also array of concatinated HDUs - cfitsio's API has operations for both on
-- the same FitsFile type

-- FIXME Tcalc in Scaling should be generic (now it is Float)

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

with System.File_Control_Block; -- GNAT specific

package FITS_IO is

   type HDU_Stream_Access is limited private;

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   type     Count          is new Ada.Streams.Stream_IO.Count;
   subtype  Positive_Count is Count range 1 .. Count'Last;

   -- Card

   subtype String_80 is String(1 .. 80);
   ENDCard   : constant String_80 := ('E','N','D', others => ' ');
   EmptyCard : constant String_80 := (others => ' ');

   package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
   package BS20 is new Ada.Strings.Bounded.Generic_Bounded_Length(20);
   package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

   type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;
   type String_80_Array is array (Positive_Count range <>) of String_80;

   -- Header

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   type HDU_Info_Type(NAXIS : Positive) is
      record
         XTENSION : BS20.Bounded_String;
         CardsCnt : Positive_Count;
         BITPIX   : Integer;
         NAXISn   : NAXIS_Array(1..NAXIS);
      end record;

   -- Image metadata

   type DU_Type is
      (Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

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

   procedure Close  (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : File_Mode);
   function  Mode    (File : File_Type) return File_Mode;
   function  End_Of_File (File : File_Type) return Boolean;

   function Stream (File : File_Type) return Ada.Streams.Stream_IO.Stream_Access;
   function Stream (File : File_Type; HDU_Num : Count) return HDU_Stream_Access;


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


   ---------------------------------
   -- Data Unit sequential access --
   ---------------------------------

   generic
   type T is private;
   type T_Arr is array (Positive_Count range <>) of T;
   with function "+"(V : in Float) return T     is <>; 
   with function "+"(V : in T)     return Float is <>; 
   with function Is_Undef  (V,U : in T) return Boolean is <>; 
   with function To_BITPIX (V   : in T) return Integer is <>; 
   procedure HDU_Read
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
   procedure HDU_Write
      (FFile : File_Type;
       Item : T_Arr);


   ----------------
   -- Exceptions --
   ----------------

   End_Error         : exception renames Ada.IO_Exceptions.End_Error;
   Programming_Error : exception;




   procedure Put_File_Type(File : File_Type; Prefix : String := "");
   -- FIXME for debug only

   private



   -- RULE all Raw related params load from Write_Header/Write_Cards functions
   -- all Physical related params loaded by API funcs called by user (incl T_Arr generic T)
   -- Raw:  User -> Header   -> set Raw-value
   -- Phys: User -> API-call -> set Phys value

   -- RULE File.Scaling loaded/inited only when END-Card & Padding read/written
   -- without that Read/Write to Data Unit will fail (BITPIX = 0?)


   -- Access Data Unit

   type Access_Rec is
      record
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
      DU_First  : Ada.Streams.Stream_IO.Positive_Count; -- start of the DataUnit
      DU_Length : Positive_Count;
      Scaling  : Access_Rec;-- load it at Write_Header_End and Read_Header
      Cache    : Cache_Rec;
   end record;
   -- FIXME divide private section to two Records:
   -- * File_Type: FITS-File related (HDUStart, HDU Site, etc...) and
   -- * HDU_Type: AccessRec, Cache_Rec, attributes (Header card keys)...
   -- for now keep HDU_Type part of File_Type. Consider: should HDU_Type have its
   -- own operations ? (Creat(HDU,..) Open(HDU,..) Close(HDU) etc)

   ----------------
   -- HDU Stream --
   ----------------

   package FCB renames System.File_Control_Block;

   type HDU_Stream_AFCB is new FCB.AFCB with record
      Index : Count := 1;
      --  Current Index value

      -- FITS specific fields

      ENDCard_Pos : Ada.Streams.Stream_IO.Positive_Count;-- keep track where is END-card
      DU_First  : Ada.Streams.Stream_IO.Positive_Count; -- start of the DataUnit
      DU_Length : Positive_Count;
      Scaling  : Access_Rec;-- load it at Write_Header_End and Read_Header
      Cache    : Cache_Rec;
 
   end record;

   type HDU_Stream_Access is access all HDU_Stream_AFCB'Class;
   type HDU_Type is access all HDU_Stream_AFCB;

   overriding function AFCB_Allocate
     (Control_Block : HDU_Stream_AFCB) return FCB.AFCB_Ptr;

   overriding procedure AFCB_Close (File : not null access HDU_Stream_AFCB);
   overriding procedure AFCB_Free  (File : not null access HDU_Stream_AFCB);

   overriding procedure Read
     (File : in out HDU_Stream_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Read operation used when Stream_IO file is treated directly as Stream

   overriding procedure Write
     (File : in out HDU_Stream_AFCB;
      Item : Ada.Streams.Stream_Element_Array);
   --  Write operation used when Stream_IO file is treated directly as Stream

   -- FITS Header becomes HDU_Stream attributes: atribute set/get funcs
   -- corrspong to Read_Cards Write_Cards
   procedure Set
     (File : in out HDU_Stream_AFCB;
      Item : Ada.Streams.Stream_Element_Array) is null;
   procedure Get
     (File : in out HDU_Stream_AFCB;
     Item : out Ada.Streams.Stream_Element_Array) is null;


end FITS_IO;

