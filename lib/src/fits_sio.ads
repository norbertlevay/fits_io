

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;


package FITS_SIO is

   type HDU_Type is
      record
         HDU_Start   : Positive_Count;  -- 1 or set by Set_Extension_Number()
         ENDCard_Pos : Count;            -- set by Write_Header/_Cards
         DU_First    : Count;            -- set by Read_Header/_Cards  Write_Header/_Cards
         DU_Last     : Count;            -- set by Read_/Write_Header
         DU_End_Written : Boolean;       -- True when DU_Last written (Close err: file incomplete)
      end record;

   Null_HDU : HDU_Type := (1,0,0,0,False);


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


   -----------------------
   -- FITS File content --
   -----------------------

   procedure Set_Extension_Number(File: File_Type; HDU: in out HDU_Type; To: Positive_Count) is null;
   function Read_Content (FFile : File_Type) return HDU_Info_Type;


   -------------------------
   -- Metadata Operations --
   -------------------------

   function Read_Header(FFile: in out File_Type; HDU: in out HDU_Type; Keys: BS_8_Array) return Image_Rec; -- Parse_Image_Header

   function Read_Cards(FFile: in out File_Type; HDU: in out HDU_Type; Keys: BS_8_Array) return  String_80_Array; -- Parse_Cards


   procedure Write_Header  -- Generate_Image_Header 
      (File : in out File_Type;
      HDU   : in out HDU_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array);

   procedure Write_Cards  -- Add_Cards
      (File : in out File_Type;
      HDU   : in out HDU_Type;
      Cards : String_80_Array);


   -- Conversions, Scaling and Undefined Values

   procedure Set_Linear_Scaling    (HDU : in out HDU_Type; A,B : Float);
   procedure Set_Undefined_Physical(HDU : in out HDU_Type; Undef_Phys : Float);


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
      HDU   : in out HDU_Type;
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
      HDU   : in out HDU_Type;
      Item : T_Arr);


   ----------------
   -- Exceptions --
   ----------------

   End_Error         : exception renames Ada.IO_Exceptions.End_Error;
   Programming_Error : exception;


end FITS_SIO;

