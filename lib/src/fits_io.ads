
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
     (File : File_Type;
      Item : out String_80_Array;
      Last : out Count);

   procedure Write
     (File : File_Type;
      Item : String_80_Array);

   -- Header

   type DU_Type is (
       Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   type Image_Rec(NAXIS : NAXIS_Index; Key_Count : Count) is
       record
            Data_Type  : DU_Type;
            NAXISn     : NAXIS_Array(1 .. NAXIS);
            Array_Keys : String_80_Array(1 .. Key_Count);
        end record;

   type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;

   function  Read_Header(FFile : File_Type; Keys : BS_8_Array) return Image_Rec;

   function  Write_Image(File : File_Type; DType : DU_Type;
            NAXISn : NAXIS_Array; Array_Keys : String_80_Array) return Image_Rec;
   procedure Write_End(File : File_Type);


   -- Data

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count;

   type Data_Unit_Type is limited private;

   procedure Create
      (Data_Unit : in out Data_Unit_Type;
      File       : File_Type;
      Image      : Image_Rec;
      Phys_Used  : Boolean := False;
      Phys_Value : Float := 0.0;
      A : Float := 0.0;
      B : Float := 1.0);
 
   procedure Open
      (Data_Unit : in out Data_Unit_Type;
      File       : File_Type;
      Image      : Image_Rec;
      Phys_Used  : Boolean := False;
      Phys_Value : Float := 0.0;
      A : Float := 0.0;
      B : Float := 1.0);


   procedure Close
      (Data_Unit : in out Data_Unit_Type;
      FFile : File_Type);



   -- FIXME hide this
   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;
   -- FIXME must be visible for init.ads - resolve later

   ----------------------------------------------
   -- Converions, Scaling and Undefined Values --
   ----------------------------------------------

   procedure Set_Raw_Type(File : in out File_Type; Raw_Type : DU_Type);
   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float);
   procedure Set_Undefined_Values(File : in out File_Type; Undef_Raw, Undef_Phys : Float);

   function Get(File : File_Type) return Access_Rec;
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

   type File_Type is record
      SIO_File : Ada.Streams.Stream_IO.File_Type;
      Scaling  : Access_Rec;
   end record;


   type Data_Unit_Type is record
      Scaling : Access_Rec;
   end record;



end FITS_IO;

