
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;


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

  type Card_Array is array (Positive_Count range <>) of String_80;

   procedure Read
     (File : File_Type;
      Item : out Card_Array;
      Last : out Count);

   procedure Write
     (File : File_Type;
      Item : Card_Array);

   -- Header

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;


   type Image_Rec(NAXIS : NAXIS_Index; Key_Count : Count) is
       record
            BITPIX     : Integer;
            NAXISn     : NAXIS_Array(1 .. NAXIS);
            Array_Keys : Card_Array(1 .. Key_Count);
        end record;

   function  Read_Header(File : File_Type) return Image_Rec;
   procedure Write_Header(File : File_Type; Image : Image_Rec) is null;

   -- read/write Mandatory-record
   -- read/append Optional-cards (read may return null array) in: Key_Array   out: Card_Array
   -- always read/write all header, up to END_Card (incl padding: skip at read; append at write)


   -- Data

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count;

   type Data_Unit_Type is limited private;

   type DU_Type is (
       Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   procedure Create
      (Data_Unit : in out Data_Unit_Type;
      File     : in File_Type;
      Raw_Type : in DU_Type);

   procedure Open
      (Data_Unit : in out Data_Unit_Type;
      File     : in File_Type;
      Raw_Type : in DU_Type);

   procedure Close
      (Data_Unit : in out Data_Unit_Type;
      FFile : in File_Type);



   -- FIXME hide this
   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;
   -- FIXME must be visible for init.ads - resolve later


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
   end record;


   type Data_Unit_Type is record
      Scaling : Access_Rec;
   end record;



end FITS_IO;

