
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

   function End_Of_File (File : File_Type) return Boolean;

   function Stream (File : File_Type) return Ada.Streams.Stream_IO.Stream_Access;

   -----------------------------
   -- Input-Output Operations --
   -----------------------------

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


   -- Data

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count;


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

end FITS_IO;

