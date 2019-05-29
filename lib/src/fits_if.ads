
with Ada.IO_Exceptions;
with Ada.Direct_IO;

with FITS; use FITS;

package FITS_IF is
   
 -- Block_IO  
 package BIO is new Ada.Direct_IO(Element_Type => Card_Block);

 type File_Type is limited private with Default_Initial_Condition;

 type File_Mode is (In_File, Inout_File, Out_File);

 type Count is range 0 .. BIO.Count'Last; 

 subtype Positive_Count is Count range 1 .. Count'Last;

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "") is null;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is null;

   procedure Close  (File : in out File_Type) is null;
   procedure Delete (File : in out File_Type) is null;
   procedure Reset  (File : in out File_Type; Mode : File_Mode) is null;
   procedure Reset  (File : in out File_Type) is null;

--   function Mode (File : File_Type) return File_Mode;
--   function Name (File : File_Type) return String;
--   function Form (File : File_Type) return String;

--   function Is_Open (File : File_Type) return Boolean;

   procedure Flush (File : File_Type) is null;

   -------------------------------------
   -- HDU Input and Output Operations --
   -------------------------------------

   procedure Read
     (File : File_Type;
      Card : out Card_Type) is null;

   procedure Write(
     File : File_Type;
     Card : Card_Type) is null;


     -- FIXME dummy
   type NCube_Type is new Integer;

--   generic  FIXME generic cannot be "is null"
--     type Data_Type is private;
   procedure Read
     (File  : File_Type;
      NCube : in out NCube_Type;--(DataType); FIXME constraint not allowed here error
      From  : Positive_Count) is null;

--   generic FIXME
--     type Data_Type is private;
   procedure Write
     (File  : File_Type;
      NCube : NCube_Type;--(DataType); FIXME constraint not allowed here error
      To    : Positive_Count) is null;

   procedure Set_HDU(File : File_Type; HDUNum : Positive_Count);
   
--   function HDU(File : File_Type) return Positive_Count;
--   function DataUnit_Size(File : File_Type) return Count;

--   function End_Of_LastHeaderBlock (File : File_Type) return Boolean;
--   function End_Of_LastDataUnitBlock (File : File_Type) return Boolean;

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;
-- read/write over the out of HDU-area
-- FITS standard violation

private
   type File_Type is new BIO.File_Type;

end FITS_IF;
