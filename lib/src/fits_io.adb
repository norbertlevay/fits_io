
with Ada.Strings.Fixed;
with Ada.Strings; -- Trim needed
use Ada.Strings; -- Left needed for Trim

with Mandatory; -- Result_rec needed
with Header;    -- Valued_Key_Record_Arr needed

with Ada.Streams.Stream_IO;

package body FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;


   ---------------------
   -- File Management --
   ---------------------

   function To_SIO_Mode(Mode : File_Mode) return SIO.File_Mode
   is
   begin
      case(Mode) is
         when In_File => return SIO.In_File;
         when Out_File => return SIO.Out_File;
         when Append_File => return SIO.Append_File;
         end case;
   end To_SIO_Mode;

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := ""; 
      Form : String := "")
   is
   begin
      SIO.Create(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
   end Create;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
   end Open;

   procedure Close  (File : in out File_Type)
   is
   begin
      SIO.Close(File.SIO_File);
   end Close;

   function End_Of_File (File : File_Type) return Boolean
   is
   begin
      return SIO.End_Of_File(File.SIO_File);
   end End_Of_File;

   function Stream (File : File_Type) return SIO.Stream_Access
   is
   begin
      return SIO.Stream(File.SIO_File);
   end Stream;

   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

   procedure Set_Index (File : File_Type; To : Positive_Count)
   is
   begin
      SIO.Set_Index(File.SIO_File, SIO.Positive_Count(To));
   end Set_Index;

   function Index (File : File_Type) return Positive_Count
   is
   begin
      return Positive_Count(SIO.Index(File.SIO_File));
   end Index;

   function Size  (File : File_Type) return Count
   is
   begin
      return Positive_Count(SIO.Size(File.SIO_File));
   end Size;


   -- FITS specific

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count
   is
      Data_Cnt : Count := 1;
   begin
      for I in NAXISn'Range
      loop
         Data_Cnt := Data_Cnt * NAXISn(I);
      end loop;
      return Data_Cnt;
   end Data_Element_Count;


end FITS_IO;

