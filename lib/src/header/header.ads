
-- Funcs in this module always operate (read/write) on all Header:
-- Read funcs below always read all header, e.g.
-- leave File_Index pointing to Data Unit start
-- FIXME how to make this explicit ? func name, some param??


with Ada.Streams.Stream_IO;
with FITS_IO; use FITS_IO; -- NAXIS_Array needed
with Mandatory;-- Result_Rec needed
with Ada.Strings.Bounded; use Ada.Strings.Bounded; -- Max20 only FIXME !!
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 

package Header is

--   package SIO renames FITS_IO;
   package SIO renames Ada.Streams.Stream_IO;

   type Valued_Key_Record_Arr is array (Natural range <>) of Optional.Valued_Key_Record;
    -- FIXME temp used in Init.Init_Reads


   -- API read Header

   function  Read_Mandatory
       (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec;

   function  Read_Optional
      (FitsFile : in  SIO.File_Type;
      Keys : in Optional.Bounded_String_8_Arr) return Card_Arr;


   -- API constructing Header

   procedure Write_Card_SIMPLE(F : in SIO.File_Type; Value : in Boolean);
   procedure Write_Card_XTENSION(F : in SIO.File_Type; Ext_Name : in String);

   procedure Write_Cards(F : in SIO.File_Type; Cards : in Card_Arr);
   -- adds cards after last written; call several times until header completed

   --procedure Close(F : in FITS_IO.File_Type);
   -- writes last END-card and padding



   -- lib-internals(?):

   function Has_Card(Cards : Card_Arr; Key : String; Value : out String) return Boolean;

   function To_Value_String( V : in Integer) return String;
   function To_Value_String( V : in String) return String;
   function To_Value_String( V : in Boolean) return String;

   function Create_Card(Key : in String; Value : in String) return String_80;
   function Create_Mandatory_Card(Key : in String; Value : in String) return String_80;
   --function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return Card_Arr;
   function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return String_80_Array;


end Header;

