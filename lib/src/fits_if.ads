
with Ada.IO_Exceptions;
with Ada.Direct_IO;
--use Ada.Direct_IO;

with FITS; use FITS;

package FITS_IF is
  
-- Direct_IO vs Stream_IO dilemma. FITS file consist of of equal sized blocks of data, 
	-- so it seems straightforward to use Direct_IO for it. However:
	-- [GNAT] records are stored in chunks rounded up to xy-size 
	-- [AdaRM for Direct_IO] [FIXME find exact citation] does not seem to state anywhere that data in physical file would be stored packed
	-- on the other hand
	-- [AdaRM fro Stream_IO] states [FIXME find exact citation] that streamable object are put to file adjecant to each other, e.g. without gaps.
	--
	-- E.g. under GNAT both Direct_IO and Stream_IO would work, but because RM guarantees no gaps in Stream_IO and it is not difficult to implement block access
	-- with Stream_IO, FITS_IO is based on Stream_IO.
	-- !!
	-- Other point to the dilamma is File_Mode: it is only 
	-- Direct_IO that has Inout_File: In_File Out_File Inout_File
	-- Stream_IO has                : In_File Out_file Append_file
	-- under GNAT one can do Inout_File by mode change after Open - a trick spedific to GNAT
	--
	-- Rationale (maybe):
	-- Direct_IO is array of equal records pointed to by Index -> can overwrite
	-- only specific location : it is safe, consistency of file not broken (cannot overwrite half record)
	--
	-- In Stream_IO Out_File deletes all file contens at Open(), Append_File adds to the FileEnd:
	-- e.g. it is impossible to partially overwrite an objct and leave the file inconsistent (not readable)
	--
	-- Inout allows overwrite in file. Out_ and Append_ writes always to empty place.
	--
	-- For FITS_IO: see FITS_IO as list of HDU's each containing 2 objects: (Padded)Header and (Padded)Data
	-- so Inout_File with incorrect positioning can leave file inconsistent (overwrite existing object in the file)
	-- Consistency in FITS is guaranteed by rules (keeping these rules can give wrong data, but should guarantee no crash):
	-- * END card must be present (?)
	-- * files is always multiple 2880 bytes
	-- * Header Data start always at 2880 border (padding rules after Header and Data)


 -- Block_IO  
 package BIO is new Ada.Direct_IO(Element_Type => Card_Block);
 use BIO;

 subtype File_Type is BIO.File_Type;--limited private with Default_Initial_Condition;
 --type File_Type is limited private with Default_Initial_Condition;

 subtype File_Mode is BIO.File_Mode;--(In_File, Inout_File, Out_File);

 type Count is range 0 .. BIO.Count'Last; 

 subtype Positive_Count is Count range 1 .. Count'Last;

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "") renames BIO.Create;

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

   -- sequential access by header-blocks

   procedure Read
     (File  : File_Type;
      Cards : out Card_Block) is null;

   procedure Write
     (File  : File_Type;
      Cards : Card_Block) is null;


   -- sequential access by data-blocks
   
    -- FIXME calculate block-size (e.g. range 1..N) in compile time
    -- at instantiation: N = (2880*8) / Data_Type'Size
    -- ? can be done at instantiation only:
    -- type Data_Block_Int16 is array (Positive_Count range 1..(2880*80)/Integer_16'Size) of Integer_16
    -- Read_Data_Int16 is new procedure Read_Data(Data_Type => Integer_16;
    --                                            Data_Block => Data_Block_Int16)


    -- NOTE 
      -- First instatiate Read/Write_Data for all expected Data_Types. After reading the Header
      -- execute the right instance selected by BITPIX/Data_Type.
      --
      -- To access data, first Header must be read to know data type and dimensions. 
      -- Set_HDU will position to begining of the HDU, e.g. start of the Header (not Data). 
      -- To call Read/Write_Data one needs to read (skip) the header, so the sequence is:
      --
      -- Set_HDU()          position to HDU start in FITS file
      -- Read_Header()      read through all Card_Blocks, until END-card found,
      --                    now file pointer points to DataUnit start
      -- Read/Write_Data()  now read/write data (by correct Data_Type instance of generic)
   
   generic
     type Data_Type is private;
     type Data_Block is array (Positive_Count range <>) of Data_Type;
   procedure Read_Data
     (File : File_Type;
      Data : out Data_Block);


   generic
     type Data_Type is private;
     type Data_Block is array (Positive_Count range <>) of Data_Type;
   procedure Write_Data
     (File : File_Type;
      Data : Data_Block);
      
   -- NOTE consider optimization: 
   -- Read/Write array of Blocks : DataBlock_Arr is array (<>) of Data_Block
   -- similarly for Header Block
   -- This with Stream_IO should result in one access to disk for complete array.


   procedure Set_HDU(File : File_Type; HDUNum : Positive_Count);
      -- position to begining of HDU before sequential access



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
