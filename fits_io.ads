----------------------------------------------------------------------
-- Ref:                                                             --
--                                                                  --
-- FITS : Definition of the Flexible Image Transport System (FITS)  --
--        The FITS Standard                                         --
--        Version 3.0: approved 2008 July 10 by the IAUFWG          --
--        Document publication date: 2010 November 18               --
--                                                                  --
-- ADA  : Ada Reference Manual, ISO/IEC 8652:2012(E)                --
--        Language and Standard Libraries                           --
--        Copyright @ 2008, 2009, 2010, 2011, 2012 AXE Consultants  --
--                                                                  --
-- GNAT : GNAT Reference Manual                                     --
--        GNAT, The GNU Ada Compiler                                --
--        For gcc version 4.8.2                                     --
--        by AdaCore                                                --
--        Copyright c 1995-2012, Free Software Foundation, Inc.     --
----------------------------------------------------------------------

with Ada.Strings.Bounded;
 -- for Header definition

package FITS_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Inout_File, Out_File, Append_File);

   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   -- Header definition

   package Card is new Ada.Strings.Bounded.Generic_Bounded_Length(CardSize);
   type Header_Type is array (Positive range <>) of Card.Bounded_String;
   -- Headers stored in text files are lines of max 80 characters

   HDU_AfterLast : constant := Positive'Last;
   -- Write's default behaviour is append a Header after last HDU in file

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (Fits : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Open
     (Fits : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Close (Fits : in out File_Type);

   function  Mode     (File : in File_Type) return File_Mode;
   procedure Set_Mode (File : in out File_Type; Mode : in File_Mode);

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   function  Read (File : in File_Type; HDU_Num : in Positive) return Header_Type;

   procedure Write
     (File    : in File_Type;
      Header  : in Header_Type;
      HDU_Num : in Positive := HDU_AfterLast); -- default: Append

   ---------------------------------
   -- FITS-file structure (HDU's) --
   ---------------------------------

   type Data_Type is (Int8, Int16, Int32, Int64, Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8]

   for  Data_Type use
     (Int8    =>   8, -- Character or unsigned binary integer
      Int16   =>  16, -- 16-bit two's complement binary integer
      Int32   =>  32, -- 32-bit two's complement binary integer
      Int64   =>  64, -- 64-bit two's complement binary integer
      Float32 => 932, -- IEEE single precision floating point
      Float64 => 964);-- IEEE double precision floating point
      -- FIXME [FITS] defines Floats negative -32 -64

   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]

   type Dim_Type is array (1..MaxAxes) of Natural;
   type HDU_Info is record
      CardsCnt : Positive;  -- number of cards in this Header
      Data     : Data_Type; -- data type as given by BITPIX
      BitPixOctets : Positive; -- size in octets for given BITPIX
      Naxes    : Natural;   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies that no data follow the header in the HDU."
      Naxis    : Dim_Type;  -- data dimensions, 0 means dimension not in use
   end record;

   Null_HDU_Info : constant HDU_Info := (1,Int32,4,0,(others=>0));

   type HDU_Info_Arr is array (Positive range <>) of HDU_Info;

   function List_HDUInfo (File : in File_Type) return HDU_Info_Arr;

private

 type File_Data;
 type File_Type is access File_Data;

 -- FIXME here would follow list of pragma Inline,
 -- but since Ada2012 obsolete, add anyway for older compilers

end FITS_IO;
