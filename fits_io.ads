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

with Interfaces;
 -- for DataUnit definitions

package FITS_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Inout_File, Out_File, Append_File);

   --  The following representation clause allows the use of unchecked
   --  conversion for rapid translation between the File_Mode type
   --  used in this package and System.File_IO.

   for File_Mode use
     (In_File     => 0,  -- System.File_IO.File_Mode'Pos (In_File)
      Inout_File  => 1,  -- System.File_IO.File_Mode'Pos (Inout_File);
      Out_File    => 2,  -- System.File_IO.File_Mode'Pos (Out_File)
      Append_File => 3); -- System.File_IO.File_Mode'Pos (Append_File)

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
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "shared=no"); --[GNAT 9.2 FORM strings]

   procedure Close (File : in out File_Type);

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
   type HDU_Info_Type is record
      CardsCnt : Positive;  -- number of cards in this Header
      Data     : Data_Type; -- data type as given by BITPIX
      BitPixOctets : Positive; -- size in octets for given BITPIX
      Naxes    : Natural;   -- [FITS 4.4.1.1 Primary Header] "A value of zero signifies that no data follow the header in the HDU."
      Naxis    : Dim_Type;  -- data dimensions, 0 means dimension not in use
   end record;

   Null_HDU_Info : constant HDU_Info_Type := (1,Int32,4,0,(others=>0));

   type HDU_Info_Arr is array (Positive range <>) of HDU_Info_Type;

   function List_HDUInfo (File : in File_Type) return HDU_Info_Arr;

   -----------------
   -- Data access --
   -----------------

 type Int8Arr_Type is
   array ( Natural range <> ) of Interfaces.Integer_8;
 pragma Pack (Int8Arr_Type);

 type Int16Arr_Type is
   array ( Natural range <> ) of Interfaces.Integer_16;
 pragma Pack (Int16Arr_Type);

 type Int32Arr_Type is
   array ( Natural range <> ) of Interfaces.Integer_32;
 pragma Pack (Int32Arr_Type);

 type Int64Arr_Type is
   array ( Natural range <> ) of Interfaces.Integer_64;
 pragma Pack (Int64Arr_Type);

 type Float32Arr_Type is
   array ( Natural range <> ) of Float; --FIXME verify size
 pragma Pack (Float32Arr_Type);

 type Float64Arr_Type is
   array ( Natural range <> ) of Long_Float;--FIXME verify size
 pragma Pack (Float64Arr_Type);

 type DataArray_Type ( Option : Data_Type ;
                       Length : Natural ) is
   record
     case Option is
      when Int8  => ArrInt8  : Int8Arr_Type (1 .. Length);
      when Int16 => ArrInt16 : Int16Arr_Type(1 .. Length);
      when Int32 => ArrInt32 : Int32Arr_Type(1 .. Length);
      when Int64 => ArrInt64 : Int64Arr_Type(1 .. Length);
      when Float32 => ArrFloat32 : Float32Arr_Type(1 .. Length);
      when Float64 => ArrFloat64 : Float64Arr_Type(1 .. Length);
     end case;
   end record;

   function  Read (File       : in File_Type;
                   HDU_Num    : in Positive;  -- 1,2,3...
                   DataType   : in Data_Type;
                   FromOffset : in Positive;  -- in units of DataType
                   Length     : in Positive ) -- in units of DataType
    return DataArray_Type;

   procedure Write (File     : in File_Type;
                    HDU_Num  : in Positive;        -- 1,2,3...
                    ToOffset : in Positive;        -- in units of DataType
                    Data     : in DataArray_Type ); -- has length Length(Data)




private

 type File_Type_Record;
 type File_Type is access File_Type_Record;

 -- FIXME here would follow list of pragma Inline,
 -- but since Ada2012 obsolete, add anyway for older compilers

end FITS_IO;
