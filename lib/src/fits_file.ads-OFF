-- basic empty header
-- __________________________
-- END
--
-- needed for Add_Cards func which needs END card


-- basic empty primary header
-- _______________________________
-- SIMPLE = F
-- END
--

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

-- Fits_File.HDU[i] sould be the Stream


-- User can use :

-- String80'Write(Stream, Card) or
-- String80_Array'Write(Stream, Cards)

-- T_Arr'Write(Stream, Data) T_Arr'Read(Stream, Data)

-- this class should keep track of correct positionning within the opened FITS-File (as stream)


-- to enable API:
-- Fits_File.Extend_Header(Stream, Cards) <- handle ENDCard&Padding; always keep Header complete
-- Fits_File.Parse_Header(Stream, Cards)  <- reads all header

-- OR
--
-- Ext2 := Fits_File.Extension[2];
-- Ext2.Set(Biblio)
--
-- S_Ext2 : Ext2.Stream;
--
-- T_Arr'Write(S_Ext2, Data_Arr);
--
--
-- NOTE shared access ?? -> user can easily open two HDU streams...
-- OR
-- allow only one hdu access forced by constructor, having HDUNum as param ??

--___________________________________________________________________________________
-- Use case: create file with one (primary) HDU

-- Create(File : in out File_Type; Name : String; Mode : File_Mode; Form : Shared)
-- (Create is always HDUNum := 0)
-- media management provides File_Type/Stream

-- Prim_Im : Image_Type := (NAXIS'Last, BITPIX, NAXISn); <- can go to Prim or any Ext[i]
--
-- PHDU : Primary := Create_Header(File_Stream; Prim_Im : Image_Type);
-- EHDU : Conforming_Extension := Create_Header(File_Stream; Prim_Im : Image_Type);
-- how to "insert" PHDU (later ExtHDU) into FITS-file being created ??
-- design assumption: PHDU and EHDU fit into memory
--
-- Fits_File.Set_Primary(File:File_Stream; PHDU);
-- Fits_File.Append_Extension(File:File_Stream; EHDU);
--
-- loop until all cards written
--  E/PHDU.Extend_Header(File_Stream; More_Cards);
-- end loop
--
-- DS : Data_Stream := PHDU.Close_Header;  <-- access to sequential data stream
--
-- write data sequentially:
--
-- loop until all data writted
--  T_Arr'Write(DS, Data_Arr);   <-- writes padding based on Index stored behind S
--                                     (Undef and Scaling is also stored inside S)
-- end loop;
--
-- PHDU.Close(DS); <-- close data stream (needed ? could simplify padding write)

-- above is about: CREATE sequential DATA-STREAM with properties as
-- defined by Cards/Header/Attributes

-- e.g. design Stream with settable properties
--___________________________________________________________________________________

-- FitsFile/HDU implements Header as set of Data-attributes (Get/Set-like funcs)
-- FitsFile/HDU provides Stream() func for T_Arr'Read/'Write
-- to access underlying Data_Stream which knows DU-end, Scaling,
-- and Undef vals --> User can call T_Arr'Write T_Arr'Read and data get converted, scaled,
-- undef-vals replaced, and exception raised if read/write over end-of-DU


-- Class Fits_File will implement Header as set of DU-attributes 
-- and for DataUnit will return Stream() - user must use this stream for T_Arr'Read T_Arr'Write
-- (similarly to Ada.Text_IO.Stream())
--
-- Such FITS Data_Stream will know:
-- * DU size (to guard for end-of-DU and padding)
-- * Scaling [A,B] for converting data values at 'Read 'Write
-- * Undef values

-- Fits_File := Create/Open(FileName, HDUNum)
--
-- Fits_File.Primary.HDU.Header.Card[4] <- reaches 4th card in prom header
-- Fits_File.Ext[2].HDU.Type <- returns Image Table BinTable
-- Fits_File.Primary.HDU.Data_Stream <- returns Data stream for 'Read 'Write:
-- Fits_File.Ext[2].HDU.Data_Stream <- returns Data stream for 'Read 'Write:
--       T_Arr'Write(Data_Stream, Data)
--       T_Arr'Read (Data_Stream, Data)
--


---------------------------------------------------------------------------------
-- FITS_File(FileName, HDUNum)  <- constructor: opens only ONE HDU

attributes:
-- FITS_File.Create_Header(Image/Table/BinTable_Type)
-- FITS_File.Extend_Header(Cards)
-- Image := FITS_File.Parse_Header(Image/Table/BinTable) - parse for a  type; if not match ->error
-- Cards := FITS_File.Parse_Cards(Keys) - parse for more cards

data:
-- FITS_File.Stream() returns Data_Unit_Stream of a specific HDU

-- Implement ONLY posiibility to acquire one Stream at a time (avoid sharying problems)



package Fits_File is

   type Fits_File_Type is limited private;


   -- HDU's represents Streams for each HDU section

   package HDU is


   type HDU_Type is limited  private;
   -- HDU_Type plays the role of File_Type in functional-API
   --e.g. represents the stream
   -- it will hold SIO.File_Type and Indexes: HDU_Start ENDCard_Pos, DU_First,DU_Last
   -- and should be capable of 'Read 'Write (like cin << blabla; )


   -- Header serves as attributes -> access with Get/Set funcs by category as in Standard
   -- FIXME instead of Key_Array/Card_Array is in Ada Map (container of Key-Value pair) ?

   function  Get(HDU : HDU_Type; Keys  : Key_Array) return String80_Array;
   procedure Set(HDU : HDU_Type; Cards : String80_Array);

   -- where

   Biblio      : constant Key_Array;
   Observation : constant Key_Array;
   -- etc...


   function Stream return Data_Stream_Access;
   -- return underlying Stream to write/read data:
   -- T_Arr'Read(Data_Stream, Data)
   -- T_Arr'Write(Data_Stream, Data)

   private

   type HDU_Control_Block is -- extends Stream_IO.Stream/File_Type 
           record
         DU_Stream : Ada.Stream_Access;
         Header : Header_Unit;
         Data   : Data_Unit;
      end record;
   -- Is like File_Control_Block e.g. controls access to one DU within FitsFile

   end HDU;

   Primary   : HDU;
   Extension : array (Positive_Count range <>) of HDU;

   function Stream(File : FITS_IO.File_Type; HDUNum : Count) returns Stream_Access;

   -- OR / AND
   function FITS_Streams(File : FITS_IO.File_Type) returns HDU_Stream_Access_Array;
   -- returns record: {Prim; Ext[n]} each of type Stream_Access



   -- HOW TO ACCESS STREAM ATTRIBITES -->>

   -- NOTE RM 13.13.1 The Package Streams :

   -- "The abstract type Root_Stream_Type is the root type of the class of stream types.
   -- The types in this class represent different kinds of streams.
   -- A new stream type is defined by extending the root type (or some other stream type),
   -- overriding the Read and Write operations, 
   -- and optionally defining additional primitive subprograms, according to the
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- requirements of the particular kind of stream." 

   -- e.g. Add to HDU_Control_Block primitive Get/Set funcs for Undef_Value and Scaling_AB
   --

   function  Get_Undef(HDU : HDU_Type) return Float;
   procedure Set_Undef(HDU : HDU_Type; Undef_Value : Float);

   function  Get_Scaling_A(HDU : HDU_Type) return Float;
   function  Get_Scaling_B(HDU : HDU_Type) return Float;
   procedure Set_Scaling_A(HDU : HDU_Type; A : Float);
   procedure Set_Scaling_B(HDU : HDU_Type; B : Float);

  -- HDU is return from Stream(FitsFIle) return Stream_Access
   -- where HDU_Type is derived from Root stream access (via Stream_IO)


private

   type Fits_File_Type is
      record
         Name : String;
         SIO_File : SIO.File_Type;
      end record;

end Fits_File;
