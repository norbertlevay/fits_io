--
-- Implementation notes:
--
-- Set_Index allows positioning in the stream if the media allows it
-- (for files yes, for network maybe?, for pipes?, for stdin stdout?).
--
-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...
-- File_Type is Access. Nevertheless verify in out...
--
-- General API problem: which API call should move the file-index ?
--   And how to convey info where the new file-index points ?
--
-- Re-consider API:
--   Offset in Set_Index() should be in 'Read() 'Write().
--   Motivation:
--   concept of Block is part of the FITS standard.
--   This API would support read/write in blocks -
--   e.g. always guarantee naturally correct size and fill-in areas.
--   If Offset is in Set_Index only Stream_IO is supported.
--   Interfaces like Direct_IO(Block) are not.
--   E.g. such new API:
--   procedure Stream(File_Type; HDUNum) <- position to begining of HDU
--   procedure used for 'Read (FITSStream, FITS_Data_Type, Offset )
--   procedure used for 'Write(FITSStream, FITS_Data_Type, Offset )
--
--   Possible at all(?): 'Read/'Write are pre-defined Stream attributes with 2 param only(?)
--
--   Drawback:
--   Read/Write of DataUnit would need to calculate Header size
--   at first call of multiple writes. Successive writes can be
--   done directly by Stream: 'Write/'Read(FITSStream, Data).
--   After first read/write we are correctly positioned, and read/write
--   move the file pointer.
--   For multiple writes (reads) to (from) continuous area,
--   a client would do:
--   FIST_Data_Type'Write(FITS, Data(1), Offset); <-- func from FITS package with initial positioning
--   FIST_Data_Type'Write(FITS, Data(2) );        <-- func from Stream_IO, only write, no positioning
--   FIST_Data_Type'Write(FITS, Data(3) );
--   ... n-times
--

with Ada.Text_IO;


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

--with FITS.Header; use FITS.Header;

with Strict;
with Formulas;
with Keyword_Record;  use Keyword_Record;


package body File is

	package TIO renames Ada.Text_IO;
	package KW renames Keyword_Record;
   
   ---------------
   -- FITS.File :

   StreamElemSize_bits : FPositive := Ada.Streams.Stream_Element'Size;
    -- FIXME [GNAT somwhere says it is 8bits]
    -- [GNAT]:
    --  type Stream_Element is mod 2 ** Standard'Storage_Unit;
    -- (Storage_Unit a.k.a 'Byte' : smallest addressable unit)
    -- note:
    --  type Count is new Stream_Element_Offset
    --                range 0 .. Stream_Element_Offset'Last;
    --  type Stream_Element_Offset is range
    --               -(2 ** (Standard'Address_Size - 1)) ..
    --               +(2 ** (Standard'Address_Size - 1)) - 1;
    -- Address_Size is 32 or 64bit nowadays

   BlockSize_bytes : FPositive := BlockSize_bits / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index()

   --
   -- Padding
   --
   -- Padding Data Unit: [FITS 3.3.2 Primary Data Array]
   -- If the data array does not fill the final data block, the remain-
   -- der of the data block shall be filled by setting all bits to zero.
   -- And for conforming Data Extensions [FITS 7.1.3]:
   -- The data format shall be identical to that of a primary data array
   -- as described in Sect. 3.3.2.
   procedure Write_Padding(FitsFile : in SIO.File_Type;
                           From     : in SIO.Positive_Count;
                           PadValue : in Unsigned_8)
   is

    FillCnt   : constant Natural :=
       Natural( From rem SIO.Positive_Count(BlockSize_bytes) );
    PadLength : constant Natural :=
       Natural(BlockSize_bytes) - FillCnt + 1;

    PadArr    : constant UInt8_Arr(1 .. FPositive(PadLength)) := (others => PadValue);
    -- FIXME full of explicit casts!! review!!
   begin
    SIO.Set_Index(FitsFile,From);
    UInt8_Arr'Write(SIO.Stream(FitsFile),PadArr);
   end Write_Padding;


   function  Read_Card  (FitsFile  : in  SIO.File_Type)
     return Card_Type
   is
     Card : Card_Type;
   begin
     Card_Type'Read(Stream(FitsFile),Card);
     return Card;
   end Read_Card;

   function  Read_Cards (FitsFile  : in  SIO.File_Type)
     return Card_Block
   is
     CardBlock : Card_Block;
   begin
     Card_Block'Read(Stream(FitsFile),CardBlock);
     return CardBlock;
   end Read_Cards;

   procedure Write_Card  (FitsFile : in SIO.File_Type;
                          Card     : in Card_Type)
   is
   begin
     Card_Type'Write(Stream(FitsFile),Card);
   end Write_Card;
   pragma Inline (Write_Card);

   procedure Write_Cards (FitsFile : in SIO.File_Type;
                          Cards    : in Card_Arr)
   is
   begin
     Card_Arr'Write(Stream(FitsFile),Cards);
   end Write_Cards;
   pragma Inline (Write_Cards);


   -- Read Data
   procedure gen_Read_Data (FitsFile : in  SIO.File_Type;
                            Data     : in out Data_Arr)
   is
   begin
     Data_Arr'Read(Stream(FitsFile),Data);
   end gen_Read_Data;
   pragma Inline (gen_Read_Data);

--   procedure Read_Data is new genRead_Data(UInt8_Arr);
--   procedure Read_Data is new genRead_Data(Int16_Arr);
--   procedure Read_Data is new genRead_Data(Int32_Arr);
--   procedure Read_Data is new genRead_Data(Int64_Arr);
--   procedure Read_Data is new genRead_Data(Float32_Arr);
--   procedure Read_Data is new genRead_Data(Float64_Arr);
--

--   procedure Read_Data (FitsFile : in  SIO.File_Type;
--                        Data     : in out UInt8_Arr)
--   is
--   begin
--     UInt8_Arr'Read(Stream(FitsFile),Data);
--   end Read_Data;
--   pragma Inline (Read_Data);

--   procedure Read_Data (FitsFile : in  SIO.File_Type;
--                        Data     : in out Float32_Arr)
--   is
--   begin
--     Float32_Arr'Read(Stream(FitsFile),Data);
--   end Read_Data;
--   pragma Inline (Read_Data);

   procedure gen_Write_Data (FitsFile : in SIO.File_Type;
                             Data     : in Data_Arr)
   is
   begin
     Data_Arr'Write(Stream(FitsFile),Data);
   end gen_Write_Data;
   pragma Inline (gen_Write_Data);


--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in UInt8_Arr)
--   is
--   begin
--     UInt8_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

   -- ... for all types ...

--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in Float32_Arr)
--   is
--   begin
--     Float32_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in Float64_Arr)
--   is
--   begin
--     Float64_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

   -- REFACTOR for final

   procedure To_Coords (Offset    : in  FPositive;
                        MaxCoords : in  Strict.Positive_Arr;
                        Coords    : out Strict.Positive_Arr)
   is
	use Strict;
      Sizes : Strict.Positive_Arr := MaxCoords;
      Divs :  Strict.Positive_Arr := MaxCoords;
      Rems :  Strict.Positive_Arr := MaxCoords;
      -- FIXME these inits are needed only to eliminate Ada error
      -- find other solution
   begin

    --
    -- generate size of each plane
    --
    declare
      Accu  : FPositive := 1;
    begin
      for I in MaxCoords'Range
      loop
       Accu := Accu * MaxCoords(I);
       Sizes(I) := Accu;
       -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
      end loop;
    end;

    --
    -- calc divisions and fractions
    --
    declare
      PrevRem : FNatural := Offset - 1;
    begin
      for I in reverse MaxCoords'First .. MaxCoords'Last
      loop
        Divs(I) := 1 + PrevRem  /  Sizes(I);
        Rems(I) := 1 + PrevRem rem Sizes(I);
          -- FIXME rem gives 0 for multiples
        PrevRem := Rems(I) - 1;
      end loop;
    end;

    --
    -- pick the coordinates from Divs & Rems
    --
    Coords := Rems(Rems'First) & Divs(Rems'First..Divs'Last-1);
   end To_Coords;


   procedure Write_DataUnit (FitsFile  : in  SIO.File_Type;
                             MaxCoords : in  Strict.Positive_Arr)
   is

     function  multiply (MaxCoords : in  Strict.Positive_Arr) return FPositive
     is
      Accu  : FPositive := 1;
     begin
      for I in MaxCoords'Range
      loop
       Accu := Accu * MaxCoords(I);
      end loop;
      return Accu;
     end multiply;

    type Item_Arr is array (FPositive range <>) of Item;

    IArrLen : FPositive := multiply(MaxCoords);
    IArr    : Item_Arr(1..IArrLen);
    Coord   : Strict.Positive_Arr := MaxCoords;

    DPadCnt  : constant Positive  := 2880 - Natural(IArrLen mod FPositive(2880));
    ItemSize : constant Positive := Item'Size/Unsigned_8'Size;
    PadUInt8 : constant UInt8_Arr(1 .. FPositive(DPadCnt*ItemSize)) := (others => 0);
--    for Padding'Size use DPadCnt*(FITS.Data.Unsigned_8'Size);
-- FIXME How to guarantee that arrays are packed OR do we need to guarantee ?
   begin

    for I in IArr'Range
    loop
     To_Coords (I, MaxCoords, Coord);
     IArr(I) := Element (Coord);
    end loop;

    Item_Arr'Write(Ada.Streams.Stream_IO.Stream(FitsFile) ,IArr);
    UInt8_Arr'Write(Ada.Streams.Stream_IO.Stream(FitsFile) ,PadUInt8);

    -- FIXME write by Blocks

   end Write_DataUnit;





   --
   -- Read File until ENDCard found
   --

   procedure Read_Card (File : in SIO.File_Type;
			HStart : in SIO.Positive_Count;
                        CurBlkNum : in out Natural;
                        Blk : in out Card_Block;
                        CardNum : in Positive;
                        Card    : out  String)
   is
	 BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
         BlkNum : Positive;
         CardNumInBlk : Positive;
         BlkNumIndex : SIO.Positive_Count;
   begin
         BlkNum := 1 + (CardNum - 1) / 36; 
         CardNumInBlk := CardNum - (BlkNum - 1) * 36; 
    
         if(BlkNum /= CurBlkNUm)
         then
                        -- FIXME BEGIN only this section depends on SIO. file access
                        -- make it Read_Block(SIO.File, FileBlkNum, Blk)
                        -- where FileBlkNum := HStart + BlkNum
                        -- BlkNum - relative to HDU start
                        -- FileBlkNum - relative to File start
               BlkNumIndex := SIO.Positive_Count( Positive(HStart) + (BlkNum-1) 
                                                * Positive(BlockSize_SIOunits) );

               SIO.Set_Index(File, BlkNumIndex);
               Card_Block'Read(SIO.Stream(File), Blk);
               CurBlkNum := BlkNum;
               -- FIXME END   only this section depends on SIO. file access
         end if;

         Card := Blk(CardNumInBlk);

   end Read_Card;







   function  Get (FitsFile : in SIO.File_Type) return HDU_Info_Type
   is
		HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);	
                CardNum : Natural;
                Card : String(1..80);

                CurBlkNum : Natural := 0; -- none read yet
                Blk : Card_Block;
   begin
                CardNum := Strict.Reset_State;
                loop
                        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Strict.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

                declare
                        PSize   : Strict.Result_Rec := Strict.Get;
                        HDUInfo : HDU_Info_Type(PSize.NAXIS_Last);
                begin
                        -- convert rec
    
                        HDUInfo.XTENSION := Max20.To_Bounded_String(
					Strict.HDU_Type'Image(Psize.HDU));
					-- FIXME PSize.HSU is enum: 20 length might not be enough
					-- find other solution then Bounded_String??
                        HDUInfo.CardsCnt := FPositive(PSize.CardsCount); --FPositive
                        HDUInfo.BITPIX   := PSize.BITPIX;--Integer; 

			-- FIXME unify types:   HDUInfo.NAXISn := PSize.NAXISArr;
                        for I in HDUInfo.NAXISn'Range
                        loop
                                HDUInfo.NAXISn(I) := FInteger(PSize.NAXISArr(I));
                        end loop;

                        return HDUInfo;
                end;

   end Get;

   --
   -- Set file index to HDU start given by HDUNum
   --



        function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in Positive) 
                return Positive
        is
                HUSize : Positive;
        begin 
                HUSize := Positive(1 + (CardsCount - 1)/36);

                return HUSize;
        end Calc_HeaderUnit_Size_blocks;


        -- FIXME consider funcs depending on variable record, put where the variable record is
        -- they always must check presence of the variable fields and 
        -- call external funcs accordingly
function  Calc_DataUnit_Size_blocks  
                (Res : in Strict.Result_Rec) return KW.FNatural
is
	Size_bits : KW.FNatural;

	BitsPerBlock : constant KW.FPositive := (2880*8);
		-- FIXME generic FITS-constant: move elsewhere
begin
	case(Res.HDU) is
	when Strict.NO_DATA =>
		Size_bits := 0;

	when Strict.IMAGE =>
		Size_bits := Formulas.PrimaryImage_DataSize_Bits(Res.BITPIX, Res.NAXISArr);

	when Strict.RANDOM_GROUPS =>
		Size_bits := Formulas.RandomGroups_DataSize_bits
                 		(Res.BITPIX, Res.NAXISArr,
                  		Res.PCOUNT, Res.GCOUNT);

	when Strict.CONFORMING_EXTENSION .. Strict.STANDARD_BINTABLE =>
		Size_bits := Formulas.ConformingExtension_DataSize_bits
                 		(Res.BITPIX, Res.NAXISArr,
                  		Res.PCOUNT, Res.GCOUNT);
	end case;

	return (1 + (Size_bits - 1) / BitsPerBlock); 
		-- FIXME consider separate func for bits -> blocks

end  Calc_DataUnit_Size_blocks;






-- read info help in Mandatory keys of the Header
  function  Get_Mandatory (FitsFile : in SIO.File_Type) return Strict.Result_Rec
  is
		HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);	
                CardNum : Natural;
                Card : String(1..80);

                CurBlkNum : Natural := 0; -- none read yet
                Blk : Card_Block;
   begin
                CardNum := Strict.Reset_State;
                loop
                        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Strict.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

                declare
                        PSize : Strict.Result_Rec := Strict.Get;
                begin
			return PSize;
                end;

   end Get_Mandatory;


	
-- these two replace size calc funcs in .Misc subpackage Copy_HDU()
   function  DU_Size_blocks (FitsFile : in SIO.File_Type) return Positive
   is 
	PSize : Strict.Result_Rec := Get_Mandatory(FitsFile);
   begin
        return Positive(Calc_DataUnit_Size_blocks(PSize)); -- FIXME down-conversion
   end DU_Size_blocks;

  function  HDU_Size_blocks (FitsFile : in SIO.File_Type) return Positive
   is 
	PSize : Strict.Result_Rec := Get_Mandatory(FitsFile);
   begin
        return Calc_HeaderUnit_Size_blocks(PSize.CardsCount)
               + Positive(Calc_DataUnit_Size_blocks(PSize)); -- FIXME down-conversion
   end HDU_Size_blocks;




procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is
        package TIO renames Ada.Text_IO;

        BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
    
        Card : String(1..80);
        CurHDUNum : Positive;
        HeaderStart : SIO.Positive_Count;
        Blk : Card_Block;
        HDUSize_blocks : KW.FPositive;
        CardNum : Natural;
        CurBlkNum : Natural := 0; -- none read yet
begin
        HeaderStart := 1;
        SIO.Set_Index(File,HeaderStart);

        CurHDUNum := 1;

        if(CurHDUNum = HDUNum) then
                return;
        end if;

        while ( CurHDUNum < HDUNum )
        loop
                TIO.New_Line;

                -- Read Header
    
                CardNum := Strict.Reset_State;
                loop
                        Read_Card(File, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Strict.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

               declare
                        PSize : Strict.Result_Rec := Strict.Get;
                begin
 			TIO.New_Line;TIO.Put_Line("DBG> HDU_Type: " 
                                                & Strict.HDU_Type'Image(PSize.HDU));

                 	HDUSize_blocks := KW.FPositive(Calc_HeaderUnit_Size_blocks(PSize.CardsCount))
					+ Calc_DataUnit_Size_blocks(PSize); 
					-- FIXME conversion for HeaderUnit
                end;

                TIO.Put_Line("DBG> HDUSize [blocks]: "
                                        & KW.FPositive'Image(HDUSize_blocks));

                -- move to next HDU

                HeaderStart := HeaderStart
                                + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
                TIO.New_Line;
                TIO.Put_Line("DBG> Next ExtHeaderStart: " & SIO.Positive_Count'Image(HeaderStart));

                SIO.Set_Index(File, HeaderStart);

                CurHDUNum := CurHDUNum + 1;

        end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;




end File;

