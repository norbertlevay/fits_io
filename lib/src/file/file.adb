
with Ada.Text_IO;


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

with Mandatory;
with File_Funcs;
with Keyword_Record;  use Keyword_Record;


package body File is

    package TIO renames Ada.Text_IO;
    package KW renames Keyword_Record;
   
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


-- read info help in Mandatory keys of the Header
  function  Read_Header (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec
  is
        HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
                CardNum : Natural;
                Card : String(1..80);

                CurBlkNum : Natural := 0; -- none read yet
                Blk : Card_Block;
   begin
                CardNum := Mandatory.Reset_State;
                loop
                        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Mandatory.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

                declare
                        PSize : Mandatory.Result_Rec := Mandatory.Get;
                begin
            return PSize;
                end;

   end Read_Header;






   function  Read_Header (FitsFile : in SIO.File_Type) return HDU_Info_Type
   is
    PSize   : Mandatory.Result_Rec := Read_Header(FitsFile);
    HDUInfo : HDU_Info_Type(PSize.NAXIS_Last);
   begin
    HDUInfo.XTENSION := Max20.To_Bounded_String(
                    Mandatory.HDU_Type'Image(Psize.HDU));
                    -- FIXME PSize.HSU is enum: 20 length might not be enough
                    -- find other solution then Bounded_String??
    HDUInfo.CardsCnt := Positive_Count(PSize.CardsCount); --Positive_Count
    HDUInfo.BITPIX   := PSize.BITPIX;--Integer; 

    -- FIXME unify types:   HDUInfo.NAXISn := PSize.NAXISn;
        for I in HDUInfo.NAXISn'Range
        loop
            HDUInfo.NAXISn(I) := PSize.NAXISn(I);
        end loop;

        return HDUInfo;

   end Read_Header;




   function  Read_Header (FitsFile : in  SIO.File_Type;
                       Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr
   is
    HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
        CardNum : Natural;
        Card : String(1..80);

    CurBlkNum : Natural := 0; -- none read yet
    Blk : Card_Block;
   begin
        CardNum := Optional.Init(Keys);
        loop
        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                CardNum := Optional.Next(CardNum, Card);
                exit when (CardNum = 0); 
        end loop;


    declare
        Cards : Card_Arr := Optional.Get_Cards;
    begin
        return Cards;
    end;

   end Read_Header;





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
                (Res : in Mandatory.Result_Rec) return SIO.Count
is
    Size_bits : SIO.Count;

    BitsPerBlock : constant Positive_Count := (2880*8);
        -- FIXME generic FITS-constant: move elsewhere
begin
    case(Res.HDU) is
    when Mandatory.NO_DATA =>
        Size_bits := 0;

    when Mandatory.IMAGE =>
        Size_bits := File_Funcs.PrimaryImage_DataSize_Bits(Res.BITPIX, Res.NAXISn);

    when Mandatory.RANDOM_GROUPS =>
        Size_bits := File_Funcs.RandomGroups_DataSize_bits
                        (Res.BITPIX, Res.NAXISn,
                        Res.PCOUNT, Res.GCOUNT);

    when Mandatory.CONFORMING_EXTENSION .. Mandatory.STANDARD_BINTABLE =>
        Size_bits := File_Funcs.ConformingExtension_DataSize_bits
                        (Res.BITPIX, Res.NAXISn,
                        Res.PCOUNT, Res.GCOUNT);
    end case;

    return (1 + (Size_bits - 1) / BitsPerBlock); 
        -- FIXME consider separate func for bits -> blocks

end  Calc_DataUnit_Size_blocks;





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
        HDUSize_blocks : Positive_Count;
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
    
                CardNum := Mandatory.Reset_State;
                loop
                        Read_Card(File, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Mandatory.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

               declare
                        PSize : Mandatory.Result_Rec := Mandatory.Get;
                begin
            TIO.New_Line;TIO.Put_Line("DBG> HDU_Type: " 
                                                & Mandatory.HDU_Type'Image(PSize.HDU));

                    HDUSize_blocks := Positive_Count(Calc_HeaderUnit_Size_blocks(PSize.CardsCount))
                    + Calc_DataUnit_Size_blocks(PSize); 
                    -- FIXME conversion for HeaderUnit
                end;

                TIO.Put_Line("DBG> HDUSize [blocks]: "
                                        & Positive_Count'Image(HDUSize_blocks));

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

