
with Ada.Text_IO;

with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

with Mandatory;
with Header; use Header;
with File_Funcs;
with Keyword_Record;  use Keyword_Record;

package body File is

    package TIO renames Ada.Text_IO;
    package KW renames Keyword_Record;
   
   --
   -- Read File until ENDCard found
   --





   function  Read_Header (FitsFile : in SIO.File_Type) return HDU_Info_Type
   is
    PSize   : Mandatory.Result_Rec := Read_Mandatory(FitsFile);
    HDUInfo : HDU_Info_Type(PSize.NAXIS_Last);
   begin
    HDUInfo.XTENSION := BS20.To_Bounded_String(
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






   --
   -- Set file index to HDU start given by HDUNum
   --



        function  Calc_HeaderUnit_Size_blocks
                (CardsCount : in Positive_Count) 
                return Positive_Count
        is
                HUSize : Positive_Count;
        begin 
                HUSize := 1 + (CardsCount - 1)/36;

                return HUSize;
        end Calc_HeaderUnit_Size_blocks;


        -- FIXME consider funcs depending on variable record, put where the variable record is
        -- they always must check presence of the variable fields and 
        -- call external funcs accordingly
function  Calc_DataUnit_Size_blocks
                (Res : in Mandatory.Result_Rec) return Count
is
    Size_bits : Count;

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

        BlockSize_SIOunits : constant Positive_Count := 2880;

        CurHDUNum : Positive;
        HeaderStart : Positive_Count;
        HDUSize_blocks : Positive_Count;
        CurBlkNum : Count := 0; -- none read yet
--        use SIO;
begin
        HeaderStart := 1;
        SIO.Set_Index(File, SIO.Count(HeaderStart));-- FIXME cast

        CurHDUNum := 1;

        if(CurHDUNum = HDUNum) then
                return;
        end if;

        while ( CurHDUNum < HDUNum )
        loop
                TIO.New_Line;

                -- Read Header and calc HDU size

                declare
                        PSize : Mandatory.Result_Rec := Read_Mandatory(File);
                begin
                    TIO.New_Line;TIO.Put_Line("DBG> HDU_Type: " 
                                                & Mandatory.HDU_Type'Image(PSize.HDU));

                    HDUSize_blocks := Calc_HeaderUnit_Size_blocks(PSize.CardsCount)
                                    + Calc_DataUnit_Size_blocks(PSize); 
                end;

                TIO.Put_Line("DBG> HDUSize [blocks]: "
                                        & Positive_Count'Image(HDUSize_blocks));

                -- move to next HDU

                HeaderStart := HeaderStart
                                + Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
                TIO.New_Line;
                TIO.Put_Line("DBG> Next ExtHeaderStart: " & Positive_Count'Image(HeaderStart));

                SIO.Set_Index(File, SIO.Count(HeaderStart));--FIXME cast

                CurHDUNum := CurHDUNum + 1;

        end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;


-- NOTE taken from original place data/data_funcs.ad? when data/ deprecated
-- indexing relative to file start
BlockSize_bytes    : constant Positive_Count :=  2880;
BlockSize_bits     : constant Positive_Count := 23040;
BlockSize_sioelems : constant Positive_Count
    := BlockSize_bits / Ada.Streams.Stream_Element'Size;
-- size counted in SIO file-index elements


function File_Block_Index(File : SIO.File_Type) return Positive_Count
is
    SIO_Index : Positive_Count := Index(File);
begin
    return (1 + (SIO_Index - 1) / BlockSize_sioelems);
end File_Block_Index;



procedure Set_File_Block_Index
        (File        : SIO.File_Type; 
         Block_Index : in Positive_Count)
is
    SIO_Index : Positive_Count :=  
        1 + (Block_Index - 1) * BlockSize_sioelems;
begin
    Set_Index(File, SIO_Index);
end Set_File_Block_Index;






end File;

