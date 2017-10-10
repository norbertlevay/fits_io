--
-- Implementation notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

package body FITS is

   --
   -- calculate Header size in FITS Blocks
   --
   function  Size_blocks (CardsCnt    : in FPositive       ) return FPositive
   is
   begin
    return ( 1 + (CardsCnt - 1)/FPositive(CardsCntInBlock) );
   end Size_blocks;
   pragma Inline (Size_blocks);

   --
   -- calculate DataUnit size in FITS Blocks
   --
   function  Size_blocks (DUSizeParam : in DUSizeParam_Type) return FPositive
   is
    DataInBlock    : FPositive;
    DUSizeInBlocks : FPositive;
    DUSize         : FPositive := 1;
   begin

     for I in 1..DUSizeParam.Naxes
     loop
      DUSize := DUSize * DUSizeParam.Naxis(I);
     end loop;
      -- DUSize cannot be 0: Naxis(I) is FPositive
      -- cannot be 0 (parsing would throw exception)

     DataInBlock := BlockSize_bits /  FNatural( abs DUSizeParam.BITPIX );
     -- per FITS standard, these values are integer multiples (no remainder)

     DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

    return DUSizeInBlocks;
   end Size_blocks;
   pragma Inline (Size_blocks);

   -- calc number of free cards to fill up HeaderBlock
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural
   is
    FreeSlotCnt : Natural := Natural( CardsCnt mod FPositive(CardsCntInBlock) );
    -- explicit conversion ok: mod < CardsCntInBlock = 36;
   begin
    if FreeSlotCnt /= 0 then
      FreeSlotCnt := CardsCntInBlock - FreeSlotCnt;
    end if;
    return FreeSlotCnt;
   end Free_Card_Slots;
   pragma Inline (Free_Card_Slots);

   --
   -- convert BITPIX keyword from Header to internal FitsData_Type
   --
   function  To_FitsDataType (BITPIX : in Integer ) return FitsData_Type
   is
    bp : FitsData_Type;
   begin
    case BITPIX is
    when   8 => bp := Int8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME ? raise exception "out of range"
    end case;
    return bp;
   end To_FITSDataType;

   -- parse from Card value if it is one of DUSizeParam_Type, do nothng otherwise
   -- and store parse value to DUSizeParam
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   procedure Parse_Card (Card        : in Card_Type;
                         DUSizeParam : in out DUSizeParam_Type)
   is
    dim : Positive;
   begin
     -- FIXME what if parsed string is '' or '     ' etc...

     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value
     if    (Card(1..9) = "BITPIX  =") then
       DUSizeParam.Data   := To_FitsDataType(Integer'Value(Card(10..30)));
       DUSizeParam.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           DUSizeParam.Naxes := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           DUSizeParam.Naxis(dim) := FPositive'Value(Card(10..30));
           -- FIXME [FITS Section??: NAXISn is always positive and zero]
           -- [FITS fixed integer]:
   	   -- Theoretical problem: Fixed integer is defined as 19 decimal digits
   	   -- (Header Card Integer value occupying columns 11..20)
   	   -- Lon_Long_Integer in GNAT is 64bit: 9.2 x 10**19 whereas
   	   -- fixed integer can reach 9.9 x 10**19)
           -- Conclude: range of NAXISn will be implementation limited.
       end if;

     end if;

   end Parse_Card;

end FITS;

