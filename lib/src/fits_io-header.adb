
with Ada.Streams;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS_IO;


package body FITS_IO.Header is

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);


   type Key_Record is
    record
	Name    : Max_8.Bounded_String;
	Value   : Max20.Bounded_String;
	Comment : Max48.Bounded_String;
    end record;

   type Comment_Key_Record is
    record
	Name    : Max_8.Bounded_String;
	Comment : Max70.Bounded_String;
    end record;

   function To_Key_Record (Card : in Card_Type)
    return Key_Record
   is
    KR : Key_Record;
   begin
    KR.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
    KR.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
    KR.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));
    return KR;
   end To_Key_Record;

   function To_Comment_Key_Record (Card : in Card_Type)
    return Comment_Key_Record
   is
    KR : Comment_Key_Record;
   begin
    KR.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
    KR.Comment := Max70.To_Bounded_String(Trim(Card(10..80),Ada.Strings.Both));
    return KR;
   end To_Comment_Key_Record;

  -- Parse scalar keys

  type Key_Arr is array (Positive_Count range <>) of Key_Record;

  -- FIXME add parsed-all boolean return value for early parsing exit
  procedure Parse_Keys(CurKey : in Key_Record;
  		       Keys   : in out Key_Arr)
  is
   use Max_8;
  begin
   for I in Keys'Range
   loop
    if(Keys(I).Name = CurKey.Name) then
       Keys(I).Value   := CurKey.Value;
       Keys(I).Comment := CurKey.Comment;
    end if;
   end loop;
  end Parse_Keys;

  function Parse_Header(Source : in Source_Type;
                        Keys   : in out Key_Arr)
    return Positive
  is
   HBlk          : Card_Block;
   Card          : Card_Type;
   CardsCnt      : Natural := 0;
   ENDCardFound  : Boolean := False;
   KeyRec        : Key_Record;
  begin
   loop
     -- [FITS] every valid FITS File must have at least one block

     HBlk := Next(Source);
     for I in HBlk'Range
     loop
       Card := HBlk(I);
       CardsCnt := CardsCnt + 1;

       KeyRec := To_Key_Record(Card);

       Parse_Keys(KeyRec, Keys);

       ENDCardFound  := (Card = ENDCard);
       exit when ENDCardFound;
     end loop;

     exit when ENDCardFound;
   end loop;
   return CardsCnt;
  end Parse_Header;


  procedure Parse_HDU_Size_Type (Keys : out Key_Arr)
  is
  begin
    -- parsing sequence foir fixed arr of keys needed to calc DUSize
   null;
  end Parse_HDU_Size_Type;



   -- Parse HDU size

   type NAXIS999_Type is array (1 .. NAXIS_Type'Last) of FITS_IO.Count;

   type HDU_Size_Type is record
      CardsCnt      : FITS_IO.Positive_Count; -- number of cards in this Header (gives Header-size)

      -- HDU type
      SIMPLE   : Character;--String(1..1);
      XTENSION : Max20.Bounded_String;

      -- Primary HDU:
      BITPIX : Integer;       -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;    -- NAXIS  from header, 0 means no DataUnit
      NAXISn : NAXIS999_Type; -- NAXISn from header, 0 means dimension not in use

      -- Conforming extensions:
      PCOUNT : FITS_IO.Count;    -- BINTABLE: size of heap OR Random Groups: param count preceding each group
      GCOUNT : FITS_IO.Positive_Count;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;

   -- DU_Size_Type collects keyword values which define DataUnit size
   -- parse from Card value if it is one of DU_Size_Type, do nothing otherwise
   -- and store parse value to DUSizeKeyVals
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   -- Size calc is valid also for IMAGE-extension, but not for TABLE extensions
   -- FIXME should check if it is IMAGE extension [FITS, Sect 7]
   -- FIXME parse NAXISnnn to an array(1...999) allocated on heap
   -- when all parsing done generate NAXISn(1..NAXIS)
   -- Implement as: generic must have User_Area_Type private
   -- which is passed to Parse_Card(...,UA:User_Area_Type) and also
   -- (gen_)Read_Header(...,UA:User_Area_Type)
   -- and UserArea must be passed as param to Parse_Card
   procedure Parse_HDU_Size_Type (KeyRec  : in Key_Record;
                                  HDUSize : in out HDU_Size_Type)
   is
    Name  : constant String := Max_8.To_String(KeyRec.Name);
    Value : constant String := Max20.To_String(KeyRec.Value);
    dim   : NAXIS_Type;
   begin

     if    (Name = "SIMPLE") then

       HDUSize.SIMPLE := Character(Value(1));
       -- FIXME need to check Value-length

     elsif (Name = "XTENSION") then

       HDUSize.XTENSION := KeyRec.Value;

     elsif (Name = "BITPIX") then

       HDUSize.BITPIX := Integer'Value(Value);

     elsif (Name = "NAXIS") then


       if (Name = "NAXIS") then
           if 0 = Natural'Value(Value)
           then
             -- no data unit in this HDU
             -- FIXME [FITS 4.4.1.1 Primary Header] "A value of zero signifies
             -- that no data follow the header in the HDU."
             null;
           else
             HDUSize.NAXIS := NAXIS_Type'Value(Value);
           end if;
       else
           dim := Positive'Value(Name(6..8));
           HDUSize.NAXISn(dim) := FITS_IO.Count'Value(Value);
       end if;


     elsif (Name = "PCOUNT") then

       HDUSize.PCOUNT := FITS_IO.Count'Value(Value);

     elsif (Name = "GCOUNT") then

       HDUSize.GCOUNT := FITS_IO.Positive_Count'Value(Value);

     end if;

    -- FIXME this is no good
    HDUSize.PCOUNT := 0;
    HDUSize.GCOUNT := 1;
     -- init these for HDU's which do not use them
     -- BINTABLE and RandomGroup extensions, if present,
     -- will overwrite these values

   end Parse_HDU_Size_Type;


--   generic
--     type Parsed_Type is (<>);
--     with procedure Parse(Card : in Card_Type;
--                         Data : in out Parsed_Type)
   function Parse_Header(Source : in Source_Type;
                         Data   : in out HDU_Size_Type)
     return Positive
   is
    HBlk          : Card_Block;
    Card          : Card_Type;
    CardsCnt      : Natural := 0;
    ENDCardFound  : Boolean := False;
    KeyRec        : Key_Record;
   begin
    loop
      -- [FITS] every valid FITS File must have at least one block

      HBlk := Next(Source);
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        CardsCnt := CardsCnt + 1;

        KeyRec := To_Key_Record(Card);

	Parse_HDU_Size_Type(KeyRec, Data);

        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound;
      end loop;

      exit when ENDCardFound;
    end loop;
    return CardsCnt;
   end Parse_Header;



   --
   -- calculate DataUnit size in FITS Blocks
   --
   -- implements Eq(1), (2) and (4) from [FITS]
   -- However we should parse other keys (SIMPLE, XTENSION, GROUPS) to
   -- establish HDU type - FIXME what strategy to take here ?
   function Size_blocks (HDUSize : in HDU_Size_Type)
     return FITS_IO.Positive_Count
   is
    DataInBlock    : FITS_IO.Positive_Count;
    DUSizeInBlocks : FITS_IO.Positive_Count;
    DUSize         : FITS_IO.Positive_Count := 1;
    From : Positive := 1;
    BlockSize_bits : constant FITS_IO.Positive_Count := 2880 * Byte'Size; -- 23040 bits
   begin

     -- if HDU is RandomGroup NAXIS1=0 and NAXIS1 is not part of size
     -- calculations [FITS Sect 6, Eq.(4)]
     if HDUSize.NAXISn(1) = 0 then
      From := 2;
     end if;

     for I in From..HDUSize.NAXIS
     loop
      DUSize := DUSize * HDUSize.NAXISn(I);
     end loop;
      -- DUSize cannot be 0: Naxis(I) is FITS_IO.Positive_Count
      -- cannot be 0 (parsing would throw exception)

     -- Conforming extensions (or 0 and 1 for Primary Header):
     DUSize := DUSize + HDUSize.PCOUNT;
     DUSize := DUSize * HDUSize.GCOUNT;

     DataInBlock := BlockSize_bits /  FITS_IO.Count( abs HDUSize.BITPIX );
     -- per FITS standard, these values are integer multiples (no remainder)

     DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

    return DUSizeInBlocks;
   end Size_blocks;
   pragma Inline (Size_blocks);



   -- Start FITS.File body

   BlockSize_bits : constant FITS_IO.Positive_Count := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   ---------------
   -- FITS.File :

   StreamElemSize_bits : FITS_IO.Positive_Count := Ada.Streams.Stream_Element'Size;
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

   BlockSize_bytes : FITS_IO.Positive_Count := BlockSize_bits / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index()



   function Read_DUSize_bytes(Source : Source_Type)
             return FITS_IO.Count
    is
     DUSize  : FITS_IO.Count := 0;
     HDUSize : HDU_Size_Type;
     ccnt : Natural;
    begin

     -- 1 parse data for size
     ccnt := Parse_Header(Source,HDUSize);

     -- 2 calc the size
     -- what HDU_Type we read?
     -- depending pn HDU tye call different size-calculators
     DUSize := BlockSize_bytes * Size_blocks (HDUSize);

     return DUSize;
    end Read_DUSize_bytes;

end FITS_IO.Header;
