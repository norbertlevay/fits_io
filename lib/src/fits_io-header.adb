
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS_IO;


package body FITS_IO.Header is

   -- Header cards / key-records decl

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
    KR.Value   := Max20.To_Bounded_String(Trim(Card(11..30),Ada.Strings.Both));
    -- FIXME KR.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));
    return KR;
   end To_Key_Record;


   function To_Comment_Key_Record (Card : in Card_Type)
    return Comment_Key_Record
   is
    KR : Comment_Key_Record;
   begin
    KR.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
    KR.Comment := Max70.To_Bounded_String(Trim(Card(11..80),Ada.Strings.Both));
    return KR;
   end To_Comment_Key_Record;

   type Key_Arr is array (Positive_Count range <>) of Key_Record;



   --
   -- Parser for Header-cards
   --

  ---------------------------
  -- BEGIN Parser generic part

  -- FIXME add parsed-all boolean return value for early parsing exit
  procedure Parse_Keys(CurKey : in Key_Record;
                       Keys   : in out Key_Arr)
  is
   use Max_8;
  begin
   for I in Keys'Range
   loop
    if(Keys(I).Name = CurKey.Name) then
       Ada.Text_IO.Put_Line("DBG: Parse_Keys: " & Max_8.To_String(CurKey.Name) & " " & Max20.To_String(CurKey.Value) & " " & Max48.To_String(CurKey.Comment));
       Keys(I).Value   := CurKey.Value;
       Keys(I).Comment := CurKey.Comment;
    end if;
   end loop;
  end Parse_Keys;



 function Parse_Header(Source : in Source_Type;
                       Keys   : in out Key_Arr)
    return FITS_IO.Positive_Count
  is
   HBlk          : Card_Block;
   Card          : Card_Type;
   CardsCnt      : FITS_IO.Positive_Count := 1;
   ENDCardFound  : Boolean := False;
   KeyRec        : Key_Record;
  begin
   loop
     HBlk := Next(Source);
     for I in HBlk'Range
     loop
       Card := HBlk(I);
       KeyRec := To_Key_Record(Card);
       Parse_Keys(KeyRec, Keys);
       ENDCardFound  := (Card = ENDCard);
       exit when ENDCardFound;
       CardsCnt := CardsCnt + 1;
     end loop;
     exit when ENDCardFound;
   end loop;
   return CardsCnt;
  end Parse_Header;



  function Generate_1D_Keys(Name : in String;
                            Min  : in Positive_Count;
                            Max  : in Positive_Count)
    return Key_Arr
  is
   Keys : Key_Arr(Min .. Max);
   use Max_8;
  begin
   for I in Min .. Max
   loop
     Keys(I).Name := Max_8.To_Bounded_String(Name) & Trim(Positive_Count'Image(I),Ada.Strings.Left);
   end loop;
   return Keys;
  end Generate_1D_Keys;

  -- END Parser generic part
  ---------------------------




   -- Parser for DU-size e.g. the Mandatory Keys

   -- DU size decl

   type NAXIS_Arr is array (NAXIS_Type range <>) of FITS_IO.Count;

   type HDU_Size_Type(NAXIS : NAXIS_Type) is 
   record
      CardsCnt      : FITS_IO.Positive_Count; 
      -- HDU type
      SIMPLE   : Max20.Bounded_String;
      XTENSION : Max20.Bounded_String;
      -- Primary HDU:
      BITPIX : Integer;       
      NAXISn : NAXIS_Arr(1 .. NAXIS); 
      -- Conforming extensions:
      PCOUNT : FITS_IO.Count; 
      GCOUNT : FITS_IO.Positive_Count;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;
   
   type HDU_Type_Size is 
   record
      HDUType  : HDU_Type;
      NAXIS    : NAXIS_Type;
   end record;

   type HDU_Size(NAXIS : NAXIS_Type) is 
   record
      -- Primary HDU:
      BITPIX : Integer;       
      NAXISn : NAXIS_Arr(1 .. NAXIS); 
      -- Conforming extensions:
      PCOUNT : FITS_IO.Count; 
      GCOUNT : FITS_IO.Positive_Count;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;


  function Parse_HDU_Type (Source  : in Source_Type)
    return HDU_Type_Size
  is
    Keys2 : Key_Arr := (
    (Max_8.To_Bounded_String("SIMPLE"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("GROUPS"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("NAXIS1"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
    (Max_8.To_Bounded_String("XTENSION"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
    (Max_8.To_Bounded_String("NAXIS"),    Max20.To_Bounded_String(""), Max48.To_Bounded_String("")) );

    HDUType : HDU_Type_Size;
    DummyCardsCnt : Positive_Count;
    use Max20;
  begin

     DummyCardsCnt := Parse_Header(Source, Keys2);

     HDUType.NAXIS := NAXIS_Type'Value(Max20.To_String(Keys2(5).Value));
     
     if (Keys2(1).Value = Max20.To_Bounded_String("T")) then 

       -- Primary
     
       if ( Keys2(1).Value = Max20.To_Bounded_String("T") AND
            Keys2(2).Value = Max20.To_Bounded_String("T") AND
            Keys2(3).Value = Max20.To_Bounded_String("0")
          ) then
         HDUType.HDUType := RandomGroups;
       else 
         HDUType.HDUType := PrimaryHeader;
       end if;

     elsif (Keys2(1).Value = Max20.To_Bounded_String("F")) then 
       -- FIXME what to do?
       null; 

     else
     
       -- Xtension 

       if (Keys2(4).Value = Max20.To_Bounded_String("IMAGE")) then 
         HDUType.HDUType := Image;
       elsif (Keys2(4).Value = Max20.To_Bounded_String("TABLE")) then 
         HDUType.HDUType := AsciiTable;
       elsif (Keys2(4).Value = Max20.To_Bounded_String("BINTABLE")) then 
         HDUType.HDUType := BinaryTable;
       end if;
     
     end if;

     return HDUType;
  end Parse_HDU_Type; 



  function Parse_Size (Source  : in Source_Type;
                       HDUType : in HDU_Type;
                       NAXIS   : in NAXIS_Type)
    return HDU_Size
  is
    Keys2 : Key_Arr := 
     Key_Arr'(
      (Max_8.To_Bounded_String("BITPIX"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
      (Max_8.To_Bounded_String("PCOUNT"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
      (Max_8.To_Bounded_String("GCOUNT"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")) 
     ) & Generate_1D_Keys("NAXIS", Positive_Count(1), Positive_Count(NAXIS));

    HDUSize : HDU_Size(NAXIS);
    DummyCardsCnt : Positive_Count;
  begin

     DummyCardsCnt := Parse_Header(Source, Keys2);

     HDUSize.BITPIX := Integer'Value(Max20.To_String(Keys2(1).Value));
     if ( HDUType = PrimaryHeader ) then
       HDUSize.PCOUNT := 0;
       HDUSize.GCOUNT := 1;
     else
       HDUSize.PCOUNT := FITS_IO.Count'Value(Max20.To_String(Keys2(2).Value));
       HDUSize.GCOUNT := FITS_IO.Count'Value(Max20.To_String(Keys2(3).Value));
     end if;

     for I in HDUSize.NAXISn'Range 
     loop
       Put_Line("DBG " & Max20.To_String(Keys2(FITS_IO.Positive_Count(I+3)).Value) );
       HDUSize.NAXISn(I) := FITS_IO.Count'Value(Max20.To_String(Keys2(FITS_IO.Positive_Count(I+3)).Value));
     end loop;
 
     if ( HDUType = RandomGroups ) then
       HDUSize.NAXISn(1) := 1; -- enables to use the same formula for Nbits
     end if;

    return HDUSize;
  end Parse_Size;






  function Parse_Mandatory_Keys (Source : Source_Type)
    return HDU_Size_Type
  is
    Keys1 : Key_Arr := (
     (Max_8.To_Bounded_String("SIMPLE"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
     (Max_8.To_Bounded_String("XTENSION"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
     (Max_8.To_Bounded_String("BITPIX"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
     (Max_8.To_Bounded_String("NAXIS"),    Max20.To_Bounded_String(""), Max48.To_Bounded_String(""))
    );
    --CardsCnt : Positive;
    NAXIS : NAXIS_Type;
    HeaderStart : Index_Type := Index(Source);
    DummyCC : FITS_IO.Positive_Count; -- FIXME
  begin

   DummyCC := Parse_Header(Source, Keys1);

   NAXIS := NAXIS_Type'Value(Max20.To_String(Keys1(4).Value));

   declare
     Keys2   : Key_Arr := Generate_1D_Keys("NAXIS", Positive_Count(1), Positive_Count(NAXIS));
     HDUSize : HDU_Size_Type(NAXIS);
   begin
     
     for I in Keys2'Range 
     loop
       Ada.Text_IO.Put_Line("Keys2: " & Max_8.To_String(Keys2(I).Name) & " " & Max20.To_String(Keys2(I).Value));
     end loop;

     HDUSize.BITPIX := Integer'Value(Max20.To_String(Keys1(3).Value));
     
     Set_Index(Source, HeaderStart);
     HDUSize.CardsCnt := Parse_Header(Source, Keys2);
     
     for I in HDUSize.NAXISn'Range 
     loop
       HDUSize.NAXISn(I) := FITS_IO.Count'Value(Max20.To_String(Keys2(FITS_IO.Positive_Count(I)).Value));
     end loop;
 
     return HDUSize;
   end;
  end Parse_Mandatory_Keys;



   --
   -- calculate DU-size
   --

   -- FIXME Where to put these ...
   BlockSize_bits : constant FITS_IO.Positive_Count := 2880 * Byte'Size; -- 23040 bits
   StreamElemSize_bits : FITS_IO.Positive_Count := Ada.Streams.Stream_Element'Size;
   BlockSize_bytes : FITS_IO.Positive_Count := BlockSize_bits / StreamElemSize_bits;

   -- calculate DataUnit size in FITS Blocks
   
   -- implements Eq(1), (2) and (4) from [FITS]
   -- FIXME However we should parse other keys (SIMPLE, XTENSION, GROUPS) to
--   function Size_blocks (HDUSize : in HDU_Size_Type)
   function Size_blocks (HDUSize : in HDU_Size)
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
     --DUSize := DUSize + HDUSize.PCOUNT;
     --DUSize := DUSize * HDUSize.GCOUNT;

     DataInBlock := BlockSize_bits /  FITS_IO.Count( abs HDUSize.BITPIX );
     -- per FITS standard, these values are integer multiples (no remainder)

     DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

    return DUSizeInBlocks;
   end Size_blocks;



   --
   -- interface func
   --

   function Read_DUSize_bytes(Source : Source_Type)
             return FITS_IO.Count
    is
    -- package SIO renames Ada.Streams.Stream_IO;

     DUSize  : FITS_IO.Count := 0;
--     HDUSize : HDU_Size_Type := Parse_Mandatory_Keys(Source);
     --ccnt : Natural;
     HeaderStart : Index_Type := Index(Source);
     HDUTypeSize : HDU_Type_Size := Parse_HDU_Type(Source);
    begin

     -- 1 parse data for size
--     ccnt := Parse_Header(Source,HDUSize);
   Set_Index(Source,HeaderStart);
   
   declare
     newHDUSize : HDU_Size := Parse_Size(Source,
                                      HDUTypeSize.HDUType,
                                      HDUTypeSize.NAXIS);
    begin
     DUSize := BlockSize_bytes * Size_blocks (newHDUSize);
    end;


     -- 2 calc the size
     -- what HDU_Type we read?
     -- depending on HDU, call different size-calculators
     --DUSize := BlockSize_bytes * Size_blocks (newHDUSize);

     return DUSize;
    end Read_DUSize_bytes;

end FITS_IO.Header;
