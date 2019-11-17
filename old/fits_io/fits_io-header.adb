
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS_IO;


package body FITS_IO.Header is


   function To_Key_Record (Card : in Card_Type)
    return Key_Record
   is
    KR : Key_Record;
   begin
    KR.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
    KR.Value   := Max20.To_Bounded_String(Trim(Card(11..30),Ada.Strings.Both));
    KR.Comment := Max48.To_Bounded_String(Trim(Card(33..80),Ada.Strings.Both));
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




   function Gen_Size_Keys(NAXIS : in NAXIS_Type)
     return Key_Arr 
   is 
    Keys : Key_Arr := 
     Key_Arr'(
      (Max_8.To_Bounded_String("BITPIX"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
      (Max_8.To_Bounded_String("PCOUNT"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
      (Max_8.To_Bounded_String("GCOUNT"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")) 
     ) & Generate_1D_Keys("NAXIS", Positive_Count(1), Positive_Count(NAXIS));
   begin 
    return Keys;
   end Gen_Size_Keys; 




  -- FIXME indexes of Keys const array must match -> use enum
  function To_HDU_Type (Keys : in Key_Arr)
    return HDU_Type_Size
  is
    HDUType : HDU_Type_Size;
    use Max20;
  begin

     HDUType.NAXIS := NAXIS_Type'Value(Max20.To_String(Keys(5).Value));
     
     if (Keys(1).Value = Max20.To_Bounded_String("T")) then 

       -- Primary
     
       if ( Keys(1).Value = Max20.To_Bounded_String("T") AND
            Keys(2).Value = Max20.To_Bounded_String("T") AND
            Keys(3).Value = Max20.To_Bounded_String("0")
          ) then
         HDUType.HDUType := RandomGroups;
       else 
         HDUType.HDUType := PrimaryHeader;
       end if;

     elsif (Keys(1).Value = Max20.To_Bounded_String("F")) then 
       -- FIXME what to do?
       null; 

     else
     
       -- Xtension 

       if (Keys(4).Value = Max20.To_Bounded_String("IMAGE")) then 
         HDUType.HDUType := Image;
       elsif (Keys(4).Value = Max20.To_Bounded_String("TABLE")) then 
         HDUType.HDUType := AsciiTable;
       elsif (Keys(4).Value = Max20.To_Bounded_String("BINTABLE")) then 
         HDUType.HDUType := BinaryTable;
       end if;
     
     end if;

     return HDUType;
  end To_HDU_Type; 


  -- FIXME indexes of Keys const array must match -> use enum
  function To_HDU_Size (Keys    : in Key_Arr;
                        HDUType : in HDU_Type;
                        NAXIS   : in NAXIS_Type)
    return HDU_Size
  is
    HDUSize : HDU_Size(NAXIS);
  begin

     HDUSize.BITPIX := Integer'Value(Max20.To_String(Keys(1).Value));
     if ( HDUType = PrimaryHeader ) then
       HDUSize.PCOUNT := 0;
       HDUSize.GCOUNT := 1;
     else
       HDUSize.PCOUNT := FITS_IO.Count'Value(Max20.To_String(Keys(2).Value));
       HDUSize.GCOUNT := FITS_IO.Count'Value(Max20.To_String(Keys(3).Value));
     end if;

     for I in HDUSize.NAXISn'Range 
     loop
       Put_Line("DBG " & Max20.To_String(Keys(FITS_IO.Positive_Count(I+3)).Value) );
       HDUSize.NAXISn(I) := FITS_IO.Count'Value(Max20.To_String(Keys(FITS_IO.Positive_Count(I+3)).Value));
     end loop;
 
     if ( HDUType = RandomGroups ) then
       HDUSize.NAXISn(1) := 1; -- enables to use the same formula for Nbits
     end if;

    return HDUSize;
  end To_HDU_Size;


   --
   -- calculate DU-size
   --


   -- calculate DataUnit size in FITS Blocks
   -- implements Eq(1), (2) and (4) from [FITS]

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
     -- FIXME the above is handled in Parse_Type_Size; use 'Range instead From

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





end FITS_IO.Header;
