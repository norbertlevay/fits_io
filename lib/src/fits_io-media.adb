
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS_IO;

with FITS_IO.Header; use FITS_IO.Header;

package body FITS_IO.Media is

  ---------------------------
  -- BEGIN Parser generic part

  -- FIXME add parsed-all boolean return value for early parsing exit
  -- FIXME consider merging this helper-func into Parse_Header
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

  -- END Parser generic part
  ---------------------------



  function Parse_HDU_Type (Source  : in Source_Type)
    return HDU_Type_Size
  is
    DummyCardsCnt : Positive_Count := Parse_Header(Source, HDUTypeKeys);
    HDUType       : HDU_Type_Size  := To_HDU_Type(HDUTypeKeys);
  begin
     return HDUType;
  end Parse_HDU_Type; 




  function Parse_HDU_Size (Source  : in Source_Type;
                           HDUType : in HDU_Type;
                           NAXIS   : in NAXIS_Type)
    return HDU_Size
  is
    Keys          : Key_Arr        := Gen_Size_Keys(NAXIS); 
    DummyCardsCnt : Positive_Count := Parse_Header(Source, Keys);
    HDUSize       : HDU_Size       := To_HDU_Size(Keys, HDUType, NAXIS);
  begin
    return HDUSize;
  end Parse_HDU_Size;



   -- interface func
   function Read_DUSize_bytes(Source : Source_Type)
     return FITS_IO.Count
    is
     DUSize      : FITS_IO.Count := 0;
     HeaderStart : Index_Type    := Index(Source);
     HDUTypeSize : HDU_Type_Size := Parse_HDU_Type(Source);
    begin

     Set_Index(Source, HeaderStart);
     
     declare
       HDUSize : HDU_Size := Parse_HDU_Size(Source,
                                            HDUTypeSize.HDUType,
                                            HDUTypeSize.NAXIS);
     begin
       DUSize := BlockSize_bytes * Size_blocks (HDUSize);
     end;

     return DUSize;
    end Read_DUSize_bytes;






   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   -- FIXME NOT USED, remove

   -- FIXME this is NOTUSED:
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


  function NOTUSED_Parse_Mandatory_Keys (Source : Source_Type)
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
  end NOTUSED_Parse_Mandatory_Keys;




end FITS_IO.Media;
