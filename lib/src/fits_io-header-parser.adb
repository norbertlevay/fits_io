
with Ada.Streams;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS_IO;

separate(FITS_IO.Header);

package body Parser is

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
     Keys(I).Name := Max_8.To_Bounded_String(Name) & Positive_Count'Image(I);
   end loop;
   return Keys;
  end Generate_1D_Keys;

  -- [FITS Table C.3]
  procedure Parse_Mandatory_Keys (Keys : out Key_Arr)
  is
    Source : Source_Type;
    Keys1 : Key_Arr := ((Max_8.To_Bounded_String("SIMPLE"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")),
			(Max_8.To_Bounded_String("XTENSION"), Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
			(Max_8.To_Bounded_String("BITPIX"),   Max20.To_Bounded_String(""), Max48.To_Bounded_String("")), 
			(Max_8.To_Bounded_String("NAXIS"),    Max20.To_Bounded_String(""), Max48.To_Bounded_String("")) 
			);
   CardsCnt : Positive;
   NAXIS : NAXIS_Type;
   -- FIXME  HeaderStart : Positive_Count := Index(Source);
  begin
    -- parsing sequence foir fixed arr of keys needed to calc DUSize
   
   CardsCnt := Parse_Header(Source, Keys1);
   
   NAXIS := NAXIS_Type'Value(Max20.To_String(Keys1(4).Value));

  declare
   Keys2 : Key_Arr := Generate_1D_Keys("NAXIS",Positive_Count(1),Positive_Count(NAXIS));
  begin
   -- FIXME Set_Index(HeaderStart);
   CardsCnt := Parse_Header(Source, Keys2);
   Keys(1 .. Keys1'Last) := Keys1;
   Keys(Keys1'Last+1 .. Keys1'Last+1 + Positive_Count(NAXIS)) := Keys2;
  end;

  end Parse_Mandatory_Keys;


end Parser;
