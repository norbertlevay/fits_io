
with Ada.Text_IO;


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with V3_Types;   use V3_Types; -- Float_32 needed
with Mandatory;
with File; use File;
with File_Funcs;
with Keyword_Record;  use Keyword_Record;


package body Header is

    package TIO renames Ada.Text_IO;
    package KW renames Keyword_Record;


    CardsCntInBlock : constant Positive := 36;
    -- FIXME Card_Block is used in File.Misc::Copy_Blocks & Copy_HDU
    -- find other solution for File.Misc, here move it inside body
    type Card_Block is array (Positive range 1..CardsCntInBlock) of String_80;
    pragma Pack (Card_Block);
    -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed
    -- OR do we need to guarantee at all ?






   --
   -- Read File until ENDCard found
   --

   procedure Read_Card (File : in SIO.File_Type;
            HStart : in SIO.Positive_Count;
                        CurBlkNum : in out SIO.Count;
                        Blk : in out Card_Block;
                        CardNum : in SIO.Positive_Count;
                        Card    : out  String)
   is
         BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
         BlkNum : SIO.Positive_Count;
         CardNumInBlk : Natural;
         BlkNumIndex : SIO.Positive_Count;
   begin
         BlkNum := 1 + (CardNum - 1) / 36; 
         CardNumInBlk := Natural(CardNum - (BlkNum - 1) * 36);-- FIXME is < 36 Natural is ok if two lines together

         if(BlkNum /= CurBlkNUm)
         then
                        -- FIXME BEGIN only this section depends on SIO. file access
                        -- make it Read_Block(SIO.File, FileBlkNum, Blk)
                        -- where FileBlkNum := HStart + BlkNum
                        -- BlkNum - relative to HDU start
                        -- FileBlkNum - relative to File start
               BlkNumIndex :=  HStart + (BlkNum-1) * BlockSize_SIOunits;

               SIO.Set_Index(File, BlkNumIndex);
               Card_Block'Read(SIO.Stream(File), Blk);
               CurBlkNum := BlkNum;
               -- FIXME END   only this section depends on SIO. file access
         end if;

         Card := Blk(CardNumInBlk);

   end Read_Card;


-- read info help in Mandatory keys of the Header
  function  Read_Mandatory (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec
  is
        HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
                CardNum : SIO.Count;
                Card : String(1..80);

                CurBlkNum : SIO.Count := 0; -- none read yet
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

   end Read_Mandatory;


   function  Read_Optional (FitsFile : in  SIO.File_Type;
                       Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr
   is
    HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
        CardNum : SIO.Count;
        Card : String(1..80);

    CurBlkNum : SIO.Count := 0; -- none read yet
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

   end Read_Optional;





function Has_Card(Cards : Card_Arr; Key : String; Value : out String) return Boolean
is
    Found : Boolean := False;
begin
    for I in Cards'Range
    loop
        if(Cards(I)(1..8) = Key(1..8))
        then
            Value := Cards(I)(11..30);
            Found := True;
        end if;
    end loop;
    return Found;
end Has_Card;





   -- from earlier image/image.adb
-- NOTE free-format integer must be right justified ? $
-- Standard ambigous Sect 4.2.3; fitsverify: no complain$
function Create_Card(Key : in String; Value : in String) return String_80
is
    C : String(1 .. 80);
    Val_Len : Positive := Value'Length;
begin
    Move(Key,   C(1  .. 8));
    Move("= ",  C(9  ..10));
 --   Move(Value, C(11 ..(10 + Value'Length)));
    Move(" ",   C(11 .. 80));
    Move(Value, C( (1 + 30 - Value'Length) .. 30 ) );
--    Move(" ",   C((11 + Value'Length) .. 80));
    return C;
end Create_Card;



function Create_Mandatory_Card(Key : in String; Value : in String) return String_80
is
    C : String(1 .. 80);
begin
    Move(Key,   C(1 .. 8));
    Move("= ",  C(9 ..10));
    Move(Value, C(11..30));
    Move(" ",   C(31..80));
    return C;
end Create_Mandatory_Card;



function To_Value_String( V : in Integer) return String
is
    Vstr: String(1 .. 20);
begin
    Move(Integer'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;



function To_Value_String( V : in SIO.Count) return String
is
    Vstr: String(1 .. 20);
begin
    Move(SIO.Count'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;

function To_Value_String( V : in Float_32) return String
is
    Vstr: String(1 .. 20);
begin
    Move(Float_32'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;

function To_Value_String( V : in Boolean) return String
is
    Vstr : String(1 .. 20);
begin
    if(V = True) then
        Move("T", Vstr, Error, Right);
    else
        Move("F", Vstr, Error, Right);
    end if;
    return Vstr;
end To_Value_String;

function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Arr) return Card_Arr
is
    Cards : Card_Arr(1 .. NAXISn'Last);
begin
    for I in NAXISn'Range
    loop
        Cards(I) := Create_Mandatory_Card("NAXIS" & Trim(Integer'Image(I),Left),
                                        To_Value_String(NAXISn(I)));
    end loop;
    return Cards;
end Create_NAXIS_Card_Arr;


function To_Cards( Im : in Image_Rec ) return Card_Arr
is
    Cards : Card_Arr(1 .. (2 + Im.NAXISn'Length));
begin
    Cards(1) := Create_Mandatory_Card("BITPIX",  To_Value_String(Im.BITPIX));
    Cards(2) := Create_Mandatory_Card("NAXIS",   To_Value_String(Im.NAXIS));
    Cards(3 .. (3 + Im.NAXISn'Length) - 1) := Create_NAXIS_Card_Arr(Im.NAXISn);
    return Cards;
end To_Cards;


-- FIXME consider: Create_First_Card be in separate array and written separately,
-- similar to writing END-card at 'closing' _independently_ of what (IMAGE TABLE etc)
-- is being written into it: Primary is tied 
-- to File_Block_Index=1 and Extension File_Block_Index/=1
-- FIXME later consider make this Write-attrib : Image_Rec'Write

function To_Primary_Cards( Im : in Image_Rec ) return Card_Arr
is
    ImCardsCnt : Positive := 2 + Im.NAXISn'Length;
    Cards : Card_Arr(1 .. (1 + ImCardsCnt));
begin
    Cards(1) := Create_Mandatory_Card("SIMPLE",  To_Value_String(True));
    Cards(2 .. (1 + ImCardsCnt)) := To_Cards(Im);
    return Cards;
end To_Primary_Cards;


function To_Extension_Cards( Im : in Image_Rec ) return Card_Arr
is
    ImCardsCnt : Positive := 2 + Im.NAXISn'Length;
    Cards  : Card_Arr(1 .. (1 + ImCardsCnt + 2 + 1));
    ImLast : Positive := ImCardsCnt;
begin
    Cards(1) := Create_Mandatory_Card("XTENSION",  "'IMAGE   '");
    Cards(2 .. ImLast) := To_Cards(Im);
    Cards(ImLast+1) := Create_Mandatory_Card("PCOUNT", To_Value_String(SIO.Count(0)));
    Cards(ImLast+2) := Create_Mandatory_Card("GCOUNT", To_Value_String(SIO.Count(1)));
    return Cards;
end To_Extension_Cards;




end Header;

