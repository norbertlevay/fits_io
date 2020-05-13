--
-- Example convert FLoat32 -> Int16
--
-- FIXME: assume Primary HDU. What if other HDU is IMAGE type too?
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Interfaces;

with Ada.Unchecked_Conversion;

with File;   use File;
with File_Funcs;  use File_Funcs;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Header; use Header;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional;
with Optional.Reserved; use Optional.Reserved;


-- new Data interface
with V3_Types; use V3_Types;

with Image_Data;
--with Physical;
with Linear_Conv;

-- new
with Linear_Private;
with Raw;

procedure minmaxraw
is

  package TIO renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;
  use SIO;

  function Get_Int_16
    (Key : in String; 
    Cards : in Optional.Card_Arr) return Integer_16
  is
  begin
    for I in Cards'Range
    loop
      if(Cards(I)(1..Key'Length) = Key)
      then
        return Integer_16'Value(Cards(I)(11..30));
      end if;
    end loop;
    return 0;-- FIXME raise exception "Not_Found"
  end Get_Int_16;


  generic
  type T is digits <>;
  function Get_Float
    (Key : in String; 
    Cards : in Optional.Card_Arr;
    Default_Value : in T) return T;

    function Get_Float
      (Key : in String;
      Cards : in Optional.Card_Arr;
      Default_Value : in T) return T
    is
    begin
      for I in Cards'Range
      loop
        if(Cards(I)(1..Key'Length) = Key)
        then
          TIO.Put_Line(Cards(I)(11..30));
          return T'Value(Cards(I)(11..30));
        end if;
      end loop;
      return Default_Value;
    end Get_Float;

    function F64_Get_Float is new Get_Float(Float_64);
    function F32_Get_Float is new Get_Float(Float_32);



    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

    DUSize  : SIO.Positive_Count;
    DUStart : SIO.Positive_Count;

    PlaneLength :  SIO.Positive_Count;
    NPlanes     :  SIO.Positive_Count;


-- new with Linear_Private
    type Tc is new Float_64;
    type Tm is new Float_64;
    TmUndefOut : Tm := Tm(F64NaN);-- FIXME cannot convert NaN
    A : Tc := Tc(0.0);
    B : Tc := Tc(1.0);
    Undef_Valid : Boolean := False;
    I64BLANK : Integer_64;--FIXME set from Header
    I32BLANK : Integer_32;--FIXME set from Header
    I16BLANK : Integer_16;--FIXME set from Header
    U8BLANK  : Unsigned_8;--FIXME set from Header



    -- instant Linear

  function "+"(R : Tc) return Tm is begin return Tm(R); end "+";

  function "+"(R : Float_64) return Tc is begin return Tc(R); end "+";
  function "+"(R : Float_32) return Tc is begin return Tc(R); end "+";
  function "+"(R : Integer_64) return Tc is begin return Tc(R); end "+";
  function "+"(R : Integer_32) return Tc is begin return Tc(R); end "+";
  function "+"(R : Integer_16) return Tc is begin return Tc(R); end "+";
  function "+"(R : Unsigned_8) return Tc is begin return Tc(R); end "+";

    function LinearFromF64 is new Linear_Private.Linear_From_Floats(Tc,A,B,Float_64,Tm, Undef_Valid,         TmUndefOut,"+","+");
    function LinearFromF32 is new Linear_Private.Linear_From_Floats(Tc,A,B,Float_32,Tm, Undef_Valid,         TmUndefOut,"+","+");
    function LinearFromI64 is new Linear_Private.Linear_From_Ints(Tc,A,B,Integer_64,Tm, Undef_Valid,I64BLANK,TmUndefOut,"+","+");
    function LinearFromI32 is new Linear_Private.Linear_From_Ints(Tc,A,B,Integer_32,Tm, Undef_Valid,I32BLANK,TmUndefOut,"+","+");
    function LinearFromI16 is new Linear_Private.Linear_From_Ints(Tc,A,B,Integer_16,Tm, Undef_Valid,I16BLANK,TmUndefOut,"+","+");
    function LinearFromU8  is new Linear_Private.Linear_From_Ints(Tc,A,B,Unsigned_8,Tm, Undef_Valid,U8BLANK, TmUndefOut,"+","+");

    -- FIXME missing no BLANK case


    -- algorithm
    Max : Tm := Tm'First;
    Min : Tm := Tm'Last;
    Undef_Count : Natural := 0;
    procedure Tm_MinMax(DD : in Tm; Max : in out Tm; Min : in out Tm)
    is
    begin 
        if (DD > Max) then Max := DD; end if;
        if (DD < Min) then Min := DD; end if;
    end Tm_MinMax;

    -- instant Data()

    procedure Data(E : Float_64) is DD: Tm; begin DD := LinearFromF64(E);   Tm_MinMax(DD,Max,Min); end;
    procedure Data(E : Float_32) is DD: Tm; begin DD := LinearFromF32(E);   Tm_MinMax(DD,Max,Min); end;
    procedure Data(E : Integer_64) is DD: Tm; begin DD := LinearFromI64(E); Tm_MinMax(DD,Max,Min); end;
    procedure Data(E : Integer_32) is DD: Tm; begin DD := LinearFromI32(E); Tm_MinMax(DD,Max,Min); end;
    procedure Data(E : Integer_16) is DD: Tm; begin DD := LinearFromI16(E); Tm_MinMax(DD,Max,Min); end;
    procedure Data(E : Unsigned_8) is DD: Tm; begin DD := LinearFromU8(E);  Tm_MinMax(DD,Max,Min); end;

    type F64_Arr is array (SIO.Positive_Count range <>) of Float_64;
    type F32_Arr is array (SIO.Positive_Count range <>) of Float_32;
    type I64_Arr is array (SIO.Positive_Count range <>) of Integer_64;
    type I32_Arr is array (SIO.Positive_Count range <>) of Integer_32;
    type I16_Arr is array (SIO.Positive_Count range <>) of Integer_16;
    type U8_Arr is array (SIO.Positive_Count range <>) of Unsigned_8;


    package F64Raw is new Raw(Float_64, F64_Arr);
    package F32Raw is new Raw(Float_32, F32_Arr);
    package I64Raw is new Raw(Integer_64, I64_Arr);
    package I32Raw is new Raw(Integer_32, I32_Arr);
    package I16Raw is new Raw(Integer_16, I16_Arr);
    package U8Raw is new Raw(Unsigned_8, U8_Arr);

    procedure F64_Read_All is new F64Raw.Read_All(Data);
    procedure F32_Read_All is new F32Raw.Read_All(Data);
    procedure I64_Read_All is new I64Raw.Read_All(Data);
    procedure I32_Read_All is new I32Raw.Read_All(Data);
    procedure I16_Read_All is new I16Raw.Read_All(Data);
    procedure U8_Read_All is new U8Raw.Read_All(Data);

  begin 

    if(Argument_Count /= 1 )
    then 
      Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    declare
      HDUInfo : HDU_Info_Type := Read_Header(InFile);
      BITPIX : Integer := HDUInfo.BITPIX;
      NAXISn : NAXIS_Arr := HDUInfo.NAXISn;
    begin
      DUStart := File_Block_Index(InFile);
      DUSize  := SIO.Positive_Count(Data_Unit_Size_elems(HDUInfo.NAXISn));

    Put_Line("DU Start [blocks] :" & SIO.Positive_Count'Image(DUStart)); 
    Put_Line("DU Size  [element count]:" & SIO.Positive_Count'Image(DUSize)); 

    File.Set_File_Block_Index(InFile,HDUStart);

    for I in NAXISn'Range
    loop
      Put_Line(Integer'Image(I) & " : " & SIO.Positive_Count'Image(NAXISn(I)));
    end loop;

    --NPlanes     := NAXISn(3)*NAXISn(2);
    NPlanes     := NAXISn(2);
    PlaneLength := NAXISn(1);
    -- Size of Plane must be reasonable
    Put_Line("Plane Size [Bytes]:" & SIO.Positive_Count'Image(PlaneLength * SIO.Positive_Count( abs BITPIX/8 ))); 
    Put_Line("NPlanes           :" & SIO.Positive_Count'Image(NPlanes)); 

    declare
      Cards : Optional.Card_Arr := Read_Optional(InFile, Optional.Reserved.Array_Keys);
      F64_BZERO  : Float_64   := F64_Get_Float("BZERO",  Cards, 0.0);
      F64_BSCALE : Float_64   := F64_Get_Float("BSCALE", Cards, 1.0);
      F32_BZERO  : Float_32   := F32_Get_Float("BZERO",  Cards, 0.0);
      F32_BSCALE : Float_32   := F32_Get_Float("BSCALE", Cards, 1.0);
      I16_BLANK  : Integer_16 := Get_Int_16("BLANK", Cards);

      function I16F32_Scale is
        new Linear_Conv.FI(Integer_16, Float_32, Float_32,F32_BZERO,F32_BSCALE);
      NewBLANK : Float_32 := I16F32_Scale(I16_BLANK);

    begin
      TIO.Put_Line("BZERO    : " & Float_32'Image(F32_BZERO));
      TIO.Put_Line("BSCALE   : " & Float_32'Image(F32_BSCALE));
      TIO.Put_Line("   BLANK : " & Integer_16'Image(I16_BLANK));
      TIO.Put_Line("NewBLANK : " & Float_32'Image(NewBLANK));

     A := Tc(F64_BZERO);
     B := Tc(F64_BSCALE);


        case(BITPIX) is
          when -64 => F64_Read_All(InFile,NAXISn);
          when -32 => F32_Read_All(InFile,NAXISn);
          when  64 => I64_Read_All(InFile,NAXISn);
          when  32 => I32_Read_All(InFile,NAXISn);
          when  16 => I16_Read_All(InFile,NAXISn);
          when   8 => U8_Read_All(InFile,NAXISn);
          when others => TIO.Put_Line("BITPIX " & Integer'Image(BITPIX) & " not implemented.");
        end case;


      TIO.New_Line;
      TIO.Put_Line("Max Value :" & Tm'Image(Max));
      TIO.Put_Line("Min Value :" & Tm'Image(Min));
      TIO.Put_Line("Invalid Count: " & Natural'Image(Undef_Count));
      TIO.Put_Line("Invalid Count [%]: " & Float_32'Image(100.0*Float_32(Undef_Count)/Float_32(DUSize)));
    end;

   end;-- NAXISn BITOUX


   SIO.Close(InFile);

    Put_Line("done.");


  exception

    when Except_ID : others =>
      declare
        Error : Ada.Text_IO.File_Type := Standard_Error;
      begin
        New_Line(Error);
        Put_Line(Error, "Exception_Information: ");
        Put_Line(Error, Exception_Information( Except_ID ) );
        New_Line(Error);
        Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
        Put_Line(" > Trace-back of call stack: " );
        Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
        -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
        -- Do the same manually, use:
        -- addr2line -e ./fits addr1 addr2 ...
      end;
  end minmaxraw;

