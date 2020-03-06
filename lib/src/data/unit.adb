
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Data_Value; use Data_Value;

package body Unit is

-- raw data

procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1)
is
begin
    null;
end Read_Array_Values;


-- physical data

 procedure Read_Values
        (F : SIO.File_Type;
        BZERO  : in Tc;
        BSCALE : in Tc;
        Length : in Positive_Count;
        First  : in Positive := 1)
 is
    function  Scale_Int is new Scale(Tf, Tm, Tc, BZERO, BSCALE, "+","+");
    procedure cb_Elem(V : in Tf)
    is
    begin
        Element_Value(Scale_Int(V));
    end cb_Elem;
    procedure Read_Raw  is new Read_Array_Values(T=>Tf, Element => cb_Elem);
 begin
    Read_Raw(F, Length, First);
 end Read_Values;




 procedure Read_Float_Values
        (F : SIO.File_Type;
        BZERO  : in Tc;
        BSCALE : in Tc;
        Undef  : in Tm;
        Length : in Positive_Count;
        First  : in Positive := 1)
 is
    function Scale_Flt is new Scale_Float(Tf, Tm, Tc, BZERO, BSCALE, Undef, "+","+");
    procedure cb_Elem(V : in Tf)
    is
    begin
        Element_Value(Scale_Flt(V));
    end cb_Elem;
    procedure Read_Raw is new Read_Array_Values(T=>Tf, Element => cb_Elem);
 begin
    Read_Raw(F, Length, First);
 end Read_Float_Values;

end Unit;

