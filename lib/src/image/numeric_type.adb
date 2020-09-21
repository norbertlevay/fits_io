
package body Numeric_Type is

function Bit_Count return Positive
is
V : T := To_Numeric(0.0);
begin
    return V'Size;
end Bit_Count;

function BITPIX return Integer
is
V : T := To_Numeric(0.0);
begin
    return To_BITPIX(V);
end BITPIX;



  -- conversions to/form Float with undef handling

    Undef : Numeric;
    Undef_Valid : Boolean := False;

    Zero : Float := 0.0;
    NaN  : Float := 0.0 / Zero; -- FIXME see alternatives to encode NaN


 procedure Set_Undefined(U : in Numeric)
 is
 begin
     Undef       := U;
     Undef_Valid := True;
 end Set_Undefined;


  -- conversions to/from Float

 function Is_Undef_Float(F : Float) return Boolean
 is
 begin
     return (Not (F = F));
 end Is_Undef_Float;


  function To_Numeric(V : in Float) return Numeric
  is
  begin
      if(Undef_Valid AND Is_Undef_Float(V))
      then return Undef;
      else return +V;
      end if;
  end To_Numeric;


  function To_Float  (V : in Numeric) return Float
  is
  begin
      if(Undef_Valid AND Is_Undef(V,Undef))
      then return NaN;
      else return +V;
      end if;
  end To_Float;




 function To_Numeric(Af : in Float_Arr) return Numeric_Arr
 is
     An : Numeric_Arr(Af'Range);
 begin

     if(Undef_Valid)
     then

        for I in Af'Range
        loop
            if(Is_Undef_Float(Af(I)))
            then An(I) := Undef;
            else An(I) := +Af(I);
            end if;
        end loop;

        return An;

     else

         for I in Af'Range
        loop
            An(I) := +(Af(I));
        end loop;
 
        return An;
 
     end if;
 
 end To_Numeric;



 function To_Float(An : in Numeric_Arr) return Float_Arr
 is
     Af : Float_Arr(An'Range);
 begin

     if(Undef_Valid)
     then

         for I in An'Range
         loop
            if(Is_Undef(An(I),Undef))
            then Af(I) := NaN;
            else Af(I) := +An(I);
            end if;
         end loop;

         return Af;

     else

         for I in An'Range
         loop
             Af(I) := +An(I);
         end loop;

         return Af;

     end if;
 
 end To_Float;

end Numeric_Type;

