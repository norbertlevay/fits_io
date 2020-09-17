
with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Numeric_Type;
with Scaling;

procedure testnum
is

package TIO renames Ada.Integer_Text_IO;
package FIO is new Ada.Text_IO.Float_IO(Long_Float);

-- pool of Numeric - Float converions

function "+"(V : in Float) return Long_Long_Float is begin return Long_Long_Float(V); end "+";
function "+"(V : in Float) return Long_Float is begin return Long_Float(V); end "+";
function "+"(V : in Float) return Float      is begin return Float(V);      end "+";
function "+"(V : in Float) return Integer    is begin return Integer(V);    end "+";

function "+"(V : in Integer)           return Float is begin return Float(V); end "+";
--function "+"(V : in Float)             return Float is begin return Float(V); end "+";
function "+"(V : in Long_Float)        return Float is begin return Float(V); end "+";
function "+"(V : in Long_Long_Float)   return Float is begin return Float(V); end "+";

function Is_Undef(V,U : in Integer)    return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Float)      return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;





-- -----------------------------------


package Int is new Numeric_Type(T=>Integer);
package Flt is new Numeric_Type(T=>Float);
package LFlt is new Numeric_Type(T=>Long_Float);
package LLFlt is new Numeric_Type(T=>Long_Long_Float);

-- not possible because we've just subsituted T with Int
-- so Int.T is actually the Int
--Vnumi : Int.T;
--Vnumf : Flt.T;

Aint : Int.T_Arr(1 .. 2);
Vint : Int.Numeric;


generic
 with package numt is new Numeric_Type(<>);
package ttt is
 procedure doSomething;
end ttt;

package body ttt is
    use numt;
    procedure doSomething
    is
        Vnt : Numeric;
    begin
        TIO.Put(Vnt'Size);
    end doSomething;
end ttt;

package ttti is new ttt(Int);
package tttllf is new ttt(LLFlt);
package tttlf is new ttt(LFlt);

------ Scaling

package I32 is new Numeric_Type(Integer);
package F64 is new Numeric_Type(Long_Float);

I32Udf : I32.Numeric := Integer(11);
F64Udf : F64.Numeric;

--     Tdst Tsrc
package F64I32_Scaling is new Scaling(I32,F64);
package I32F64_Scaling is new Scaling(F64,I32);

Zero : Long_Float := 0.0;
NaN  : Long_Float := 0.0/Zero;
------------------------------
begin

TIO.Put(Int.Bit_Count);
TIO.Put(Flt.Bit_Count);
ttti.doSomething;
tttlf.doSomething;
tttllf.doSomething;

TIO.Put(Aint'Size);
TIO.Put(Vint'Size);


-- Scaling

Ada.Text_IO.New_Line;
Ada.Text_IO.Put_Line("Scaling test (Undefined values not set)");

TIO.Put(I32Udf);
F64Udf := F64I32_Scaling.Linear(I32Udf);
FIO.Put(F64Udf);

Ada.Text_IO.New_Line;
Ada.Text_IO.Put_Line("Scaling test F->I with NaN");

I32.Set_Undefined(I32Udf);
I32F64_Scaling.Set_Undefined(NaN);
I32Udf := I32F64_Scaling.Linear(NaN);
TIO.Put(I32Udf);

Ada.Text_IO.New_Line;
Ada.Text_IO.Put_Line("Set_undefined test");

F64I32_Scaling.Set_Undefined(I32Udf);
TIO.Put(I32.Get_Undefined);
FIO.Put(F64.Get_Undefined);




Ada.Text_IO.New_Line;
Ada.Text_IO.Put_Line("Scaling test (Undefined values set)");

TIO.Put(I32Udf);
F64Udf := F64I32_Scaling.Linear(I32Udf);
FIO.Put(F64Udf);


end testnum;
