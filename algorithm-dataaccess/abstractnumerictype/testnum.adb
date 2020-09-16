
with Ada.Integer_Text_IO;

with Numeric_Type;

procedure testnum
is

package TIO renames Ada.Integer_Text_IO;
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


------------------------------
begin

TIO.Put(Int.Bit_Count);
TIO.Put(Flt.Bit_Count);
ttti.doSomething;
tttlf.doSomething;
tttllf.doSomething;

TIO.Put(Aint'Size);
TIO.Put(Vint'Size);


end testnum;
