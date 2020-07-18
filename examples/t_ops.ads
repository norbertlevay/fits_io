

with V3_Types; use V3_Types;

package T_Ops is

    function T_First return Float_64;
    function T_First return Float_32;
    function T_First return Integer_64;
    function T_First return Integer_32;
    function T_First return Integer_16;
    function T_First return Unsigned_8;

    function T_Last return Float_64;
    function T_Last return Float_32;
    function T_Last return Integer_64;
    function T_Last return Integer_32;
    function T_Last return Integer_16;
    function T_Last return Unsigned_8;


    function T_Image(V: Float_64) return String;
    function T_Image(V: Float_32) return String;
    function T_Image(V: Integer_64) return String;
    function T_Image(V: Integer_32) return String;
    function T_Image(V: Integer_16) return String;
    function T_Image(V: Unsigned_8) return String;

    function T_Valid(V: Float_64) return Boolean;
    function T_Valid(V: Float_32) return Boolean;
    function T_Valid(V: Integer_64) return Boolean;
    function T_Valid(V: Integer_32) return Boolean;
    function T_Valid(V: Integer_16) return Boolean;
    function T_Valid(V: Unsigned_8) return Boolean;


--    function ">"(L,R : Float_64) return Boolean;
--    function ">"(L,R : Float_32) return Boolean;

end T_Ops;

