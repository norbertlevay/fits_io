

with V3_Types; use V3_Types;

package body T_Ops is

    function T_First return Float_64 is begin return Float_64'First; end T_First;
    function T_First return Float_32 is begin return Float_32'First; end T_First;
    function T_First return Integer_64 is begin return Integer_64'First; end T_First;
    function T_First return Integer_32 is begin return Integer_32'First; end T_First;
    function T_First return Integer_16 is begin return Integer_16'First; end T_First;
    function T_First return Unsigned_8 is begin return Unsigned_8'First; end T_First;

    function T_Last return Float_64 is begin return Float_64'Last; end T_Last;
    function T_Last return Float_32 is begin return Float_32'Last; end T_Last;
    function T_Last return Integer_64 is begin return Integer_64'Last; end T_Last;
    function T_Last return Integer_32 is begin return Integer_32'Last; end T_Last;
    function T_Last return Integer_16 is begin return Integer_16'Last; end T_Last;
    function T_Last return Unsigned_8 is begin return Unsigned_8'Last; end T_Last;





    function T_Image(V: Float_64) return String is begin return Float_64'Image(V); end T_Image;
    function T_Image(V: Float_32) return String is begin return Float_32'Image(V); end T_Image;
    function T_Image(V: Integer_64) return String is begin return Integer_64'Image(V);end T_Image;
    function T_Image(V: Integer_32) return String is begin return Integer_32'Image(V);end T_Image;
    function T_Image(V: Integer_16) return String is begin return Integer_16'Image(V);end T_Image;
    function T_Image(V: Unsigned_8) return String is begin return Unsigned_8'Image(V);end T_Image;

    function T_Valid(V: Float_64) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Float_32) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_64) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_32) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_16) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Unsigned_8) return Boolean is begin return V'Valid; end T_Valid;


--    function ">"(L,R : Float_64) return Boolean is begin return (L>R); end ">";
--    function ">"(L,R : Float_32) return Boolean is begin return (L>R); end ">";

end T_Ops;

