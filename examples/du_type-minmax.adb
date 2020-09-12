
with Ada.Text_IO;
--with V3_Types; use V3_Types;

package body DU_Type.Minmax is

    package TIO renames Ada.Text_IO;

    Min : Tm := T_Last;
    Max : Tm := T_First;


    procedure Plane_Data(E : Tm)
    is
    begin
        if(not T_Valid(E))
        then
            Special_Count := Special_Count + 1; -- Invalid but not NaN
        else
            if(E > Max) then Max := E; end if;
            if(E < Min) then Min := E; end if;
        end if;
    end Plane_Data;


    procedure Undef_Data(E : Tm)
    is
    begin
        Undef_Count := Undef_Count + 1;
    end Undef_Data;


    procedure Put_Results(UndefValid : in Boolean; UndefValue : in String)
    is
    begin
        TIO.Put_Line("UndefValid             : " & Boolean'Image(UndefValid));
        if(UndefValid)
        then
            TIO.Put_Line("UndefValue             : " & UndefValue);
        end if;
        TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
        TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
        TIO.Put_Line("Min                    : " & T_Image(Min));
        TIO.Put_Line("Max                    : " & T_Image(Max));
    end Put_Results;

end DU_Type.Minmax;

