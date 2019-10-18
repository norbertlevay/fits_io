with Ada.Text_IO;

with FITS; use FITS; -- Card_Type needed


package body Reserved is

-- Biblioghapic keys
	-- FIXME not implemented




-- Observation related keys
package TIO renames Ada.Text_IO;

procedure DBG_Print(Obs : in Obs_Type)
is
begin
TIO.Put("Obs "&Boolean'Image(Obs.DATEOBS.Read) & " DATE-OBS ");
TIO.Put_Line(Obs.DATEOBS.Value);
TIO.Put("Obs "&Boolean'Image(Obs.TELESCOP.Read) & " TELESCOP ");
TIO.Put_Line(Obs.TELESCOP.Value);
TIO.Put("Obs "&Boolean'Image(Obs.INSTRUME.Read) & " INSTRUME ");
TIO.Put_Line(Obs.INSTRUME.Value);
TIO.Put("Obs "&Boolean'Image(Obs.OBSERVER.Read) & " OBSERVER ");
TIO.Put_Line(Obs.OBSERVER.Value);
TIO.Put("Obs "&Boolean'Image(Obs.OBJECT.Read) & " OBJECT ");
TIO.Put_Line(Obs.OBJECT.Value);
end DBG_Print;




        function Match_Any_Obs(Card : in Card_Type;
                                Obs : in out Obs_Type) return Boolean
        is
        begin

		-- FIXME missing check duplicate card

                if(Card(1..8) = "DATE-OBS")
                then
                        Obs.DATEOBS.Value :=  Card(11..30);
                        Obs.DATEOBS.Read  := True;

--              elsif(Carda(1..4) = "DATE") FIXME how to deal with this ? [FITS 4.4.2.2] 
--              then

                elsif(Card(1..8) = "TELESCOP")
                then
                        Obs.TELESCOP.Value :=  Card(11..30);
                        Obs.TELESCOP.Read  := True;

                elsif(Card(1..8) = "INSTRUME")
                then
                        Obs.INSTRUME.Value :=  Card(11..30);
                        Obs.INSTRUME.Read  := True;

                elsif(Card(1..8) = "OBSERVER")
                then
                        Obs.OBSERVER.Value :=  Card(11..30);
                        Obs.OBSERVER.Read  := True;

                elsif(Card(1..8) = "OBJECT  ")
                then
                        Obs.OBJECT.Value :=  Card(11..30);
                        Obs.OBJECT.Read  := True;

                else
                        return False;
                end if;

                return True;

        end Match_Any_Obs;






end Reserved;

