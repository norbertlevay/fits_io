with Ada.Text_IO;

with FITS; use FITS; -- Card_Type needed


-- Notes on Reserved optional keys:
--          Generic      Specific
-- Prim IMG   x             none
-- Prim RG    x	            yes
-- Ext  IMG   x             none
-- Ext  TAB   x             Tab
-- Ext BTAB   x             Tab+more

-- Generic key groups:
-- 1 general descriptive
-- 2 observations
-- 3 bibliographic
-- 4 commentary (COMMENT HISTORY <blank>)
-- 5 array value related
-- 6 WCS related
-- 7 extension keywords



package body Reserved is

package TIO renames Ada.Text_IO;

-- Biblioghapic keys

procedure DBG_Print(Biblio : in Biblio_Type)
is
begin
if(Biblio.AUTHOR.Read)   then TIO.Put_Line("Biblio AUTHOR   "&Biblio.AUTHOR.Value);   end if;
if(Biblio.REFERENC.Read) then TIO.Put_Line("Biblio REFERENC "&Biblio.REFERENC.Value); end if;
end DBG_Print;


        function Match_Any_Biblio(Flag   : Boolean;
				  Card   : in Card_Type;
                                  Biblio : in out Biblio_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		-- FIXME missing check duplicate card

                if(Card(1..8) = "AUTHOR  ")
                then
                        Biblio.AUTHOR.Value :=  Card(11..30);
                        Biblio.AUTHOR.Read  := True;


                elsif(Card(1..8) = "REFERENC")
                then
                        Biblio.REFERENC.Value :=  Card(11..30);
                        Biblio.REFERENC.Read  := True;

                else
                        return False;
                end if;

                return True;

        end Match_Any_Biblio;






-- Observation related keys

procedure DBG_Print(Obs : in Obs_Type)
is
begin
if(Obs.DATEOBS.Read)  then TIO.Put_Line("Obs DATE-OBS "&Obs.DATEOBS.Value);  end if;
if(Obs.TELESCOP.Read) then TIO.Put_Line("Obs TELESCOP "&Obs.TELESCOP.Value); end if;
if(Obs.INSTRUME.Read) then TIO.Put_Line("Obs INSTRUME "&Obs.INSTRUME.Value); end if;
if(Obs.OBSERVER.Read) then TIO.Put_Line("Obs OBSERVER "&Obs.OBSERVER.Value); end if;
if(Obs.OBJECT.Read)   then TIO.Put_Line("Obs OBJECT   "&Obs.OBJECT.Value);   end if;
end DBG_Print;




        function Match_Any_Obs(Flag : Boolean;
				Card : in Card_Type;
                                Obs : in out Obs_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

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

