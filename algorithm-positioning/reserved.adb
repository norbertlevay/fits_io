with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
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

-- Production keys

procedure DBG_Print(Prod : in Prod_Type)
is
begin
if(Prod.DATE.Read)    then TIO.Put_Line("Prod DATE    "&Prod.DATE.Value);   end if;
if(Prod.ORIGIN.Read)  then TIO.Put_Line("Prod ORIGIN  "&Prod.ORIGIN.Value); end if;
if(Prod.BLOCKED.Read) then TIO.Put_Line("Prod BLOCKED "&Prod.BLOCKED.Value); end if;
end DBG_Print;


        function Match_Any_Prod(Flag : Boolean;
			        Card : in Card_Type;
                                Prod : in out Prod_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;


                if(Card(1..8) = "DATE    ")
                then
                        if (NOT Prod.DATE.Read)
                        then
				Prod.DATE.Value :=  Card(11..30);
                        	Prod.DATE.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                elsif(Card(1..8) = "ORIGIN  ")
                then
                        --if (NOT Prod.ORIGIN.Read) FIXME note: this card appears twice in some FITS files
                        if (True)
                        then
	                	Prod.ORIGIN.Value :=  Card(11..30);
                        	Prod.ORIGIN.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "BLOCKED ")
                then
                        if (NOT Prod.BLOCKED.Read)
                        then
	                	Prod.BLOCKED.Value :=  Card(11..30);
                        	Prod.BLOCKED.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                else
                        return False;
                end if;

                return True;

        end Match_Any_Prod;



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


                if(Card(1..8) = "AUTHOR  ")
                then
			if (NOT Biblio.AUTHOR.Read)
                        then
	                	Biblio.AUTHOR.Value :=  Card(11..30);
                        	Biblio.AUTHOR.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "REFERENC")
                then
        		if (NOT Biblio.REFERENC.Read)
                        then
		                Biblio.REFERENC.Value :=  Card(11..30);
                	        Biblio.REFERENC.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

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


                if(Card(1..8) = "DATE-OBS")
                then
         		if (NOT Obs.DATEOBS.Read)
                        then
	                       Obs.DATEOBS.Value :=  Card(11..30);
			       Obs.DATEOBS.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

--              elsif(Carda(1..4) = "DATE") FIXME how to deal with this ? [FITS 4.4.2.2] 
--              then

                elsif(Card(1..8) = "TELESCOP")
                then
          		if (NOT Obs.TELESCOP.Read)
                        then
	                	Obs.TELESCOP.Value :=  Card(11..30);
          	        	Obs.TELESCOP.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "INSTRUME")
                then
           		if (NOT Obs.INSTRUME.Read)
                        then
	                	Obs.INSTRUME.Value :=  Card(11..30);
                        	Obs.INSTRUME.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "OBSERVER")
                then
            		if (NOT Obs.OBSERVER.Read)
                        then
	               		Obs.OBSERVER.Value :=  Card(11..30);
                        	Obs.OBSERVER.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "OBJECT  ")
                then
             		if (NOT Obs.OBJECT.Read)
                        then
	              		Obs.OBJECT.Value :=  Card(11..30);
                        	Obs.OBJECT.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                else
                        return False;
                end if;

                return True;

        end Match_Any_Obs;




-- all Reserved categories

procedure DBG_Print(Res : in Reserved_Type)
is
begin
	DBG_Print(Res.Prod);
	DBG_Print(Res.Biblio);
	DBG_Print(Res.Obs);
end DBG_Print;



        function Match_Any(Flag : Boolean;
			   Card : in Card_Type;
                           Reserved : in out Reserved_Type) return Boolean
        is
        begin
		if(Match_Any_Prod(Flag, Card, Reserved.Prod))
		then
			null;

		elsif(Match_Any_Biblio(Flag, Card, Reserved.Biblio))
		then
			null;

		elsif(Match_Any_Obs(Flag, Card, Reserved.Obs))
		then
			null;
		else
			return False;
		end if;

		return True;

	end Match_Any;
	
----------------------------------------------------------	
-- Array structure (a.k.a. IMAGEs) keys

procedure DBG_Print(Arr : in Arr_Type)
is
begin
if(Arr.BSCALE.Read)  then TIO.Put_Line("Arr BSCALE  "&Arr.BSCALE.Value);  end if;
if(Arr.BZERO.Read)   then TIO.Put_Line("Arr BZERO   "&Arr.BZERO.Value);   end if;
if(Arr.BUNIT.Read)   then TIO.Put_Line("Arr BUNIT   "&Arr.BUNIT.Value);   end if;
if(Arr.BLANK.Read)   then TIO.Put_Line("Arr BLANK   "&Arr.BLANK.Value);   end if;
if(Arr.DATAMAX.Read) then TIO.Put_Line("Arr DATAMAX "&Arr.DATAMAX.Value); end if;
if(Arr.DATAMIN.Read) then TIO.Put_Line("Arr DATAMIN "&Arr.DATAMIN.Value); end if;
end DBG_Print;


        function Match_Any_Arr(Flag : Boolean;
			        Card : in Card_Type;
                                Arr  : in out Arr_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;


                if(Card(1..8) = "BSCALE  ")
                then
                        if (NOT Arr.BSCALE.Read)
                        then
				Arr.BSCALE.Value :=  Card(11..30);
                        	Arr.BSCALE.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                elsif(Card(1..8) = "BZERO   ")
                then
                        if (NOT Arr.BZERO.Read) 
                        then
	                	Arr.BZERO.Value :=  Card(11..30);
                        	Arr.BZERO.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "BUNIT   ")
                then
                        if (NOT Arr.BUNIT.Read)
                        then
	                	Arr.BUNIT.Value :=  Card(11..30);
                        	Arr.BUNIT.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "BLANK   ")
                then
                        if (NOT Arr.BLANK.Read)
                        then
	                	Arr.BLANK.Value :=  Card(11..30);
                        	Arr.BLANK.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif(Card(1..8) = "DATAMAX ")
                then
                        if (NOT Arr.DATAMAX.Read)
                        then
	                	Arr.DATAMAX.Value :=  Card(11..30);
                        	Arr.DATAMAX.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

               elsif(Card(1..8) = "DATAMIN ")
                then
                        if (NOT Arr.DATAMIN.Read)
                        then
	                	Arr.DATAMIN.Value :=  Card(11..30);
                        	Arr.DATAMIN.Read  := True;
		       	else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                else
                        return False;
                end if;

                return True;

        end Match_Any_Arr;




end Reserved;

