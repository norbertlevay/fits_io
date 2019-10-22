with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;-- Trim needed

with FITS; use FITS; -- Card_Type needed
with Keyword_Record; use Keyword_Record; -- Is_Array() needed

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
	for I in Prod_Key'Range
	loop
		if(Prod(I).Read)
		then 
			TIO.Put_Line("Prod "&Prod_Key'Image(I) &" "&Prod(I).Value);
		end if;
	end loop;
end DBG_Print;


        function Match_Any_Prod(Flag : Boolean;
			        Card : in Card_Type;
                                Prod : in out Prod_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in Prod_Key'Range
		loop
			if(Trim(Card(1..8), Ada.Strings.Right) = Prod_Key'Image(I))
			then
				if (NOT Prod(I).Read)
        	        	then
	        	       		Prod(I).Value :=  Card(11..30);
                  		      	Prod(I).Read  := True;

					return True;
		       		else
                               		Raise_Exception(Duplicate_Card'Identity, Card);
                        	end if;
			end if;
		end loop;

		return False;

        end Match_Any_Prod;



-- Biblioghapic keys

procedure DBG_Print(Biblio : in Biblio_Type)
is
begin
	for I in Biblio_Key'Range
	loop
		if(Biblio(I).Read)
		then 
			TIO.Put_Line("Biblio "&Biblio_Key'Image(I) &" "&Biblio(I).Value);
		end if;
	end loop;
end DBG_Print;


        function Match_Any_Biblio(Flag   : Boolean;
				  Card   : in Card_Type;
                                  Biblio : in out Biblio_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in Biblio_Key'Range
		loop
			if(Trim(Card(1..8), Ada.Strings.Right) = Biblio_Key'Image(I))
			then
				if (NOT Biblio(I).Read)
        	        	then
	        	       		Biblio(I).Value :=  Card(11..30);
                        		Biblio(I).Read  := True;

					return True;
		       		else
                               		Raise_Exception(Duplicate_Card'Identity, Card);
                        	end if;
			end if;
		end loop;

		return False;

        end Match_Any_Biblio;

-- Comments

procedure DBG_Print(Comments : in Comment_Type)
is
begin
TIO.New_Line;
for I in 1 .. Comments.COMMENT.Last
loop
	TIO.Put_Line("C"&Integer'Image(Comments.COMMENT.Value(I).Pos)
	                        &"> "& Comments.COMMENT.Value(I).Str);
end loop;
for I in 1 .. Comments.HISTORY.Last
loop
	TIO.Put_Line("H"&Integer'Image(Comments.HISTORY.Value(I).Pos)
	                        &"> "& Comments.HISTORY.Value(I).Str);
end loop;
for I in 1 .. Comments.empty_key.Last
loop
	TIO.Put_Line(" "&Integer'Image(Comments.empty_key.Value(I).Pos)
	                        &"> "& Comments.empty_key.Value(I).Str);
end loop;
TIO.New_Line;
end DBG_Print;


        function Match_Any_Comment(Flag    : Boolean;
				   Pos     : in Positive;
				   Card    : in Card_Type;
                                   Comment : in out Comment_Type) return Boolean
        is
		Ix : Positive;
        begin
		if(NOT Flag) then
			return False;
		end if;


		if ( Card(1..8) = "COMMENT " )
                then
                        Ix := Comment.COMMENT.Last + 1;
			if(Comment.COMMENT.Last < Comments_Max)
                        then
                                Comment.COMMENT.Value(Ix) := (Pos,Card(9..80));
                        	Comment.COMMENT.Last := Comment.COMMENT.Last + 1;
                        else
                                Raise_Exception(Constraint_Error'Identity, "COMMENT array full.");
                        end if;

		elsif ( Card(1..8) = "HISTORY " )
                then
                        Ix := Comment.HISTORY.Last + 1;
			if(Comment.HISTORY.Last < Comments_Max)
                        then
                                Comment.HISTORY.Value(Ix) := (Pos,Card(9..80));
                        	Comment.HISTORY.Last := Comment.HISTORY.Last + 1;
                        else
                                Raise_Exception(Constraint_Error'Identity, "HISTORY array full.");
                        end if;

		elsif ( Card(1..8) = "        " )
                then
                        Ix := Comment.empty_key.Last + 1;
			if(Comment.empty_key.Last < Comments_Max)
                        then
                                Comment.empty_key.Value(Ix) := (Pos,Card(9..80));
                        	Comment.empty_key.Last := Comment.empty_key.Last + 1;
                        else
                                Raise_Exception(Constraint_Error'Identity, "empty_key array full.");
                        end if;

                else
                        return False;
                end if;

                return True;

        end Match_Any_Comment;









-- Observation related keys

procedure DBG_Print(Obs : in Obs_Type)
is
begin
for I in Obs_Key
loop
	if(Obs(I).Read)  then TIO.Put_Line("Obs "&Obs_Key'Image(I)&" "&Obs(I).Value);  end if;
end loop;
end DBG_Print;


        function Match_Any_Obs(Flag : Boolean;
				Card : in Card_Type;
                                Obs : in out Obs_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

--              elsif(Carda(1..4) = "DATE") FIXME how to deal with this ? [FITS 4.4.2.2] 
--              then


		for I in Obs_Key
		loop
			if(Trim(Card(1..8), Ada.Strings.Right) = Obs_Key'Image(I))
			then
	         		if (NOT Obs(I).Read)
                	        then
					Obs(I).Value := Card(11..30);
					Obs(I).Read  := True;

					return True;
				else
                                	Raise_Exception(Duplicate_Card'Identity, Card);
                        	end if;
			end if;

		end loop;

		return False;

        end Match_Any_Obs;




-- all Reserved categories

procedure DBG_Print(Res : in Reserved_Type)
is
begin
	DBG_Print(Res.Prod);
	DBG_Print(Res.Biblio);
	DBG_Print(Res.Comments);
	DBG_Print(Res.Obs);
end DBG_Print;



        function Match_Any(Flag : Boolean;
			   Pos  : in Positive;
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

		elsif(Match_Any_Comment(Flag, Pos, Card, Reserved.Comments))
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

procedure DBG_Print(DataArr : in DataArr_Type)
is
begin
	for I in DataArr_Key'Range
	loop
		if(DataArr(I).Read)
		then 
			TIO.Put_Line("DataArr "&DataArr_Key'Image(I)&" "&DataArr(I).Value);
		end if;
	end loop;
end DBG_Print;


        function Match_Any_DataArr(Flag : Boolean;
			           Card    : in Card_Type;
                                   DataArr : in out DataArr_Type) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in DataArr_Type'Range
		loop
			if(Trim(Card(1..8),Ada.Strings.Right) = DataArr_Key'Image(I))
			then 
	                        if (NOT DataArr(I).Read)
         	               	then
					DataArr(I).Value :=  Card(11..30);
                        		DataArr(I).Read  := True;

					return True;
		       		else
                        	        Raise_Exception(Duplicate_Card'Identity, Card);
                       		end if;
			end if;
		end loop;

		return False;

        end Match_Any_DataArr;

end Reserved;

