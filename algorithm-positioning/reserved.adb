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

 -- FIXME Reserved (optional) keys are not fixed-format, but free-format -> Value NOT Card(11..30) 

-- FIXME pose Reserved as special case of ANY optional card parsing, like user's own proprietary card definitions <- implemente such generic parsing and use as special case for ReservdKeys defined in Stabdard


package body Reserved is

package TIO renames Ada.Text_IO;

-- Production keys



        function Match_Any_Prod(Flag : Boolean;
			        Card : in Card_Type;
                                Res  : in out Common_Key_Values) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in Prod_Key'Range
		loop
			if(Trim(Card(1..8), Ada.Strings.Right) = Prod_Key'Image(I))
			then
				if (NOT Res(I).Read)
        	        	then
	        	       		Res(I).Value :=  Card(11..30);
                  		      	Res(I).Read  := True;

					return True;
		       		else
                               		Raise_Exception(Duplicate_Card'Identity, Card);
                        	end if;
			end if;
		end loop;

		return False;

        end Match_Any_Prod;






-- Biblioghapic keys


        function Match_Any_Biblio(Flag   : Boolean;
				  Card   : in Card_Type;
                                Res  : in out Common_Key_Values) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in Biblio_Key'Range
		loop
			if(Trim(Card(1..8), Ada.Strings.Right) = Biblio_Key'Image(I))
			then
				if (NOT Res(I).Read)
        	        	then
	        	       		Res(I).Value :=  Card(11..30);
                        		Res(I).Read  := True;

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


        function Match_Any_Obs(Flag : Boolean;
				Card : in Card_Type;
                                Res  : in out Common_Key_Values) return Boolean
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
	         		if (NOT Res(I).Read)
                	        then
					Res(I).Value := Card(11..30);
					Res(I).Read  := True;

					return True;
				else
                                	Raise_Exception(Duplicate_Card'Identity, Card);
                        	end if;
			end if;

		end loop;

		return False;

        end Match_Any_Obs;




-- all Reserved categories

procedure DBG_Print(Res : in Primary_Type)
is
begin
	DBG_Print(Res.Comments);
-- ??	DBG_Print(Res.Res);
end DBG_Print;



        function Match_Any(Flag : Boolean;
			   Pos  : in Positive;
			   Card : in Card_Type;
                           Reserved : in out Primary_Type) return Boolean
        is
        begin
		if(Match_Any_Prod(Flag, Card, Reserved.Res))
		then
			null;

		elsif(Match_Any_Biblio(Flag, Card, Reserved.Res))
		then
			null;

		elsif(Match_Any_Comment(Flag, Pos, Card, Reserved.Comments))
		then
			null;

		elsif(Match_Any_Obs(Flag, Card, Reserved.Res))
		then
			null;
		else
			return False;
		end if;

		return True;

	end Match_Any;
	
----------------------------------------------------------	
-- Array structure (a.k.a. IMAGEs) keys


        function Match_Any_DataArr(Flag : Boolean;
			           Card    : in Card_Type;
                           Res : in out Common_Key_Values) return Boolean
        is
        begin
		if(NOT Flag) then
			return False;
		end if;

		for I in DataArr_Key'Range
		loop
			if(Trim(Card(1..8),Ada.Strings.Right) = DataArr_Key'Image(I))
			then 
	                        if (NOT Res(I).Read)
         	               	then
					Res(I).Value :=  Card(11..30);
                        		Res(I).Read  := True;

					return True;
		       		else
                        	        Raise_Exception(Duplicate_Card'Identity, Card);
                       		end if;
			end if;
		end loop;

		return False;

        end Match_Any_DataArr;



function Match_Any_ResRG(Flag : Boolean;
                         Card : in Card_Type;
                         RGArr : in out Primary_Root_Values) return Boolean
is
                Idx : Positive;
begin
	if(NOT Flag) then
		return False;
	end if;

        for I in Primary_Root'Range
        loop
		if(Is_Array(Card,  Primary_Root'Image(I),1,RANDG_Max,Idx ) )
                then
			if (NOT RGArr(I)(Idx).Read)
                        then
				RGArr(I)(Idx).Value :=  Card(11..30);
                                RGArr(I)(Idx).Read  := True;

                                return True;
                         else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                         end if;
                 end if;
        end loop;

        return False;

end Match_Any_ResRG;



-- for Extension
procedure DBG_Print(Res : in Extension_Type)
is
begin
        DBG_Print(Res.Comments);
-- ??   DBG_Print(Res.Res);
end DBG_Print;


        function Match_Any_ConfExt(Card : in Card_Type;
                                   ConfExt : in out Extension_Key_Values) return Boolean
        is
        begin

                if ( "EXTNAME " = Card(1..8) )
                then
                        if(NOT ConfExt(EXTNAME).Read)
                        then
                                ConfExt(EXTNAME).Value := Card(11..30);
                                ConfExt(EXTNAME).Read := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif ( "EXTVER  " = Card(1..8) )
                then
                        if(NOT ConfExt(EXTVER).Read)
                        then
                                ConfExt(EXTVER).Value := Card(11..30);
                                ConfExt(EXTVER).Read := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                elsif ( "EXTLEVEL" = Card(1..8) )
                then

                        if(NOT ConfExt(EXTLEVEL).Read)
                        then
                                ConfExt(EXTLEVEL).Value := Card(11..30);
                                ConfExt(EXTLEVEL).Read := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                else
                        return False;
                end if;

                return True;

        end Match_Any_ConfExt;








       function Match_Any_Tab(Card : in Card_Type;
                              Tab  : in out Extension_Root_Values) return Boolean
        is
                Ix : Positive;
        begin

		for I in Tab_Root'Range
		loop
	                if (Is_Array(Card,  Tab_Root'Image(I),1,TFIELDS_Max,Ix )  )
        	        then
                        	if(NOT Tab(I)(Ix).Read)
	                        then
        	                        Tab(I)(Ix).Value := Card(11..30);
                	                Tab(I)(Ix).Read := True;

					return True;
                        	else
	                                Raise_Exception(Duplicate_Card'Identity, Card);
        	                end if;
			end if;
		end loop;

		return False;

        end Match_Any_Tab;

       function Match_Any_BinTab(Card : in Card_Type;
                                 Ext  : in out Extension_Type) return Boolean
        is
                Ix : Positive;
        begin

                if ( "TDIM" = Card(1..4) )
                then
--                        if(State.XTENSION_Val = BIN_TABLE)
  --                      then
                                Ix := Extract_Index("TDIM",Card(1..8));
                                if(NOT Ext.Arr(TDIM)(Ix).Read)
                                then
                                        Ext.Arr(TDIM)(Ix).Value := Card(11..30);
                                        Ext.Arr(TDIM)(Ix).Read  := True;
                                else
                                        Raise_Exception(Duplicate_Card'Identity, Card);
                                end if;

    --                    else
      --                          Raise_Exception(Unexpected_Card'Identity, Card);
        --                end if;


                elsif ( "THEAP   " = Card(1..8) )
                then
          --              if(State.XTENSION_Val = BIN_TABLE)
            --            then
                                if(NOT Ext.Ext(THEAP).Read)
                                then
                                        Ext.Ext(THEAP).Value := Card(11..30);
                                        Ext.Ext(THEAP).Read  := True;
                                else
                                        Raise_Exception(Duplicate_Card'Identity, Card);
                                end if;

              --          else
                --                Raise_Exception(Unexpected_Card'Identity, Card);
                  --      end if;
		
		else
			return False;
		end if;

		return True;

        end Match_Any_BinTab;








        function Match_Any(Flag : Boolean;
                           Pos  : in Positive;
                           Card : in Card_Type;
                           Reserved : in out Extension_Type) return Boolean
        is
        begin
                if(Match_Any_Prod(Flag, Card, Reserved.Comm))
                then
                        null;

                elsif(Match_Any_Biblio(Flag, Card, Reserved.Comm))
                then
                        null;

                elsif(Match_Any_Comment(Flag, Pos, Card, Reserved.Comments))
                then
                        null;

                elsif(Match_Any_Obs(Flag, Card, Reserved.Comm))
                then
                        null;
                else
                        return False;
                end if;

                return True;

        end Match_Any;


-- NON FUNCTIONAL: only backup here, does not compile
-- move here Get funcs from FA_Primary FA_Extension
-- These Get funcs would read out and return optional/Reserved keys after FA-stopped


-- Final FA states naming: 
-- IMAGE : header contains exactly only mandatory cards for IMAGE, no other cards.
-- IMAGE with other(n) : header has mandatory IMAGE cards and n extra cards 
-- not spec'd by Options or unknown to this implementation.
-- Other cases refer to Reserved key groups, like biblio, related to bibligraphic keys:
-- IMAGE with biblo wcs : has only mandatory keys and at least one of biblio related 
-- reserved keys, and some WCS keys.



-- From FA_Primary:
--

-- FA_Primary.ads had:
        
-- Reserved keys

        type Res_Key_Arr is array (Positive range <>) of Reserved_Key;

        type Key_Rec is  
                record
                        Key   : Reserved_Key;
                        Value : String(1..20);
                end record;
        type Key_Rec_Arr is array (Positive range <>) of Key_Rec;

        -- Reserved indexed keys

        type Reserved_Root is (PTYPE,PSCAL,PZERO);

        type Res_Root_Arr  is array (Natural range <>) of Reserved_Root;

        type IdxKey_Rec is
                record
                        Root : Reserved_Root;
                        Arr  : RANDG_Arr;
                end record;
        type IdxKey_Rec_Arr is array (Natural range <>) of IdxKey_Rec;


        function Get(Keys  : in Res_Key_Arr)  return Key_Rec_Arr;
        function Get(Roots : in Res_Root_Arr) return IdxKey_Rec_Arr;

--
-- FA_Primary.adb had:
--
 function Needed_Count(Keys : in Res_Key_Arr) return Natural
 is
         Count : Natural := 0;
 begin
        for I in Keys'Range
        loop
                if(State.Res.Res(Keys(I)).Read)
                then
                        Count := Count + 1;
                end if;
        end loop;

        return Count;

 end Needed_Count;


 function Get(Keys : in Res_Key_Arr) return Key_Rec_Arr
 is
         FoundCount : Natural := Needed_Count(Keys);
         OutKeys : Key_Rec_Arr(1..FoundCount);
         Idx : Positive := 1;  
 begin

        for I in Keys'Range
        loop
                if(State.Res.Res(Keys(I)).Read)
                then
                        OutKeys(Idx).Key   := Keys(I);
                        OutKeys(Idx).Value := State.Res.Res(Keys(I)).Value;
                        exit when (Idx = FoundCount);
                        Idx := Idx + 1;
                end if;
        end loop;

        return OutKeys;
 end Get;


        -- experimental Get(RootArr) to return Tab_Type's read arrays

        -- FIXME modify so that Arr can be shorter, containing only the Read elements
        function Is_Any_Element_Read(Arr : RANDG_Arr) return Boolean
        --function Is_Any_Element_Read(Arr : Reserved.RANDG_Arr) return Boolean
        is
                Is_Read : Boolean := False;
                -- FIXME what if Arr is empty array ? raise exception or return False
        begin
                for I in Arr'Range
                loop
                        Is_Read := Arr(I).Read;
                        exit when Arr(I).Read;
               end loop;
                return Is_Read;
        end Is_Any_Element_Read;


       function Is_In_Set(Root : Reserved_Root; Roots : Res_Root_Arr) return Boolean
        is
        begin
                for I in Roots'Range
                loop
                        if(Root = Roots(I))
                        then
                                return True;
                        end if;
                end loop;
                return False;
        end Is_In_Set;

        function Needed_Length(Roots : Res_Root_Arr) return Natural
        is
                Len : Natural := 0;
        begin
                if(Is_In_Set(PTYPE,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PTYPE))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(PSCAL,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PSCAL))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(PZERO,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PZERO))) then Len := Len + 1; end if;
                end if;
                return Len;
        end Needed_Length;




 function Get(Roots : Res_Root_Arr) return IdxKey_Rec_Arr
        is
                IdxKey : IdxKey_Rec_Arr(1..Needed_Length(Roots));
                Len : Natural := 0;-- FIXME rename to Idx
        begin
                if(Is_In_Set(PTYPE,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PTYPE)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := PTYPE;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PTYPE);
                        end if;
                end if;

                if(Is_In_Set(PSCAL,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PSCAL)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := PSCAL;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PSCAL);
                        end if;
                end if;

                if(Is_In_Set(PZERO,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PZERO)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := PZERO;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PZERO);
                        end if;
                end if;

        return IdxKey;
        end Get;

-- ---------------------------------------
-- From FA_Extension:
--

-- FA_Extension.ads had:


        -- Reserved keys

        type Res_Key_Arr is array (Positive range <>) of Reserved_Key;

        type Key_Rec is
                record
                        Key   : Reserved_Key;
                        Value : String(1..20);
                end record;
        type Key_Rec_Arr is array (Positive range <>) of Key_Rec;

        -- Reserved indexed keys 

        type Reserved_Root is (TTYPE,TUNIT,TSCAL,TZERO,TNULL,TDISP);

        type Res_Root_Arr  is array (Natural range <>) of Reserved_Root;

        type IdxKey_Rec is
                record
                        Root : Reserved_Root;
                        Arr  : TFIELDS_Arr;
                end record;
        type IdxKey_Rec_Arr is array (Natural range <>) of IdxKey_Rec;

        function Get(Keys  : in Res_Key_Arr)  return Key_Rec_Arr;
        function Get(Roots : in Res_Root_Arr) return IdxKey_Rec_Arr;


-- FA_Extension.adb had:


-- Reserved (optional) keys in any Conforming Extension
type ConfExt_Type is
        record
                EXTNAME  : CardValue;
                EXTVER   : CardValue;
                EXTLEVEL : CardValue;
        end record;

InitConfExt : ConfExt_Type := (InitVal, InitVal, InitVal);

-- Reserved (optional) keys related to TABLEs
type Tab_Type is
        record
                TTYPEn : TFIELDS_Arr;
                TUNITn : TFIELDS_Arr;
                TSCALn : TFIELDS_Arr;
                TZEROn : TFIELDS_Arr;
                TNULLn : TFIELDS_Arr;
                TDISPn : TFIELDS_Arr;
        end record;

InitTFIELDSArr : constant TFIELDS_Arr := (others => InitVal);

InitTab : Tab_Type := (others => InitTFIELDSArr);

type BinTab_Type is
        record
                TDIMn : TFIELDS_Arr;
                THEAP : CardValue;
        end record;

InitBinTab : BinTab_Type := (InitTFIELDSArr, InitVal);

-- Get funcs to return optional/reserved keys:





 function Needed_Count(Keys : in Res_Key_Arr) return Natural
 is
         Count : Natural := 0;
 begin

         for I in Keys'Range
         loop
                 case(Keys(I)) is
                        when DATE .. DATAMIN =>

                        if(State.Res.Comm(Keys(I)).Read)
                        then
                                Count := Count + 1;
                        end if;


                        when EXTNAME .. THEAP =>

                        if(State.Res.Ext(Keys(I)).Read)
                        then
                                Count := Count + 1;
                        end if;

--                      when others =>  null;
                end case;
        end loop;

        return Count;

 end Needed_Count;

 function Get(Keys : in Res_Key_Arr) return Key_Rec_Arr
 is
         FoundCount : Natural := Needed_Count(Keys);
         OutKeys : Key_Rec_Arr(1..FoundCount);
         Idx : Positive := 1;
 begin

        for I in Keys'Range
        loop
                case(Keys(I)) is
                        when DATE .. DATAMIN =>

                        if(State.Res.Comm(Keys(I)).Read)
                        then
                            OutKeys(Idx).Key   := Keys(I);
                            OutKeys(Idx).Value := State.Res.Comm(Keys(I)).Value;
                            exit when (Idx = FoundCount);
                            Idx := Idx + 1;
                        end if;

                when EXTNAME .. THEAP =>

                        if(State.Res.Ext(Keys(I)).Read)
                        then
                                OutKeys(Idx).Key   := Keys(I);
                                OutKeys(Idx).Value := State.Res.Ext(Keys(I)).Value;
                                exit when (Idx = FoundCount);
                                Idx := Idx + 1;
                        end if;

--                      when others => null;
                end case;

        end loop;

        return OutKeys;
 end Get;


        -- experimental Get(RootArr) to return Tab_Type's read arrays

        -- FIXME modify so that Arr can be shorter, containing only the Read elements
        function Is_Any_Element_Read(Arr : TFIELDS_Arr) return Boolean
        --function Is_Any_Element_Read(Arr : Reserved.TFIELDS_Arr) return Boolean
        is
                Is_Read : Boolean := False;
                -- FIXME what if Arr is empty array ? raise exception or return False
        begin
                for I in Arr'Range
                loop
                        Is_Read := Arr(I).Read;
                        exit when Arr(I).Read;
                end loop;
                return Is_Read;
        end Is_Any_Element_Read;


        function Is_In_Set(Root : Reserved_Root; Roots : Res_Root_Arr) return Boolean
        is
        begin
                for I in Roots'Range
                loop
                        if(Root = Roots(I))
                        then
                                return True;
                        end if;
                end loop;
                return False;
        end Is_In_Set;

        function Needed_Length(Roots : Res_Root_Arr) return Natural
        is
                Len : Natural := 0;
        begin
                if(Is_In_Set(TTYPE,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TTYPE))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(TUNIT,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TUNIT))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(TSCAL,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TSCAL))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(TZERO,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TZERO))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(TNULL,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TNULL))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(TDISP,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TDISP))) then Len := Len + 1; end if;
                end if;
                return Len;
        end Needed_Length;


        function Get(Roots : Res_Root_Arr) return IdxKey_Rec_Arr
        is
                IdxKey : IdxKey_Rec_Arr(1..Needed_Length(Roots));
                Len : Natural := 0;-- FIXME rename to Idx
        begin
                if(Is_In_Set(TTYPE,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TTYPE)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TTYPE;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TTYPE);
                        end if;
                end if;

                if(Is_In_Set(TUNIT,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TUNIT)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TUNIT;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TUNIT);
                        end if;
                end if;

                if(Is_In_Set(TSCAL,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TSCAL)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TSCAL;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TSCAL);
                        end if;
                end if;

                if(Is_In_Set(TZERO,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TZERO)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TZERO;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TZERO);
                        end if;
                end if;

                if(Is_In_Set(TNULL,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TNULL)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TNULL;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TNULL);
                        end if;
                end if;

                if(Is_In_Set(TDISP,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.TDISP)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := TDISP;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.TDISP);
                        end if;
                end if;

        return IdxKey;
        end Get;

-- DBG_Print funcs from fa_extension:

        procedure DBG_Print_Reserved
        is
        Res : Key_Rec_Arr := Get((EXTNAME,DATAMAX,DATAMIN,INSTRUME,TELESCOP));
        begin
        TIO.Put_Line("DBG_Print_reserved");
        for I in Res'Range
        loop
        TIO.Put("Get Res> "&Reserved_Key'Image(Res(I).Key));
        TIO.Put(" : "&Res(I).Value);
        TIO.New_Line;
        end loop;


        end DBG_Print_Reserved;


procedure DBG_Print(IdxKeys : IdxKey_Rec_Arr)
        is
        begin
        TIO.Put_Line("DBG_Print IdxKeys");
        for I in IdxKeys'Range
        loop
        TIO.Put("Get ResArr "& Reserved_Root'Image(IdxKeys(I).Root)&" ");
        for Idx in IdxKeys(I).Arr'Range
        loop
        TIO.Put(Integer'Image(Idx) &":"& IdxKeys(I).Arr(Idx).Value);
        end loop;
        TIO.New_Line;
        end loop;
        end DBG_Print;

















end Reserved;

