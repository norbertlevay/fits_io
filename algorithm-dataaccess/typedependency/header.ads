


package Header is

subtype String_20 is String(1..20);

type Linear_Conv_Rec(BLANK_Valid : Boolean) is
    record
        A,B   : String_20;
        case(BLANK_Valid) is
            when False => null;
            when True  => BLANK : String_20;
        end case;
    end record;

end Header;
