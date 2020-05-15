


package Header is

subtype String_20 is String(1..20);

type Linear_Conv_Rec is
    record
        A,B   : String_20;
        BV : Boolean;
        BLANK : String_20;
    end record;

end Header;
