
with Ada.Text_IO;            use Ada.Text_IO;

procedure Sieve is

    procedure Print_Primes (N : Integer) is
        J, Prime    : Natural;

        type Ftype is Array (0 .. N) of Boolean;
        Flags : Ftype;
    begin
        for I in 0 .. N loop
            Flags (I)    := False;
        end loop;

        for I in 0 .. N loop
            if not Flags (I) then
                Prime        := I + 2;
                J            := I + Prime;
--                Put_Line (Integer'Image (Prime));
                while J <= N loop
                    Flags (J)    := True;
                    J            := J + Prime;
                end loop;
            end if;
        end loop;
    end Print_Primes;

begin
    Print_Primes (1000000);
end Sieve;