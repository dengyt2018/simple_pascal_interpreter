program Main;

procedure Alpha(a : integer; b : integer);
var x : integer;
var s : string;

   procedure Beta(a : integer; b : integer);
   var x : integer;
   begin
      x := a * 10 + b * 2;  {Beta x}
   end;

begin
   x := (a + b ) * 2;  {Alpha x}

   Beta(5, 10);      { procedure call }
end;

begin { Main }

   Alpha(3 + 5, 7);  { procedure call }
   s := "hello world!";
end.  { Main }
