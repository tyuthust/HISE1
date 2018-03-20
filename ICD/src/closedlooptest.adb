with ClosedLoop;
use ClosedLoop;

procedure ClosedLoopTest is

begin
   ClosedLoop.Init;
   for I in Integer range 0..500  loop
      ClosedLoop.Tick;
   end loop;

end ClosedLoopTest;
