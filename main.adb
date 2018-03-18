with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Measures;use Measures;

  
with ICD; use ICD;

procedure Main is
   -- Stores some history information on measured heart rate
   Hrh: HeartRateHistory;

   
   HeartRate:BPM:=99;
   Output:Boolean:=False;
begin
--     Output:=isTachycardia(HeartRate);
--     Put("The patient is Tachycardia: ");
--     New_Line;
--     Put(Boolean'Image(Output));
   initHRH(Hrh);
   updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 99,
                          CurrentTime => 0);
   updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 109,
                          CurrentTime => 1);
      updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 99,
                             CurrentTime => 2);
      updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 109,
                             CurrentTime => 3);
      updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 99,
                             CurrentTime => 4);
      updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 109,
                             CurrentTime => 5);
      updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => 99,
                             CurrentTime => 6);
   Output:=isVentricleFibrillation(Hrh);
   Put("The patient is VentricleFibrillation: ");
   New_Line;
   Put(Boolean'Image(Output));
end Main;
