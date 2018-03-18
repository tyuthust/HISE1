with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Measures;use Measures;
with ICD; use ICD;

procedure Main is
   -- Stores some history information on measured heart rate
   Hrh: HeartRateHistory;
   Icd: ICDType;
   
   HeartRate:BPM:=99;
   Output:Boolean:=False;
begin
--     Output:=isTachycardia(HeartRate);
--     Put("The patient is Tachycardia: ");
--     New_Line;
--     Put(Boolean'Image(Output));
   Init(Hrh         => Hrh,
        Icd => Icd);
   Put(Icd.HealthType'Image);
   New_Line;
   Put(Boolean'Image(Icd.IsOn));
   New_Line;
   On(Icd);
   Put(Icd.HealthType'Image);
   New_Line;
   Put(Boolean'Image(Icd.IsOn));
   New_Line;
   Off(Icd);
   Put(Icd.HealthType'Image);
   New_Line;
   Put(Boolean'Image(Icd.IsOn));
--     updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 99,
--                            CurrentTime => 0);
--     updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 109,
--                            CurrentTime => 1);
--        updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 99,
--                               CurrentTime => 2);
--        updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 109,
--                               CurrentTime => 3);
--        updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 99,
--                               CurrentTime => 4);
--        updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 109,
--                               CurrentTime => 5);
--        updateHeartRateHistory(Hrh         => Hrh,
--                            HeartRate   => 99,
--                               CurrentTime => 6);
--     Output:=isVentricleFibrillation(Hrh);
--     Put("The patient is VentricleFibrillation: ");
--     New_Line;
--     Put(Boolean'Image(Output));
end Main;
