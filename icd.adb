with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures;
with Heart; use Heart;

package body ICD is
   function isTachycardia(HeartRate: in BPM) return Boolean is
      HeartRateThreshold: Constant Integer:=100;
      Output: Boolean:=False;
   begin
      if(HeartRate>HeartRateThreshold) then
         Output:= True;
      else
         Output:= False;
      end if;
      return Output;
   end isTachycardia;

   function isVentricleFibrillation(Hrh: in HeartRateHistory) return Boolean is
      Sum: Integer := 0;
      Temp_Minus: Integer;
      Output: Boolean:=False;
      begin
      for I in Integer range 1..Hrh'Last loop
         Temp_Minus:= Hrh(I).Rate-Hrh(I-1).Rate;
--           Put("Temp_Minus is :");
--           New_Line;
--           Put(Temp_Minus);
--           New_Line;
--           Put("Time:");
--           Put(Integer(Hrh(I).Time));
--           New_Line;
         if Temp_Minus>=0 then
            Sum:=Sum+Temp_Minus;
         else
            Sum:=Sum-Temp_Minus;
         end if;
      end loop;
--          Put("The sum is: ");
--          New_Line;
--          Put(Sum);
--          New_Line;
      if Sum/6>=10 then
         Output:=True;
         end if;
      return Output;
      end isVentricleFibrillation;

   procedure updateHeartRateHistory(Hrh: in out HeartRateHistory; HeartRate: in BPM; CurrentTime: in TickCount) is
      HistoryPro: Integer:= Hrh'Last;
   begin
--        Put("HistoryPro");
--        Put(HistoryPro);
--        New_Line;
      while HistoryPro>Hrh'First loop
         Hrh(HistoryPro):=(Rate => Hrh(HistoryPro-1).Rate,
                           Time => Hrh(HistoryPro-1).Time);
         HistoryPro:=HistoryPro-1;
      end loop;
      Hrh(Hrh'First):=(Rate => HeartRate, Time => CurrentTime);
   end updateHeartRateHistory;

   procedure initHRH(Hrh: in out HeartRateHistory) is
      DefaultHR:BPM:=-1;
   begin
      for I in Integer range 0..Hrh'Last loop
         Hrh(I).Time:=TickCount(TickCount'First);
         Hrh(I).Rate:=DefaultHR;
      end loop;
   end initHRH;
end ICD;
