with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures;
with Heart; use Heart;
with Network;
with ImpulseGenerator;

package body ICD is
   -- The initial upper bound for tachycardia when the system starts
   TachyBound: Integer:=100;

   -- The initial number of joules to deliver in the case of a ventricle fibrillation
   JoulesToDeliver: Measures.Joules:=30;

   -- The constant of how much the given impulse rate should be greater than the current heart rate
   AboveHeartRate: Constant Measures.BPM:=15;

   function isTachycardia(HeartRate: in BPM) return Boolean is
      Output: Boolean:=False;
   begin
      if(HeartRate>TachyBound) then
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

   function changeTachycardiaUpperBoundSet(TachyB: in Integer;Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         TachyBound:=TachyB;
         return True;
      end if;
   end changeTachycardiaUpperBoundSet;

   function changeJoulesDeliverNumForVentricle_fibrillation(JoulesToD: in Measures.Joules;Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         JoulesToDeliver:=JoulesToD;
         return True;
      end if;
   end changeJoulesDeliverNumForVentricle_fibrillation;

   function readSet(TachyB: out Integer;JoulesToD: out Measures.Joules;Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         TachyB:=TachyBound;
         JoulesToD:=JoulesToDeliver;
         return True;
      end if;
   end readSet;


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

   procedure Init(Hrh: in out HeartRateHistory;Icd: out ICDType) is
      DefaultHR:BPM:=-1;
   begin
      for I in Integer range 0..Hrh'Last loop
         Hrh(I).Time:=TickCount(TickCount'First);
         Hrh(I).Rate:=DefaultHR;
      end loop;
      Icd.HealthType:=Healthy;
      Icd.IsOn:=False;
   end Init;

   procedure On(Icd: out ICDType) is
   begin
      Icd.IsOn:=True;
   end On;

   procedure Off(Icd: out ICDType) is
   begin
      Icd.IsOn:=False;
   end Off;

   function IsOn(Icd : in ICDType) return Boolean is
   begin
      return Icd.IsOn;
   end IsOn;

   procedure activeWhenTachycardia(ImpulseGeneratorcounter: out Integer;Generator:in out ImpulseGenerator.GeneratorType; Hrt : in out Heart.HeartType) is
      HeartRate: Measures.BPM;
      TachyJoules: Measures.Joules:=2;
   begin
      -- Set the Joules of the impulse deliver to haert when Tachycardia
      ImpulseGenerator.SetImpulse(Generator,TachyJoules);
      -- Set the
      ImpulseGeneratorcounter:=10;
      Heart.GetRate(Heart => Hrt,
                    Rate  => HeartRate);
      HeartRate:=HeartRate+AboveHeartRate;
      Hrt.Rate:=HeartRate;
   end activeWhenTachycardia;


   procedure activeWhenVentricle_fibrillation(ImpulseGeneratorcounter: in out Integer;Generator:in out ImpulseGenerator.GeneratorType) is
   begin
      ImpulseGeneratorcounter:=1;
      ImpulseGenerator.SetImpulse(Generator,JoulesToDeliver);
   end activeWhenVentricle_fibrillation;


   procedure Tick(Icd: in out ICDType;Hrh: in out HeartRateHistory;HeartRate: in BPM;CurrentTime:in TickCount;Generator:in out ImpulseGenerator.GeneratorType;Hrt : in out Heart.HeartType;ImpulseGeneratorcounter: in out Integer;ActiveFlag: in out Boolean) is
   begin
      if Icd.IsOn then
         updateHeartRateHistory(Hrh         => Hrh,
                          HeartRate   => HeartRate,
                               CurrentTime => CurrentTime);
         if isTachycardia(HeartRate => HeartRate) then
            New_Line;
            Icd.HealthType:=Tachycardia;
            if ActiveFlag=False then
               activeWhenTachycardia(ImpulseGeneratorcounter => ImpulseGeneratorcounter,
                                     Generator               => Generator ,
                                     Hrt                     => Hrt);
               ActiveFlag:=True;
            end if;
         elsif isVentricleFibrillation(Hrh => Hrh) then
            if CurrentTime>6 then
               if ActiveFlag=False then
                  Icd.HealthType:=Ventricle_fibrillation;
                  activeWhenVentricle_fibrillation(ImpulseGeneratorcounter => ImpulseGeneratorcounter,
                                                   Generator               => Generator);
                  ActiveFlag:=True;
               end if;
            end if;
         else
            Icd.HealthType:=Healthy;
         end if;
      end if;
--        Put(Icd.HealthType'Image);
--        New_Line;
   end Tick;


end ICD;
