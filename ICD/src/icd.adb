with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures;
with Heart; use Heart;
with Network;
with ImpulseGenerator;

package body ICD is
   -- The initial upper bound for tachycardia when the system starts
   TachyBound: Measures.BPM:=100;

   -- The initial number of joules to deliver in the case of a ventricle fibrillation
   JoulesToDeliver: Measures.Joules:=30;

   -- The constant of how much the given impulse rate should be greater than the current heart rate
   AboveHeartRate: Constant Measures.BPM:=15;

   -- The init counter records how many ticks after the icd has been init
   InitCounter: Integer:=-1;

   -- The variable records the current heart rate when tachycardia is detected
   CurrentHeartRate:Measures.BPM:=0;

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
         if Temp_Minus>=0 then
            Sum:=Sum+Temp_Minus;
         else
            Sum:=Sum-Temp_Minus;
         end if;
      end loop;
      -- When the tick is less than 6 after then icd is inited, the data is too little to consider whether there is ventricle fibrillation
      if Sum/6>=10 and InitCounter>6 then
--        if Sum/6>=10 then
         Output:=True;
      end if;
      return Output;
   end isVentricleFibrillation;


   function changeTachycardiaUpperBoundSet(TachyB: in Measures.BPM; Icd: in ICDType) return Boolean is
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

   function readSet(TachyB: out Measures.BPM;JoulesToD: out Measures.Joules;Icd: in ICDType) return Boolean is
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
      InitCounter:=0;
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

   procedure activeWhenTachycardia(ImpulseGeneratorcounter: in out Integer;
                                   Generator:in out ImpulseGenerator.GeneratorType;
                                   HeartRate : in Measures.BPM;
                                   ActiveFlag: in out Boolean;
                                   TickTimesInOneMinute: in Integer;
                                   ImpulseTickFloat:in out Float;
                                   ImpulseTickFlag: in out Boolean) is
      TachyJoules: Measures.Joules:=2;
      TempHeartRate: Measures.BPM;
      ReciprocalTickFloat: Float range 0.00..1.00 :=0.00;
   begin
      if ImpulseGeneratorcounter>0 and ImpulseTickFlag=True then
--        if ImpulseGeneratorcounter>0 then
         -- Set the Joules of the impulse deliver to haert when Tachycardia
         ImpulseGenerator.SetImpulse(Generator,TachyJoules);
         -- Set the heart rate as the current heart rate plus above heart rate(15)
         TempHeartRate:=HeartRate+AboveHeartRate;
         PUT("ImpulseGenerator: ");
         PUT(ImpulseGeneratorcounter);
         New_Line;
         ReciprocalTickFloat:=Float(TempHeartRate)/Float(TickTimesInOneMinute);
--           PUT("TempHeartrate/TickTimesInOneMinute : ");
--           PUT(ReciprocalTickFloat'Image);
--           New_Line;
         -- Add the temp reciprocalTick in the Impulse tick progress
         ImpulseTickFloat:=ImpulseTickFloat+ReciprocalTickFloat;
         PUT("ImpulseTickFloat : ");
         PUT(ImpulseTickFloat'Image);
         New_Line;
         ImpulseGeneratorcounter:=ImpulseGeneratorcounter-1;
         ImpulseTickFlag:=False;
      else
         -- The treatment stops
         ActiveFlag:=False;
         -- Set the Joules of the impulse deliver to haert as 0 when Tachycardia treatment is done
         ImpulseGenerator.SetImpulse(Generator,J => 0);
         PUT("Reset the impulse: ");
         PUT(Generator.Impulse);
         New_Line;
         PUT("Done the Tachycardia treatment");
         New_Line;
      end if;
   end activeWhenTachycardia;


   procedure activeWhenVentricle_fibrillation(ImpulseGeneratorcounter: in out Integer;
                                              Generator:in out ImpulseGenerator.GeneratorType;
                                              ActiveFlag: in out Boolean) is
   begin
      if ImpulseGeneratorcounter>0 then
         ImpulseGenerator.SetImpulse(Generator,JoulesToDeliver);

         PUT("Signals Joules: ");
         PUT(JoulesToDeliver);
         New_Line;
         PUT("ImpulseGenerator: ");
         PUT(ImpulseGeneratorcounter);
         New_Line;
         ImpulseGeneratorcounter:=ImpulseGeneratorcounter-1;
      else
          -- The treatment stops
         ActiveFlag:=False;
         -- Set the Joules of the impulse deliver to haert as 0 when Tachycardia treatment is done
         ImpulseGenerator.SetImpulse(Generator,J => 0);
         PUT("Reset the impulse: ");
         PUT(Generator.Impulse);
         New_Line;
         PUT("Done the Vemtricle Fibrillation treatment");
         New_Line;
      end if;
   end activeWhenVentricle_fibrillation;


   procedure Tick(Icd: in out ICDType;
                  Hrh: in out HeartRateHistory;
                  HeartRate: in BPM;
                  CurrentTime:in TickCount;
                  Generator:in out ImpulseGenerator.GeneratorType;
                  TickTimesInOneMinute: in Integer;
                  ImpulseTickFloat: in out Float;
                  ImpulseGeneratorcounter: in out Integer;
                  ActiveFlag: in out Boolean;
                  ImpulseTickFlag: in out Boolean) is
   begin
      if Icd.IsOn then
         if InitCounter>=0 then
            InitCounter:=InitCounter+1;
         end if;

         -- update the current time and heart rate in the heart rate history array(hrh).
         updateHeartRateHistory(Hrh         => Hrh,
                                HeartRate   => HeartRate,
                                CurrentTime => CurrentTime);
         if isTachycardia(HeartRate => HeartRate) then
            -- When detected the tachycardia and it is not under a treatment, set the 10 signals and start the treatment
            if ActiveFlag=False then
               ImpulseGeneratorcounter:=10;
               ActiveFlag:=True;
               CurrentHeartRate:=HeartRate;
            end if;

            -- Set the health type as tachycardia
            Icd.HealthType:=Tachycardia;

            activeWhenTachycardia(ImpulseGeneratorcounter => ImpulseGeneratorcounter,
                                  Generator               => Generator ,
                                  TickTimesInOneMinute =>TickTimesInOneMinute,
                                  ImpulseTickFloat =>ImpulseTickFloat,
                                  ActiveFlag              => ActiveFlag,
                                  ImpulseTickFlag =>ImpulseTickFlag,
                                  HeartRate   => CurrentHeartRate);
         elsif isVentricleFibrillation(Hrh => Hrh) then
            -- When detected the tachycardia and it is not under a treatment, set the 1 signals and start the treatment
            if ActiveFlag=False then
               ImpulseGeneratorcounter:=1;
               ActiveFlag:=True;
            end if;

            -- Set the health type as ventricle fibrillation
            Icd.HealthType:=Ventricle_fibrillation;
            activeWhenVentricle_fibrillation(ImpulseGeneratorcounter => ImpulseGeneratorcounter,
                                             Generator               => Generator,
                                             ActiveFlag              => ActiveFlag);
         else
            -- Set the health type as healthy
            Icd.HealthType:=Healthy;
         end if;
      end if;
   end Tick;
end ICD;
