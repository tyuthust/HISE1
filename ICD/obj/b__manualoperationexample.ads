pragma Warnings (Off);
pragma Ada_95;
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2017 (20170515-63)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_manualoperationexample" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#5b71c59c#;
   pragma Export (C, u00001, "manualoperationexampleB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0a55feef#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#87cd2ab9#;
   pragma Export (C, u00005, "ada__calendar__delaysB");
   u00006 : constant Version_32 := 16#b27fb9e9#;
   pragma Export (C, u00006, "ada__calendar__delaysS");
   u00007 : constant Version_32 := 16#0d7f1a43#;
   pragma Export (C, u00007, "ada__calendarB");
   u00008 : constant Version_32 := 16#5b279c75#;
   pragma Export (C, u00008, "ada__calendarS");
   u00009 : constant Version_32 := 16#85a06f66#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#1a0dcc03#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#32a08138#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#4e7785b8#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#ac24596d#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#4c8a8c47#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#30ad09e5#;
   pragma Export (C, u00018, "system__secondary_stackB");
   u00019 : constant Version_32 := 16#88327e42#;
   pragma Export (C, u00019, "system__secondary_stackS");
   u00020 : constant Version_32 := 16#f103f468#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#1f63cb3c#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#bc1fead0#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#6f0ee87a#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#5ac3ecce#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00028, "system__exceptions__machineB");
   u00029 : constant Version_32 := 16#047ef179#;
   pragma Export (C, u00029, "system__exceptions__machineS");
   u00030 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#4c2a78fc#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#307b61fa#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#6c825ffc#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#32fb7748#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#18d5fcc5#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#9df1ae6d#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00040, "ada__containersS");
   u00041 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00041, "ada__exceptions__tracebackB");
   u00042 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00042, "ada__exceptions__tracebackS");
   u00043 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00043, "system__bounded_stringsB");
   u00044 : constant Version_32 := 16#455da021#;
   pragma Export (C, u00044, "system__bounded_stringsS");
   u00045 : constant Version_32 := 16#42315736#;
   pragma Export (C, u00045, "system__crtlS");
   u00046 : constant Version_32 := 16#08e0d717#;
   pragma Export (C, u00046, "system__dwarf_linesB");
   u00047 : constant Version_32 := 16#b1bd2788#;
   pragma Export (C, u00047, "system__dwarf_linesS");
   u00048 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00048, "ada__charactersS");
   u00049 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00049, "ada__characters__handlingB");
   u00050 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00050, "ada__characters__handlingS");
   u00051 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00051, "ada__characters__latin_1S");
   u00052 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00052, "ada__stringsS");
   u00053 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00053, "ada__strings__mapsB");
   u00054 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00054, "ada__strings__mapsS");
   u00055 : constant Version_32 := 16#9dc9b435#;
   pragma Export (C, u00055, "system__bit_opsB");
   u00056 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00056, "system__bit_opsS");
   u00057 : constant Version_32 := 16#0626fdbb#;
   pragma Export (C, u00057, "system__unsigned_typesS");
   u00058 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00058, "ada__strings__maps__constantsS");
   u00059 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00059, "interfacesS");
   u00060 : constant Version_32 := 16#9f00b3d3#;
   pragma Export (C, u00060, "system__address_imageB");
   u00061 : constant Version_32 := 16#934c1c02#;
   pragma Export (C, u00061, "system__address_imageS");
   u00062 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00062, "system__img_unsB");
   u00063 : constant Version_32 := 16#99d2c14c#;
   pragma Export (C, u00063, "system__img_unsS");
   u00064 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00064, "system__ioB");
   u00065 : constant Version_32 := 16#ace27677#;
   pragma Export (C, u00065, "system__ioS");
   u00066 : constant Version_32 := 16#11faaec1#;
   pragma Export (C, u00066, "system__mmapB");
   u00067 : constant Version_32 := 16#08d13e5f#;
   pragma Export (C, u00067, "system__mmapS");
   u00068 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#9d8ecedc#;
   pragma Export (C, u00069, "system__mmap__os_interfaceB");
   u00070 : constant Version_32 := 16#8f4541b8#;
   pragma Export (C, u00070, "system__mmap__os_interfaceS");
   u00071 : constant Version_32 := 16#54632e7c#;
   pragma Export (C, u00071, "system__os_libB");
   u00072 : constant Version_32 := 16#ed466fde#;
   pragma Export (C, u00072, "system__os_libS");
   u00073 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00073, "system__case_utilB");
   u00074 : constant Version_32 := 16#16a9e8ef#;
   pragma Export (C, u00074, "system__case_utilS");
   u00075 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00075, "system__stringsB");
   u00076 : constant Version_32 := 16#4c1f905e#;
   pragma Export (C, u00076, "system__stringsS");
   u00077 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00077, "interfaces__cB");
   u00078 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00078, "interfaces__cS");
   u00079 : constant Version_32 := 16#d0bc914c#;
   pragma Export (C, u00079, "system__object_readerB");
   u00080 : constant Version_32 := 16#7f932442#;
   pragma Export (C, u00080, "system__object_readerS");
   u00081 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00081, "system__val_lliB");
   u00082 : constant Version_32 := 16#a8846798#;
   pragma Export (C, u00082, "system__val_lliS");
   u00083 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00083, "system__val_lluB");
   u00084 : constant Version_32 := 16#7cd4aac9#;
   pragma Export (C, u00084, "system__val_lluS");
   u00085 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00085, "system__val_utilB");
   u00086 : constant Version_32 := 16#9e0037c6#;
   pragma Export (C, u00086, "system__val_utilS");
   u00087 : constant Version_32 := 16#5bbc3f2f#;
   pragma Export (C, u00087, "system__exception_tracesB");
   u00088 : constant Version_32 := 16#167fa1a2#;
   pragma Export (C, u00088, "system__exception_tracesS");
   u00089 : constant Version_32 := 16#d178f226#;
   pragma Export (C, u00089, "system__win32S");
   u00090 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00090, "system__wch_conB");
   u00091 : constant Version_32 := 16#29dda3ea#;
   pragma Export (C, u00091, "system__wch_conS");
   u00092 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00092, "system__wch_stwB");
   u00093 : constant Version_32 := 16#04cc8feb#;
   pragma Export (C, u00093, "system__wch_stwS");
   u00094 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00094, "system__wch_cnvB");
   u00095 : constant Version_32 := 16#266a1919#;
   pragma Export (C, u00095, "system__wch_cnvS");
   u00096 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00096, "system__wch_jisB");
   u00097 : constant Version_32 := 16#a61a0038#;
   pragma Export (C, u00097, "system__wch_jisS");
   u00098 : constant Version_32 := 16#a99e1d66#;
   pragma Export (C, u00098, "system__os_primitivesB");
   u00099 : constant Version_32 := 16#b82f904e#;
   pragma Export (C, u00099, "system__os_primitivesS");
   u00100 : constant Version_32 := 16#b6166bc6#;
   pragma Export (C, u00100, "system__task_lockB");
   u00101 : constant Version_32 := 16#532ab656#;
   pragma Export (C, u00101, "system__task_lockS");
   u00102 : constant Version_32 := 16#1a9147da#;
   pragma Export (C, u00102, "system__win32__extS");
   u00103 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00103, "system__tracesB");
   u00104 : constant Version_32 := 16#c0bde992#;
   pragma Export (C, u00104, "system__tracesS");
   u00105 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00105, "ada__integer_text_ioB");
   u00106 : constant Version_32 := 16#b85ee1d1#;
   pragma Export (C, u00106, "ada__integer_text_ioS");
   u00107 : constant Version_32 := 16#1d1c6062#;
   pragma Export (C, u00107, "ada__text_ioB");
   u00108 : constant Version_32 := 16#95711eac#;
   pragma Export (C, u00108, "ada__text_ioS");
   u00109 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00109, "ada__streamsB");
   u00110 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00110, "ada__streamsS");
   u00111 : constant Version_32 := 16#d85792d6#;
   pragma Export (C, u00111, "ada__tagsB");
   u00112 : constant Version_32 := 16#8813468c#;
   pragma Export (C, u00112, "ada__tagsS");
   u00113 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00113, "system__htableB");
   u00114 : constant Version_32 := 16#b66232d2#;
   pragma Export (C, u00114, "system__htableS");
   u00115 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00115, "system__string_hashB");
   u00116 : constant Version_32 := 16#143c59ac#;
   pragma Export (C, u00116, "system__string_hashS");
   u00117 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00117, "system__val_unsB");
   u00118 : constant Version_32 := 16#168e1080#;
   pragma Export (C, u00118, "system__val_unsS");
   u00119 : constant Version_32 := 16#4c01b69c#;
   pragma Export (C, u00119, "interfaces__c_streamsB");
   u00120 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00120, "interfaces__c_streamsS");
   u00121 : constant Version_32 := 16#6f0d52aa#;
   pragma Export (C, u00121, "system__file_ioB");
   u00122 : constant Version_32 := 16#95d1605d#;
   pragma Export (C, u00122, "system__file_ioS");
   u00123 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00123, "ada__finalizationS");
   u00124 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00124, "system__finalization_rootB");
   u00125 : constant Version_32 := 16#7d52f2a8#;
   pragma Export (C, u00125, "system__finalization_rootS");
   u00126 : constant Version_32 := 16#cf3f1b90#;
   pragma Export (C, u00126, "system__file_control_blockS");
   u00127 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00127, "ada__text_io__integer_auxB");
   u00128 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00128, "ada__text_io__integer_auxS");
   u00129 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00129, "ada__text_io__generic_auxB");
   u00130 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00130, "ada__text_io__generic_auxS");
   u00131 : constant Version_32 := 16#b10ba0c7#;
   pragma Export (C, u00131, "system__img_biuB");
   u00132 : constant Version_32 := 16#c00475f6#;
   pragma Export (C, u00132, "system__img_biuS");
   u00133 : constant Version_32 := 16#4e06ab0c#;
   pragma Export (C, u00133, "system__img_llbB");
   u00134 : constant Version_32 := 16#81c36508#;
   pragma Export (C, u00134, "system__img_llbS");
   u00135 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00135, "system__img_lliB");
   u00136 : constant Version_32 := 16#23efd4e9#;
   pragma Export (C, u00136, "system__img_lliS");
   u00137 : constant Version_32 := 16#a756d097#;
   pragma Export (C, u00137, "system__img_llwB");
   u00138 : constant Version_32 := 16#28af469e#;
   pragma Export (C, u00138, "system__img_llwS");
   u00139 : constant Version_32 := 16#eb55dfbb#;
   pragma Export (C, u00139, "system__img_wiuB");
   u00140 : constant Version_32 := 16#ae45f264#;
   pragma Export (C, u00140, "system__img_wiuS");
   u00141 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00141, "system__val_intB");
   u00142 : constant Version_32 := 16#7a05ab07#;
   pragma Export (C, u00142, "system__val_intS");
   u00143 : constant Version_32 := 16#34a19a2a#;
   pragma Export (C, u00143, "heartB");
   u00144 : constant Version_32 := 16#34b37eac#;
   pragma Export (C, u00144, "heartS");
   u00145 : constant Version_32 := 16#17bda97a#;
   pragma Export (C, u00145, "measuresB");
   u00146 : constant Version_32 := 16#ab0ff7ab#;
   pragma Export (C, u00146, "measuresS");
   u00147 : constant Version_32 := 16#1d885ae5#;
   pragma Export (C, u00147, "randomnumberB");
   u00148 : constant Version_32 := 16#f8c8aef0#;
   pragma Export (C, u00148, "randomnumberS");
   u00149 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00149, "ada__numericsS");
   u00150 : constant Version_32 := 16#03e83d1c#;
   pragma Export (C, u00150, "ada__numerics__elementary_functionsB");
   u00151 : constant Version_32 := 16#c6ca7006#;
   pragma Export (C, u00151, "ada__numerics__elementary_functionsS");
   u00152 : constant Version_32 := 16#e5114ee9#;
   pragma Export (C, u00152, "ada__numerics__auxB");
   u00153 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00153, "ada__numerics__auxS");
   u00154 : constant Version_32 := 16#36373acb#;
   pragma Export (C, u00154, "system__fat_llfS");
   u00155 : constant Version_32 := 16#5fc82639#;
   pragma Export (C, u00155, "system__machine_codeS");
   u00156 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00156, "system__exn_llfB");
   u00157 : constant Version_32 := 16#8ede3ae4#;
   pragma Export (C, u00157, "system__exn_llfS");
   u00158 : constant Version_32 := 16#6ad59d2c#;
   pragma Export (C, u00158, "system__fat_fltS");
   u00159 : constant Version_32 := 16#d976e2b4#;
   pragma Export (C, u00159, "ada__numerics__float_randomB");
   u00160 : constant Version_32 := 16#62aa8dd2#;
   pragma Export (C, u00160, "ada__numerics__float_randomS");
   u00161 : constant Version_32 := 16#d34f9f29#;
   pragma Export (C, u00161, "system__random_numbersB");
   u00162 : constant Version_32 := 16#f1b831a2#;
   pragma Export (C, u00162, "system__random_numbersS");
   u00163 : constant Version_32 := 16#40a8df0e#;
   pragma Export (C, u00163, "system__random_seedB");
   u00164 : constant Version_32 := 16#69b0a863#;
   pragma Export (C, u00164, "system__random_seedS");
   u00165 : constant Version_32 := 16#d8663c70#;
   pragma Export (C, u00165, "hrmB");
   u00166 : constant Version_32 := 16#33bceb74#;
   pragma Export (C, u00166, "hrmS");
   u00167 : constant Version_32 := 16#65159e11#;
   pragma Export (C, u00167, "icdB");
   u00168 : constant Version_32 := 16#18423bfc#;
   pragma Export (C, u00168, "icdS");
   u00169 : constant Version_32 := 16#dcfac860#;
   pragma Export (C, u00169, "impulsegeneratorB");
   u00170 : constant Version_32 := 16#057156a5#;
   pragma Export (C, u00170, "impulsegeneratorS");
   u00171 : constant Version_32 := 16#8b74a921#;
   pragma Export (C, u00171, "networkB");
   u00172 : constant Version_32 := 16#ed9a5ee9#;
   pragma Export (C, u00172, "networkS");
   u00173 : constant Version_32 := 16#9094876d#;
   pragma Export (C, u00173, "ada__assertionsB");
   u00174 : constant Version_32 := 16#1a0b0d2c#;
   pragma Export (C, u00174, "ada__assertionsS");
   u00175 : constant Version_32 := 16#52f1910f#;
   pragma Export (C, u00175, "system__assertionsB");
   u00176 : constant Version_32 := 16#ff2dadac#;
   pragma Export (C, u00176, "system__assertionsS");
   u00177 : constant Version_32 := 16#2bf99b12#;
   pragma Export (C, u00177, "principalB");
   u00178 : constant Version_32 := 16#ba98eec4#;
   pragma Export (C, u00178, "principalS");
   u00179 : constant Version_32 := 16#18e0e51c#;
   pragma Export (C, u00179, "system__img_enum_newB");
   u00180 : constant Version_32 := 16#53ec87f8#;
   pragma Export (C, u00180, "system__img_enum_newS");
   u00181 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00181, "system__img_boolB");
   u00182 : constant Version_32 := 16#c779f0d3#;
   pragma Export (C, u00182, "system__img_boolS");
   u00183 : constant Version_32 := 16#ee101ba4#;
   pragma Export (C, u00183, "system__memoryB");
   u00184 : constant Version_32 := 16#6bdde70c#;
   pragma Export (C, u00184, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.val_util%s
   --  system.standard_library%s
   --  system.exception_traces%s
   --  ada.exceptions%s
   --  system.wch_stw%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.os_lib%s
   --  system.bit_ops%s
   --  ada.characters.handling%s
   --  ada.exceptions.traceback%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.containers%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.bounded_strings%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.wch_stw%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  interfaces.c%s
   --  system.win32%s
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap.os_interface%b
   --  system.mmap%b
   --  system.os_lib%b
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%b
   --  ada.exceptions.traceback%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.bounded_strings%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  system.object_reader%s
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  interfaces.c%b
   --  ada.strings.maps%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.object_reader%b
   --  ada.numerics%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  ada.numerics.elementary_functions%s
   --  ada.numerics.elementary_functions%b
   --  system.task_lock%s
   --  system.task_lock%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_int%s
   --  system.val_int%b
   --  system.win32.ext%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  system.assertions%s
   --  system.assertions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  ada.numerics.float_random%s
   --  ada.numerics.float_random%b
   --  measures%s
   --  measures%b
   --  principal%s
   --  principal%b
   --  randomnumber%s
   --  randomnumber%b
   --  heart%s
   --  heart%b
   --  hrm%s
   --  hrm%b
   --  impulsegenerator%s
   --  impulsegenerator%b
   --  network%s
   --  network%b
   --  icd%s
   --  icd%b
   --  manualoperationexample%b
   --  END ELABORATION ORDER

end ada_main;
