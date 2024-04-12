with Ada.Text_IO;
with Raylib; use Raylib;
with Raylib.GUI; use Raylib.GUI;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure GUI_Example is

   screenWidth  : constant := 960;
   screenHeight : constant := 560;

   Force_Squared_Check : aliased C_bool := False;
   Unused : int;
   Spinner_001_Value : aliased int := 0;
   Spinner_Edit_Mode : C_bool := False;

   Valuebox_002_Value : aliased int := 0;
   Valuebox_Edit_Mode : C_bool := False;

   Textbox_Text      : aliased char_array := To_C ("Text Box",
                                                   Append_Nul => True);
   Textbox_Edit_Mode : C_bool := False;

   Show_Textinput_Box : C_bool := False;

   Prev_Visual_Style, Visual_Style_Active : aliased int := 0;

   Dropdown_000_Edit_Mode : C_bool := False;
   Dropdown_000_Active : aliased int := 0;
   Dropdown_001_Edit_Mode : C_bool := False;
   Dropdown_001_Active : aliased int := 0;

   ListViewScrollIndex : aliased int := 0;
   ListViewActive : aliased int := 0;

   listViewExList : aliased constant chars_ptr_array :=
     (New_String ("This"),
      New_String ("is"),
      New_String ("a"),
      New_String ("list view"),
      New_String ("with"),
      New_String ("disable"),
      New_String ("elements"),
      New_String ("amazing!"));

   ListViewExScrollIndex : aliased int := 0;
   ListViewExActive : aliased int := 2;
   ListViewExFocus : aliased int := -1;

   ToggleGroupActive : aliased int := 0;
   ToggleSliderActive : aliased int := 1;

   ColorPickerValue : aliased Color;

   SliderValue : aliased C_float := 0.0;
   SliderBarValue : aliased C_float := 0.0;

   ProgressValue : aliased C_float := 0.0;

   ViewScroll : aliased Vector2 := (0.0, 0.0);
   View : aliased Rectangle := (others => 0.0);

   MouseCell : aliased Vector2 := (0.0, 0.0);

   AlphaValue : aliased C_float := 1.0;

   TextBoxMultiText : aliased char_array :=
     To_C ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, " &
             "sed do eiusmod tempor incididunt ut labore et dolore magna " &
             "aliqua. Ut enim ad minim veniam, quis nostrud exercitation " &
             "ullamco laboris nisi ut aliquip ex ea commodo consequat.\n\n" &
             "Duis aute irure dolor in reprehenderit in voluptate velit " &
             "esse cillum dolore eu fugiat nulla pariatur.\n\n" &
             "Thisisastringlongerthanexpectedwithoutspacestotestcharbrea" &
             "ksforthosecases,checkingifworkingasexpected.\n\nExcepteur " &
             "sint occaecat cupidatat non proident, sunt in culpa qui " &
             "officia deserunt mollit anim id est laborum.",
           Append_Nul => True);
   TextBoxMultiEditMode : aliased C_bool := False;

   Show_Message_Box : Boolean := False;
   Exit_Window : C_bool := False;

   Text_Input : aliased char_array := (1 .. 256 => nul);
   Text_Input_Filename : aliased char_array := (1 .. 256 => nul);

begin
   Raylib.InitWindow (screenWidth, screenHeight, "Example window");

   Raylib.InitAudioDevice;

   Raylib.SetTargetFPS (60);
   Raylib.EnableCursor;
   Raylib.SetExitKey (0);

   while not Exit_Window loop

      Exit_Window := WindowShouldClose;

      if IsKeyPressed (KEY_ESCAPE) then
         Show_Message_Box := not Show_Message_Box;
      end if;

      if IsKeyPressed (KEY_LEFT) then
         ProgressValue := ProgressValue - 0.1;
      elsif IsKeyPressed (KEY_RIGHT) then
         ProgressValue := ProgressValue + 0.1;
      end if;

      if IsKeyDown (KEY_LEFT_CONTROL) and then IsKeyPressed (KEY_S) then
         Show_Textinput_Box := True;
      end if;

      ProgressValue := C_float'Min (1.0, C_float'Max (0.0, ProgressValue));

      if Prev_Visual_Style /= Visual_Style_Active then
         case Visual_Style_Active is
            when others => null;
         end case;

         Prev_Visual_Style := Visual_Style_Active;
      end if;

      if IsFileDropped then
         declare
            Dropped_Files : constant FilePathList := LoadDroppedFiles;
         begin
            if Dropped_Files.count > 0 then
               Ada.Text_IO.Put_Line (Value (Dropped_Files.paths (0)));
               GuiLoadStyle (Dropped_Files.paths (0));
            else
               Ada.Text_IO.Put_Line ("Dropped files count is ZERO...");
            end if;
            UnloadDroppedFiles (Dropped_Files);
         end;
      end if;

      GuiSetAlpha (AlphaValue);

      Raylib.BeginDrawing;

      ClearBackground
        (GetColor (GuiGetStyle (DEFAULT, BACKGROUND_COLOR)));

      Unused := GuiCheckBox ((25.0, 108.0, 15.0, 15.0), "FORCE CHECK!",
                             Force_Squared_Check'Access);

      GuiSetStyle (TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);
      if GuiSpinner ((25.0, 135.0, 125.0, 30.0),
                     "",
                     Spinner_001_Value'Access,
                     0,
                     100,
                     Spinner_Edit_Mode) /= 0
      then
         Spinner_Edit_Mode := not Spinner_Edit_Mode;
      end if;

      if GuiValueBox ((25.0, 175.0, 125.0, 30.0),
                      "",
                      Valuebox_002_Value'Access,
                      0,
                      100,
                      Valuebox_Edit_Mode) /= 0
      then
         Valuebox_Edit_Mode := not Valuebox_Edit_Mode;
      end if;

      GuiSetStyle (TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
      if GuiTextBox ((25.0, 215.0, 125.0, 30.0),
                     To_Chars_Ptr (Textbox_Text'Unchecked_Access),
                     Textbox_Text'Length,
                     Textbox_Edit_Mode) /= 0
      then
         Textbox_Edit_Mode := not Textbox_Edit_Mode;
      end if;

      GuiSetStyle (BUTTON, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
      if GuiButton ((25.0, 255.0, 125.0, 30.0),
                    GuiIconText (ICON_FILE_SAVE'Enum_Rep, "Save File")) /= 0
      then
         Show_Textinput_Box := True;
      end if;

      declare
         Y : C_float := 320.0;
      begin
         Unused := GuiGroupBox ((25.0, 310.0, 125.0, 150.0), "STATES");
         for State in GuiState loop
            GuiSetState (State'Enum_Rep);
            if GuiButton ((30.0, Y, 115.0, 30.0), State'Img) /= 0 then
               null;
            end if;
            Y := Y + 35.0;
         end loop;
         GuiSetState (STATE_NORMAL'Enum_Rep);
      end;

      Unused := GuiComboBox
        ((25.0, 480.0, 125.0, 30.0),
         "default;Jungle;Lavanda;Dark;Bluish;Cyber;Terminal",
         Visual_Style_Active'Access);

      --  NOTE: GuiDropdownBox must draw after any other control that can be
      --  covered on unfolding.
      GuiUnlock;
      GuiSetStyle (DROPDOWNBOX, TEXT_PADDING, 4);
      GuiSetStyle (DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
      if GuiDropdownBox ((25.0, 65.0, 125.0, 30.0),
                         "#01#ONE;#02#TWO;#03#THREE;#04#FOUR",
                         Dropdown_001_Active'Access,
                         Dropdown_001_Edit_Mode) /= 0
      then
         Dropdown_001_Edit_Mode := not Dropdown_001_Edit_Mode;
      end if;
      if GuiDropdownBox ((25.0, 25.0, 125.0, 30.0),
                         "ONE;TWO;THREE",
                         Dropdown_000_Active'Access,
                         Dropdown_000_Edit_Mode) /= 0
      then
         Dropdown_000_Edit_Mode := not Dropdown_000_Edit_Mode;
      end if;
      GuiSetStyle (DROPDOWNBOX, TEXT_PADDING, 0);
      GuiSetStyle (DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);

      --  Second GUI Column
      Unused := GuiListView
        ((165.0, 25.0, 140.0, 124.0),
         "Charmander;Bulbasaur;#18#Squirtel;Pikachu;Eevee;Pidgey",
         ListViewScrollIndex'Access,
         ListViewActive'Access);

      Unused := GuiListViewEx ((165.0, 162.0, 140.0, 184.0),
                               To_C_String_Array_Access (listViewExList),
                               listViewExList'Length,
                               ListViewExScrollIndex'Access,
                               ListViewExActive'Access,
                               ListViewExFocus'Access);

      Unused := GuiToggleGroup ((165.0, 360.0, 140.0, 24.0),
                                "#1#ONE" & ASCII.LF &
                                "#3#TWO" & ASCII.LF &
                                "#8#THREE" & ASCII.LF &
                                "#23#",
                                ToggleGroupActive'Access);

      GuiSetStyle (SLIDER, SLIDER_PADDING, 2);
      Unused := GuiToggleSlider ((165.0, 480.0, 140.0, 30.0),
                                 "ON;OFF",
                                 ToggleSliderActive'Access);
      GuiSetStyle (SLIDER, SLIDER_PADDING'Enum_Rep, 0);

      --  Third GUI column
      Unused := GuiPanel ((320.0, 25.0, 225.0, 140.0), "Panel Info");

      Unused := GuiColorPicker ((320.0, 185.0, 196.0, 192.0),
                                "",
                                ColorPickerValue'Access);
      if ToggleSliderActive = 0 then
         GuiSetStyle (DEFAULT,
                      BACKGROUND_COLOR,
                      ColorToInt (ColorPickerValue));
      end if;

      Unused := GuiSlider ((355.0, 400.0, 165.0, 20.0),
                           "TEST",
                           SliderValue'Img,
                           SliderValue'Access,
                           -50.0, 100.0);

      Unused := GuiSliderBar ((320.0, 430.0, 200.0, 20.0),
                              "",
                              SliderBarValue'Img,
                              SliderBarValue'Access,
                              0.0, 100.0);

      Unused := GuiProgressBar ((320.0, 460.0, 200.0, 20.0),
                                "",
                                ProgressValue'Img,
                                ProgressValue'Access,
                                0.0, 1.0);
      GuiEnable;

      Unused := GuiScrollPanel ((560.0, 25.0, 102.0, 354.0),
                                "",
                                (560.0, 25.0, 300.0, 1200.0),
                                ViewScroll'Access,
                                View'Access);

      Unused := GuiGrid ((560.0, 25.0 + 180.0 + 195.0, 100.0, 120.0),
                         "",
                         20.0,
                         3,
                         MouseCell'Access);

      Unused := GuiColorBarAlpha
        ((320.0, 490.0, 200.0, 30.0), "", AlphaValue'Access);

      --  WARNING: Word-wrap does not work as expected in case of no-top
      --  alignment.
      GuiSetStyle (DEFAULT,
                   TEXT_ALIGNMENT_VERTICAL,
                   TEXT_ALIGN_TOP'Enum_Rep);
      --  WARNING: If wrap mode enabled, text editing is not supported
      GuiSetStyle (DEFAULT,
                   TEXT_WRAP_MODE,
                   TEXT_WRAP_WORD'Enum_Rep);
      if GuiTextBox ((678.0, 25.0, 258.0, 492.0),
                     To_Chars_Ptr (TextBoxMultiText'Unchecked_Access),
                     1024,
                     TextBoxMultiEditMode) /= 0
      then
         TextBoxMultiEditMode := not TextBoxMultiEditMode;
      end if;
      GuiSetStyle (DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_NONE);
      GuiSetStyle (DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_MIDDLE);

      GuiSetStyle (DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_NONE);
      GuiSetStyle (DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_MIDDLE);

      GuiSetStyle (DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
      Unused := GuiStatusBar ((0.0,
                              C_float (GetScreenHeight - 20),
                              C_float (GetScreenWidth),
                              20.0), "This is a status bar");
      GuiSetStyle (DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

      if Show_Message_Box then
         DrawRectangle (0, 0, GetScreenWidth, GetScreenHeight,
                        Fade (RAYWHITE, 0.8));

         declare
            Result : constant int :=
              GuiMessageBox ((C_float (GetScreenWidth / 2 - 125),
                             C_float (GetScreenHeight / 2 - 50),
                             250.0, 100.0),
                             GuiIconText (ICON_EXIT'Enum_Rep, "Close Window"),
                             "Do you really want to exit?", "Yes;No");
         begin
            case Result is
               when 1 =>
                  Exit_Window := True;
               when 0 | 2 =>
                  Show_Message_Box := False;
               when others =>
                  null;
            end case;
         end;
      end if;

      if Show_Textinput_Box then
         DrawRectangle (0, 0, GetScreenWidth, GetScreenHeight,
                        Fade (RAYWHITE, 0.8));

         GuiSetStyle (TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
         declare
            Result : constant int :=
              GuiTextInputBox ((C_float (GetScreenWidth / 2 - 120),
                               C_float (GetScreenHeight / 2 - 60),
                               240.0,
                               140.0),
                               GuiIconText (ICON_FILE_SAVE'Enum_Rep,
                                            "Save file as..."),
                               "Introduce output file name:",
                               "Ok;Cancel",
                               To_Chars_Ptr (Text_Input'Unchecked_Access),
                               255,
                               null);
         begin
            if Result = 1 then
               Unused := TextCopy
                 (To_Chars_Ptr (Text_Input_Filename'Unchecked_Access),
                  To_Chars_Ptr (Text_Input'Unchecked_Access));
            end if;

            if Result in 0 | 1 | 2 then
               Show_Textinput_Box := False;
               Text_Input (Text_Input'First) := nul;
            end if;
         end;
      end if;

      Raylib.DrawFPS (0, 0);
      Raylib.EndDrawing;
   end loop;

   Raylib.CloseWindow;

end GUI_Example;
