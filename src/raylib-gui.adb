with Ada.Unchecked_Conversion;
package body Raylib.GUI is
   pragma Style_Checks ("M2000");
   function To_C_String_Array_Access (A : aliased Interfaces.C.Strings.chars_ptr_array) return C_String_Array_Access is
      function Convert
      is new Ada.Unchecked_Conversion (System.Address, C_String_Array_Access);
   begin
      return Convert (A'Address);
   end To_C_String_Array_Access;
   procedure GuiLoadStyle (fileName : String) is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
   begin
      GuiLoadStyle (C_fileName);
      Free (C_fileName);
   end GuiLoadStyle;

   procedure GuiSetTooltip (tooltip : String) is
      use Interfaces.C.Strings;
      C_tooltip : Interfaces.C.Strings.chars_ptr := New_String (tooltip);
   begin
      GuiSetTooltip (C_tooltip);
      Free (C_tooltip);
   end GuiSetTooltip;

   function GuiIconText (iconId : Interfaces.C.int; text : String) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant String := Value (GuiIconText (iconId, C_text));
   begin
      Free (C_text);
      return Result;
   end GuiIconText;

   function GuiLoadIcons (fileName : String; loadIconsName : Interfaces.C.C_bool) return access Interfaces.C.Strings.chars_ptr is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant access Interfaces.C.Strings.chars_ptr := GuiLoadIcons (C_fileName, loadIconsName);
   begin
      Free (C_fileName);
      return Result;
   end GuiLoadIcons;

   function GuiWindowBox (bounds : Rectangle; title : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_title : Interfaces.C.Strings.chars_ptr := New_String (title);
      Result : constant Interfaces.C.int := GuiWindowBox (bounds, C_title);
   begin
      Free (C_title);
      return Result;
   end GuiWindowBox;

   function GuiGroupBox (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiGroupBox (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiGroupBox;

   function GuiLine (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiLine (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiLine;

   function GuiPanel (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiPanel (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiPanel;

   function GuiScrollPanel (bounds : Rectangle; text : String; content : Rectangle; scroll : access Vector2; view : access Rectangle) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiScrollPanel (bounds, C_text, content, scroll, view);
   begin
      Free (C_text);
      return Result;
   end GuiScrollPanel;

   function GuiLabel (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiLabel (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiLabel;

   function GuiButton (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiButton (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiButton;

   function GuiLabelButton (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiLabelButton (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiLabelButton;

   function GuiToggle (bounds : Rectangle; text : String; active : access Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiToggle (bounds, C_text, active);
   begin
      Free (C_text);
      return Result;
   end GuiToggle;

   function GuiToggleGroup (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiToggleGroup (bounds, C_text, active);
   begin
      Free (C_text);
      return Result;
   end GuiToggleGroup;

   function GuiToggleSlider (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiToggleSlider (bounds, C_text, active);
   begin
      Free (C_text);
      return Result;
   end GuiToggleSlider;

   function GuiCheckBox (bounds : Rectangle; text : String; checked : access Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiCheckBox (bounds, C_text, checked);
   begin
      Free (C_text);
      return Result;
   end GuiCheckBox;

   function GuiComboBox (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiComboBox (bounds, C_text, active);
   begin
      Free (C_text);
      return Result;
   end GuiComboBox;

   function GuiDropdownBox (bounds : Rectangle; text : String; active : access Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiDropdownBox (bounds, C_text, active, editMode);
   begin
      Free (C_text);
      return Result;
   end GuiDropdownBox;

   function GuiSpinner (bounds : Rectangle; text : String; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiSpinner (bounds, C_text, value, minValue, maxValue, editMode);
   begin
      Free (C_text);
      return Result;
   end GuiSpinner;

   function GuiValueBox (bounds : Rectangle; text : String; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiValueBox (bounds, C_text, value, minValue, maxValue, editMode);
   begin
      Free (C_text);
      return Result;
   end GuiValueBox;

   function GuiTextBox (bounds : Rectangle; text : String; textSize : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiTextBox (bounds, C_text, textSize, editMode);
   begin
      Free (C_text);
      return Result;
   end GuiTextBox;

   function GuiSlider (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_textLeft : Interfaces.C.Strings.chars_ptr := New_String (textLeft);
      C_textRight : Interfaces.C.Strings.chars_ptr := New_String (textRight);
      Result : constant Interfaces.C.int := GuiSlider (bounds, C_textLeft, C_textRight, value, minValue, maxValue);
   begin
      Free (C_textLeft);
      Free (C_textRight);
      return Result;
   end GuiSlider;

   function GuiSliderBar (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_textLeft : Interfaces.C.Strings.chars_ptr := New_String (textLeft);
      C_textRight : Interfaces.C.Strings.chars_ptr := New_String (textRight);
      Result : constant Interfaces.C.int := GuiSliderBar (bounds, C_textLeft, C_textRight, value, minValue, maxValue);
   begin
      Free (C_textLeft);
      Free (C_textRight);
      return Result;
   end GuiSliderBar;

   function GuiProgressBar (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_textLeft : Interfaces.C.Strings.chars_ptr := New_String (textLeft);
      C_textRight : Interfaces.C.Strings.chars_ptr := New_String (textRight);
      Result : constant Interfaces.C.int := GuiProgressBar (bounds, C_textLeft, C_textRight, value, minValue, maxValue);
   begin
      Free (C_textLeft);
      Free (C_textRight);
      return Result;
   end GuiProgressBar;

   function GuiStatusBar (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiStatusBar (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiStatusBar;

   function GuiDummyRec (bounds : Rectangle; text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiDummyRec (bounds, C_text);
   begin
      Free (C_text);
      return Result;
   end GuiDummyRec;

   function GuiGrid (bounds : Rectangle; text : String; spacing : Interfaces.C.C_float; subdivs : Interfaces.C.int; mouseCell : access Vector2) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiGrid (bounds, C_text, spacing, subdivs, mouseCell);
   begin
      Free (C_text);
      return Result;
   end GuiGrid;

   function GuiListView (bounds : Rectangle; text : String; scrollIndex : access Interfaces.C.int; active : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiListView (bounds, C_text, scrollIndex, active);
   begin
      Free (C_text);
      return Result;
   end GuiListView;

   function GuiMessageBox (bounds : Rectangle; title : String; message : String; buttons : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_title : Interfaces.C.Strings.chars_ptr := New_String (title);
      C_message : Interfaces.C.Strings.chars_ptr := New_String (message);
      C_buttons : Interfaces.C.Strings.chars_ptr := New_String (buttons);
      Result : constant Interfaces.C.int := GuiMessageBox (bounds, C_title, C_message, C_buttons);
   begin
      Free (C_title);
      Free (C_message);
      Free (C_buttons);
      return Result;
   end GuiMessageBox;

   function GuiTextInputBox (bounds : Rectangle; title : String; message : String; buttons : String; text : Interfaces.C.Strings.chars_ptr; textMaxSize : Interfaces.C.int; secretViewActive : access Interfaces.C.C_bool) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_title : Interfaces.C.Strings.chars_ptr := New_String (title);
      C_message : Interfaces.C.Strings.chars_ptr := New_String (message);
      C_buttons : Interfaces.C.Strings.chars_ptr := New_String (buttons);
      Result : constant Interfaces.C.int := GuiTextInputBox (bounds, C_title, C_message, C_buttons, text, textMaxSize, secretViewActive);
   begin
      Free (C_title);
      Free (C_message);
      Free (C_buttons);
      return Result;
   end GuiTextInputBox;

   function GuiColorPicker (bounds : Rectangle; text : String; color_p : access Color) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorPicker (bounds, C_text, color_p);
   begin
      Free (C_text);
      return Result;
   end GuiColorPicker;

   function GuiColorPanel (bounds : Rectangle; text : String; color_p : access Color) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorPanel (bounds, C_text, color_p);
   begin
      Free (C_text);
      return Result;
   end GuiColorPanel;

   function GuiColorBarAlpha (bounds : Rectangle; text : String; alpha : access Interfaces.C.C_float) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorBarAlpha (bounds, C_text, alpha);
   begin
      Free (C_text);
      return Result;
   end GuiColorBarAlpha;

   function GuiColorBarHue (bounds : Rectangle; text : String; value : access Interfaces.C.C_float) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorBarHue (bounds, C_text, value);
   begin
      Free (C_text);
      return Result;
   end GuiColorBarHue;

   function GuiColorPickerHSV (bounds : Rectangle; text : String; colorHsv : access Vector3) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorPickerHSV (bounds, C_text, colorHsv);
   begin
      Free (C_text);
      return Result;
   end GuiColorPickerHSV;

   function GuiColorPanelHSV (bounds : Rectangle; text : String; colorHsv : access Vector3) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GuiColorPanelHSV (bounds, C_text, colorHsv);
   begin
      Free (C_text);
      return Result;
   end GuiColorPanelHSV;

end Raylib.GUI;
