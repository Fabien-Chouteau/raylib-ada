with Interfaces.C;
with Interfaces.C.Strings;
package Raylib.GUI
  with Preelaborate
is
   pragma Style_Checks ("M2000");
   function To_C_String_Array_Access (A : aliased Interfaces.C.Strings.chars_ptr_array) return C_String_Array_Access;
   type GuiState is
     (
         STATE_NORMAL
       , STATE_FOCUSED
       , STATE_PRESSED
       , STATE_DISABLED
     )
     with Convention => C;
   --  Gui control state

   for GuiState use
     (
         STATE_NORMAL => 0
       , STATE_FOCUSED => 1
       , STATE_PRESSED => 2
       , STATE_DISABLED => 3
     );

   subtype GuiTextAlignment is Interfaces.C.int;
   --  Gui control text alignment

   TEXT_ALIGN_LEFT : constant GuiTextAlignment := 0;
   TEXT_ALIGN_CENTER : constant GuiTextAlignment := 1;
   TEXT_ALIGN_RIGHT : constant GuiTextAlignment := 2;

   subtype GuiTextAlignmentVertical is Interfaces.C.int;
   --  Gui control text alignment vertical

   TEXT_ALIGN_TOP : constant GuiTextAlignmentVertical := 0;
   TEXT_ALIGN_MIDDLE : constant GuiTextAlignmentVertical := 1;
   TEXT_ALIGN_BOTTOM : constant GuiTextAlignmentVertical := 2;

   subtype GuiTextWrapMode is Interfaces.C.int;
   --  Gui control text wrap mode

   TEXT_WRAP_NONE : constant GuiTextWrapMode := 0;
   TEXT_WRAP_CHAR : constant GuiTextWrapMode := 1;
   TEXT_WRAP_WORD : constant GuiTextWrapMode := 2;

   type GuiControl is
     (
         DEFAULT
       , LABEL -- Used also for: LABELBUTTON
       , BUTTON
       , TOGGLE -- Used also for: TOGGLEGROUP
       , SLIDER -- Used also for: SLIDERBAR, TOGGLESLIDER
       , PROGRESSBAR
       , CHECKBOX
       , COMBOBOX
       , DROPDOWNBOX
       , TEXTBOX -- Used also for: TEXTBOXMULTI
       , VALUEBOX
       , SPINNER -- Uses: BUTTON, VALUEBOX
       , LISTVIEW
       , COLORPICKER
       , SCROLLBAR
       , STATUSBAR
     )
     with Convention => C;
   --  Gui controls

   for GuiControl use
     (
         DEFAULT => 0
       , LABEL => 1
       , BUTTON => 2
       , TOGGLE => 3
       , SLIDER => 4
       , PROGRESSBAR => 5
       , CHECKBOX => 6
       , COMBOBOX => 7
       , DROPDOWNBOX => 8
       , TEXTBOX => 9
       , VALUEBOX => 10
       , SPINNER => 11
       , LISTVIEW => 12
       , COLORPICKER => 13
       , SCROLLBAR => 14
       , STATUSBAR => 15
     );

   subtype GuiControlProperty is Interfaces.C.int;
   --  Gui base properties for every control

   BORDER_COLOR_NORMAL : constant GuiControlProperty := 0; -- Control border color in STATE_NORMAL
   BASE_COLOR_NORMAL : constant GuiControlProperty := 1; -- Control base color in STATE_NORMAL
   TEXT_COLOR_NORMAL : constant GuiControlProperty := 2; -- Control text color in STATE_NORMAL
   BORDER_COLOR_FOCUSED : constant GuiControlProperty := 3; -- Control border color in STATE_FOCUSED
   BASE_COLOR_FOCUSED : constant GuiControlProperty := 4; -- Control base color in STATE_FOCUSED
   TEXT_COLOR_FOCUSED : constant GuiControlProperty := 5; -- Control text color in STATE_FOCUSED
   BORDER_COLOR_PRESSED : constant GuiControlProperty := 6; -- Control border color in STATE_PRESSED
   BASE_COLOR_PRESSED : constant GuiControlProperty := 7; -- Control base color in STATE_PRESSED
   TEXT_COLOR_PRESSED : constant GuiControlProperty := 8; -- Control text color in STATE_PRESSED
   BORDER_COLOR_DISABLED : constant GuiControlProperty := 9; -- Control border color in STATE_DISABLED
   BASE_COLOR_DISABLED : constant GuiControlProperty := 10; -- Control base color in STATE_DISABLED
   TEXT_COLOR_DISABLED : constant GuiControlProperty := 11; -- Control text color in STATE_DISABLED
   BORDER_WIDTH : constant GuiControlProperty := 12; -- Control border size, 0 for no border
   TEXT_PADDING : constant GuiControlProperty := 13; -- Control text padding, not considering border
   TEXT_ALIGNMENT : constant GuiControlProperty := 14; -- Control text horizontal alignment inside control text bound (after border and padding)

   subtype GuiDefaultProperty is Interfaces.C.int;
   --  DEFAULT extended properties

   TEXT_SIZE : constant GuiDefaultProperty := 16; -- Text size (glyphs max height)
   TEXT_SPACING : constant GuiDefaultProperty := 17; -- Text spacing between glyphs
   LINE_COLOR : constant GuiDefaultProperty := 18; -- Line control color
   BACKGROUND_COLOR : constant GuiDefaultProperty := 19; -- Background color
   TEXT_LINE_SPACING : constant GuiDefaultProperty := 20; -- Text spacing between lines
   TEXT_ALIGNMENT_VERTICAL : constant GuiDefaultProperty := 21; -- Text vertical alignment inside text bounds (after border and padding)
   TEXT_WRAP_MODE : constant GuiDefaultProperty := 22; -- Text wrap-mode inside text bounds

   subtype GuiToggleProperty is Interfaces.C.int;
   --  Toggle/ToggleGroup

   GROUP_PADDING : constant GuiToggleProperty := 16; -- ToggleGroup separation between toggles

   subtype GuiSliderProperty is Interfaces.C.int;
   --  Slider/SliderBar

   SLIDER_WIDTH : constant GuiSliderProperty := 16; -- Slider size of internal bar
   SLIDER_PADDING : constant GuiSliderProperty := 17; -- Slider/SliderBar internal bar padding

   subtype GuiProgressBarProperty is Interfaces.C.int;
   --  ProgressBar

   PROGRESS_PADDING : constant GuiProgressBarProperty := 16; -- ProgressBar internal padding

   subtype GuiScrollBarProperty is Interfaces.C.int;
   --  ScrollBar

   ARROWS_SIZE : constant GuiScrollBarProperty := 16; -- ScrollBar arrows size
   ARROWS_VISIBLE : constant GuiScrollBarProperty := 17; -- ScrollBar arrows visible
   SCROLL_SLIDER_PADDING : constant GuiScrollBarProperty := 18; -- ScrollBar slider internal padding
   SCROLL_SLIDER_SIZE : constant GuiScrollBarProperty := 19; -- ScrollBar slider size
   SCROLL_PADDING : constant GuiScrollBarProperty := 20; -- ScrollBar scroll padding from arrows
   SCROLL_SPEED : constant GuiScrollBarProperty := 21; -- ScrollBar scrolling speed

   subtype GuiCheckBoxProperty is Interfaces.C.int;
   --  CheckBox

   CHECK_PADDING : constant GuiCheckBoxProperty := 16; -- CheckBox internal check padding

   subtype GuiComboBoxProperty is Interfaces.C.int;
   --  ComboBox

   COMBO_BUTTON_WIDTH : constant GuiComboBoxProperty := 16; -- ComboBox right button width
   COMBO_BUTTON_SPACING : constant GuiComboBoxProperty := 17; -- ComboBox button separation

   subtype GuiDropdownBoxProperty is Interfaces.C.int;
   --  DropdownBox

   ARROW_PADDING : constant GuiDropdownBoxProperty := 16; -- DropdownBox arrow separation from border and items
   DROPDOWN_ITEMS_SPACING : constant GuiDropdownBoxProperty := 17; -- DropdownBox items separation

   subtype GuiTextBoxProperty is Interfaces.C.int;
   --  TextBox/TextBoxMulti/ValueBox/Spinner

   TEXT_READONLY : constant GuiTextBoxProperty := 16; -- TextBox in read-only mode: 0-text editable, 1-text no-editable

   subtype GuiSpinnerProperty is Interfaces.C.int;
   --  Spinner

   SPIN_BUTTON_WIDTH : constant GuiSpinnerProperty := 16; -- Spinner left/right buttons width
   SPIN_BUTTON_SPACING : constant GuiSpinnerProperty := 17; -- Spinner buttons separation

   subtype GuiListViewProperty is Interfaces.C.int;
   --  ListView

   LIST_ITEMS_HEIGHT : constant GuiListViewProperty := 16; -- ListView items height
   LIST_ITEMS_SPACING : constant GuiListViewProperty := 17; -- ListView items separation
   SCROLLBAR_WIDTH : constant GuiListViewProperty := 18; -- ListView scrollbar size (usually width)
   SCROLLBAR_SIDE : constant GuiListViewProperty := 19; -- ListView scrollbar side (0-SCROLLBAR_LEFT_SIDE, 1-SCROLLBAR_RIGHT_SIDE)

   subtype GuiColorPickerProperty is Interfaces.C.int;
   --  ColorPicker

   COLOR_SELECTOR_SIZE : constant GuiColorPickerProperty := 16;
   HUEBAR_WIDTH : constant GuiColorPickerProperty := 17; -- ColorPicker right hue bar width
   HUEBAR_PADDING : constant GuiColorPickerProperty := 18; -- ColorPicker right hue bar separation from panel
   HUEBAR_SELECTOR_HEIGHT : constant GuiColorPickerProperty := 19; -- ColorPicker right hue bar selector height
   HUEBAR_SELECTOR_OVERFLOW : constant GuiColorPickerProperty := 20; -- ColorPicker right hue bar selector overflow

   type GuiIconName is
     (
         ICON_NONE
       , ICON_FOLDER_FILE_OPEN
       , ICON_FILE_SAVE_CLASSIC
       , ICON_FOLDER_OPEN
       , ICON_FOLDER_SAVE
       , ICON_FILE_OPEN
       , ICON_FILE_SAVE
       , ICON_FILE_EXPORT
       , ICON_FILE_ADD
       , ICON_FILE_DELETE
       , ICON_FILETYPE_TEXT
       , ICON_FILETYPE_AUDIO
       , ICON_FILETYPE_IMAGE
       , ICON_FILETYPE_PLAY
       , ICON_FILETYPE_VIDEO
       , ICON_FILETYPE_INFO
       , ICON_FILE_COPY
       , ICON_FILE_CUT
       , ICON_FILE_PASTE
       , ICON_CURSOR_HAND
       , ICON_CURSOR_POINTER
       , ICON_CURSOR_CLASSIC
       , ICON_PENCIL
       , ICON_PENCIL_BIG
       , ICON_BRUSH_CLASSIC
       , ICON_BRUSH_PAINTER
       , ICON_WATER_DROP
       , ICON_COLOR_PICKER
       , ICON_RUBBER
       , ICON_COLOR_BUCKET
       , ICON_TEXT_T
       , ICON_TEXT_A
       , ICON_SCALE
       , ICON_RESIZE
       , ICON_FILTER_POINT
       , ICON_FILTER_BILINEAR
       , ICON_CROP
       , ICON_CROP_ALPHA
       , ICON_SQUARE_TOGGLE
       , ICON_SYMMETRY
       , ICON_SYMMETRY_HORIZONTAL
       , ICON_SYMMETRY_VERTICAL
       , ICON_LENS
       , ICON_LENS_BIG
       , ICON_EYE_ON
       , ICON_EYE_OFF
       , ICON_FILTER_TOP
       , ICON_FILTER
       , ICON_TARGET_POINT
       , ICON_TARGET_SMALL
       , ICON_TARGET_BIG
       , ICON_TARGET_MOVE
       , ICON_CURSOR_MOVE
       , ICON_CURSOR_SCALE
       , ICON_CURSOR_SCALE_RIGHT
       , ICON_CURSOR_SCALE_LEFT
       , ICON_UNDO
       , ICON_REDO
       , ICON_REREDO
       , ICON_MUTATE
       , ICON_ROTATE
       , ICON_REPEAT
       , ICON_SHUFFLE
       , ICON_EMPTYBOX
       , ICON_TARGET
       , ICON_TARGET_SMALL_FILL
       , ICON_TARGET_BIG_FILL
       , ICON_TARGET_MOVE_FILL
       , ICON_CURSOR_MOVE_FILL
       , ICON_CURSOR_SCALE_FILL
       , ICON_CURSOR_SCALE_RIGHT_FILL
       , ICON_CURSOR_SCALE_LEFT_FILL
       , ICON_UNDO_FILL
       , ICON_REDO_FILL
       , ICON_REREDO_FILL
       , ICON_MUTATE_FILL
       , ICON_ROTATE_FILL
       , ICON_REPEAT_FILL
       , ICON_SHUFFLE_FILL
       , ICON_EMPTYBOX_SMALL
       , ICON_BOX
       , ICON_BOX_TOP
       , ICON_BOX_TOP_RIGHT
       , ICON_BOX_RIGHT
       , ICON_BOX_BOTTOM_RIGHT
       , ICON_BOX_BOTTOM
       , ICON_BOX_BOTTOM_LEFT
       , ICON_BOX_LEFT
       , ICON_BOX_TOP_LEFT
       , ICON_BOX_CENTER
       , ICON_BOX_CIRCLE_MASK
       , ICON_POT
       , ICON_ALPHA_MULTIPLY
       , ICON_ALPHA_CLEAR
       , ICON_DITHERING
       , ICON_MIPMAPS
       , ICON_BOX_GRID
       , ICON_GRID
       , ICON_BOX_CORNERS_SMALL
       , ICON_BOX_CORNERS_BIG
       , ICON_FOUR_BOXES
       , ICON_GRID_FILL
       , ICON_BOX_MULTISIZE
       , ICON_ZOOM_SMALL
       , ICON_ZOOM_MEDIUM
       , ICON_ZOOM_BIG
       , ICON_ZOOM_ALL
       , ICON_ZOOM_CENTER
       , ICON_BOX_DOTS_SMALL
       , ICON_BOX_DOTS_BIG
       , ICON_BOX_CONCENTRIC
       , ICON_BOX_GRID_BIG
       , ICON_OK_TICK
       , ICON_CROSS
       , ICON_ARROW_LEFT
       , ICON_ARROW_RIGHT
       , ICON_ARROW_DOWN
       , ICON_ARROW_UP
       , ICON_ARROW_LEFT_FILL
       , ICON_ARROW_RIGHT_FILL
       , ICON_ARROW_DOWN_FILL
       , ICON_ARROW_UP_FILL
       , ICON_AUDIO
       , ICON_FX
       , ICON_WAVE
       , ICON_WAVE_SINUS
       , ICON_WAVE_SQUARE
       , ICON_WAVE_TRIANGULAR
       , ICON_CROSS_SMALL
       , ICON_PLAYER_PREVIOUS
       , ICON_PLAYER_PLAY_BACK
       , ICON_PLAYER_PLAY
       , ICON_PLAYER_PAUSE
       , ICON_PLAYER_STOP
       , ICON_PLAYER_NEXT
       , ICON_PLAYER_RECORD
       , ICON_MAGNET
       , ICON_LOCK_CLOSE
       , ICON_LOCK_OPEN
       , ICON_CLOCK
       , ICON_TOOLS
       , ICON_GEAR
       , ICON_GEAR_BIG
       , ICON_BIN
       , ICON_HAND_POINTER
       , ICON_LASER
       , ICON_COIN
       , ICON_EXPLOSION
       , ICON_1UP
       , ICON_PLAYER
       , ICON_PLAYER_JUMP
       , ICON_KEY
       , ICON_DEMON
       , ICON_TEXT_POPUP
       , ICON_GEAR_EX
       , ICON_CRACK
       , ICON_CRACK_POINTS
       , ICON_STAR
       , ICON_DOOR
       , ICON_EXIT
       , ICON_MODE_2D
       , ICON_MODE_3D
       , ICON_CUBE
       , ICON_CUBE_FACE_TOP
       , ICON_CUBE_FACE_LEFT
       , ICON_CUBE_FACE_FRONT
       , ICON_CUBE_FACE_BOTTOM
       , ICON_CUBE_FACE_RIGHT
       , ICON_CUBE_FACE_BACK
       , ICON_CAMERA
       , ICON_SPECIAL
       , ICON_LINK_NET
       , ICON_LINK_BOXES
       , ICON_LINK_MULTI
       , ICON_LINK
       , ICON_LINK_BROKE
       , ICON_TEXT_NOTES
       , ICON_NOTEBOOK
       , ICON_SUITCASE
       , ICON_SUITCASE_ZIP
       , ICON_MAILBOX
       , ICON_MONITOR
       , ICON_PRINTER
       , ICON_PHOTO_CAMERA
       , ICON_PHOTO_CAMERA_FLASH
       , ICON_HOUSE
       , ICON_HEART
       , ICON_CORNER
       , ICON_VERTICAL_BARS
       , ICON_VERTICAL_BARS_FILL
       , ICON_LIFE_BARS
       , ICON_INFO
       , ICON_CROSSLINE
       , ICON_HELP
       , ICON_FILETYPE_ALPHA
       , ICON_FILETYPE_HOME
       , ICON_LAYERS_VISIBLE
       , ICON_LAYERS
       , ICON_WINDOW
       , ICON_HIDPI
       , ICON_FILETYPE_BINARY
       , ICON_HEX
       , ICON_SHIELD
       , ICON_FILE_NEW
       , ICON_FOLDER_ADD
       , ICON_ALARM
       , ICON_CPU
       , ICON_ROM
       , ICON_STEP_OVER
       , ICON_STEP_INTO
       , ICON_STEP_OUT
       , ICON_RESTART
       , ICON_BREAKPOINT_ON
       , ICON_BREAKPOINT_OFF
       , ICON_BURGER_MENU
       , ICON_CASE_SENSITIVE
       , ICON_REG_EXP
       , ICON_FOLDER
       , ICON_FILE
       , ICON_SAND_TIMER
       , ICON_220
       , ICON_221
       , ICON_222
       , ICON_223
       , ICON_224
       , ICON_225
       , ICON_226
       , ICON_227
       , ICON_228
       , ICON_229
       , ICON_230
       , ICON_231
       , ICON_232
       , ICON_233
       , ICON_234
       , ICON_235
       , ICON_236
       , ICON_237
       , ICON_238
       , ICON_239
       , ICON_240
       , ICON_241
       , ICON_242
       , ICON_243
       , ICON_244
       , ICON_245
       , ICON_246
       , ICON_247
       , ICON_248
       , ICON_249
       , ICON_250
       , ICON_251
       , ICON_252
       , ICON_253
       , ICON_254
       , ICON_255
     )
     with Convention => C;

   for GuiIconName use
     (
         ICON_NONE => 0
       , ICON_FOLDER_FILE_OPEN => 1
       , ICON_FILE_SAVE_CLASSIC => 2
       , ICON_FOLDER_OPEN => 3
       , ICON_FOLDER_SAVE => 4
       , ICON_FILE_OPEN => 5
       , ICON_FILE_SAVE => 6
       , ICON_FILE_EXPORT => 7
       , ICON_FILE_ADD => 8
       , ICON_FILE_DELETE => 9
       , ICON_FILETYPE_TEXT => 10
       , ICON_FILETYPE_AUDIO => 11
       , ICON_FILETYPE_IMAGE => 12
       , ICON_FILETYPE_PLAY => 13
       , ICON_FILETYPE_VIDEO => 14
       , ICON_FILETYPE_INFO => 15
       , ICON_FILE_COPY => 16
       , ICON_FILE_CUT => 17
       , ICON_FILE_PASTE => 18
       , ICON_CURSOR_HAND => 19
       , ICON_CURSOR_POINTER => 20
       , ICON_CURSOR_CLASSIC => 21
       , ICON_PENCIL => 22
       , ICON_PENCIL_BIG => 23
       , ICON_BRUSH_CLASSIC => 24
       , ICON_BRUSH_PAINTER => 25
       , ICON_WATER_DROP => 26
       , ICON_COLOR_PICKER => 27
       , ICON_RUBBER => 28
       , ICON_COLOR_BUCKET => 29
       , ICON_TEXT_T => 30
       , ICON_TEXT_A => 31
       , ICON_SCALE => 32
       , ICON_RESIZE => 33
       , ICON_FILTER_POINT => 34
       , ICON_FILTER_BILINEAR => 35
       , ICON_CROP => 36
       , ICON_CROP_ALPHA => 37
       , ICON_SQUARE_TOGGLE => 38
       , ICON_SYMMETRY => 39
       , ICON_SYMMETRY_HORIZONTAL => 40
       , ICON_SYMMETRY_VERTICAL => 41
       , ICON_LENS => 42
       , ICON_LENS_BIG => 43
       , ICON_EYE_ON => 44
       , ICON_EYE_OFF => 45
       , ICON_FILTER_TOP => 46
       , ICON_FILTER => 47
       , ICON_TARGET_POINT => 48
       , ICON_TARGET_SMALL => 49
       , ICON_TARGET_BIG => 50
       , ICON_TARGET_MOVE => 51
       , ICON_CURSOR_MOVE => 52
       , ICON_CURSOR_SCALE => 53
       , ICON_CURSOR_SCALE_RIGHT => 54
       , ICON_CURSOR_SCALE_LEFT => 55
       , ICON_UNDO => 56
       , ICON_REDO => 57
       , ICON_REREDO => 58
       , ICON_MUTATE => 59
       , ICON_ROTATE => 60
       , ICON_REPEAT => 61
       , ICON_SHUFFLE => 62
       , ICON_EMPTYBOX => 63
       , ICON_TARGET => 64
       , ICON_TARGET_SMALL_FILL => 65
       , ICON_TARGET_BIG_FILL => 66
       , ICON_TARGET_MOVE_FILL => 67
       , ICON_CURSOR_MOVE_FILL => 68
       , ICON_CURSOR_SCALE_FILL => 69
       , ICON_CURSOR_SCALE_RIGHT_FILL => 70
       , ICON_CURSOR_SCALE_LEFT_FILL => 71
       , ICON_UNDO_FILL => 72
       , ICON_REDO_FILL => 73
       , ICON_REREDO_FILL => 74
       , ICON_MUTATE_FILL => 75
       , ICON_ROTATE_FILL => 76
       , ICON_REPEAT_FILL => 77
       , ICON_SHUFFLE_FILL => 78
       , ICON_EMPTYBOX_SMALL => 79
       , ICON_BOX => 80
       , ICON_BOX_TOP => 81
       , ICON_BOX_TOP_RIGHT => 82
       , ICON_BOX_RIGHT => 83
       , ICON_BOX_BOTTOM_RIGHT => 84
       , ICON_BOX_BOTTOM => 85
       , ICON_BOX_BOTTOM_LEFT => 86
       , ICON_BOX_LEFT => 87
       , ICON_BOX_TOP_LEFT => 88
       , ICON_BOX_CENTER => 89
       , ICON_BOX_CIRCLE_MASK => 90
       , ICON_POT => 91
       , ICON_ALPHA_MULTIPLY => 92
       , ICON_ALPHA_CLEAR => 93
       , ICON_DITHERING => 94
       , ICON_MIPMAPS => 95
       , ICON_BOX_GRID => 96
       , ICON_GRID => 97
       , ICON_BOX_CORNERS_SMALL => 98
       , ICON_BOX_CORNERS_BIG => 99
       , ICON_FOUR_BOXES => 100
       , ICON_GRID_FILL => 101
       , ICON_BOX_MULTISIZE => 102
       , ICON_ZOOM_SMALL => 103
       , ICON_ZOOM_MEDIUM => 104
       , ICON_ZOOM_BIG => 105
       , ICON_ZOOM_ALL => 106
       , ICON_ZOOM_CENTER => 107
       , ICON_BOX_DOTS_SMALL => 108
       , ICON_BOX_DOTS_BIG => 109
       , ICON_BOX_CONCENTRIC => 110
       , ICON_BOX_GRID_BIG => 111
       , ICON_OK_TICK => 112
       , ICON_CROSS => 113
       , ICON_ARROW_LEFT => 114
       , ICON_ARROW_RIGHT => 115
       , ICON_ARROW_DOWN => 116
       , ICON_ARROW_UP => 117
       , ICON_ARROW_LEFT_FILL => 118
       , ICON_ARROW_RIGHT_FILL => 119
       , ICON_ARROW_DOWN_FILL => 120
       , ICON_ARROW_UP_FILL => 121
       , ICON_AUDIO => 122
       , ICON_FX => 123
       , ICON_WAVE => 124
       , ICON_WAVE_SINUS => 125
       , ICON_WAVE_SQUARE => 126
       , ICON_WAVE_TRIANGULAR => 127
       , ICON_CROSS_SMALL => 128
       , ICON_PLAYER_PREVIOUS => 129
       , ICON_PLAYER_PLAY_BACK => 130
       , ICON_PLAYER_PLAY => 131
       , ICON_PLAYER_PAUSE => 132
       , ICON_PLAYER_STOP => 133
       , ICON_PLAYER_NEXT => 134
       , ICON_PLAYER_RECORD => 135
       , ICON_MAGNET => 136
       , ICON_LOCK_CLOSE => 137
       , ICON_LOCK_OPEN => 138
       , ICON_CLOCK => 139
       , ICON_TOOLS => 140
       , ICON_GEAR => 141
       , ICON_GEAR_BIG => 142
       , ICON_BIN => 143
       , ICON_HAND_POINTER => 144
       , ICON_LASER => 145
       , ICON_COIN => 146
       , ICON_EXPLOSION => 147
       , ICON_1UP => 148
       , ICON_PLAYER => 149
       , ICON_PLAYER_JUMP => 150
       , ICON_KEY => 151
       , ICON_DEMON => 152
       , ICON_TEXT_POPUP => 153
       , ICON_GEAR_EX => 154
       , ICON_CRACK => 155
       , ICON_CRACK_POINTS => 156
       , ICON_STAR => 157
       , ICON_DOOR => 158
       , ICON_EXIT => 159
       , ICON_MODE_2D => 160
       , ICON_MODE_3D => 161
       , ICON_CUBE => 162
       , ICON_CUBE_FACE_TOP => 163
       , ICON_CUBE_FACE_LEFT => 164
       , ICON_CUBE_FACE_FRONT => 165
       , ICON_CUBE_FACE_BOTTOM => 166
       , ICON_CUBE_FACE_RIGHT => 167
       , ICON_CUBE_FACE_BACK => 168
       , ICON_CAMERA => 169
       , ICON_SPECIAL => 170
       , ICON_LINK_NET => 171
       , ICON_LINK_BOXES => 172
       , ICON_LINK_MULTI => 173
       , ICON_LINK => 174
       , ICON_LINK_BROKE => 175
       , ICON_TEXT_NOTES => 176
       , ICON_NOTEBOOK => 177
       , ICON_SUITCASE => 178
       , ICON_SUITCASE_ZIP => 179
       , ICON_MAILBOX => 180
       , ICON_MONITOR => 181
       , ICON_PRINTER => 182
       , ICON_PHOTO_CAMERA => 183
       , ICON_PHOTO_CAMERA_FLASH => 184
       , ICON_HOUSE => 185
       , ICON_HEART => 186
       , ICON_CORNER => 187
       , ICON_VERTICAL_BARS => 188
       , ICON_VERTICAL_BARS_FILL => 189
       , ICON_LIFE_BARS => 190
       , ICON_INFO => 191
       , ICON_CROSSLINE => 192
       , ICON_HELP => 193
       , ICON_FILETYPE_ALPHA => 194
       , ICON_FILETYPE_HOME => 195
       , ICON_LAYERS_VISIBLE => 196
       , ICON_LAYERS => 197
       , ICON_WINDOW => 198
       , ICON_HIDPI => 199
       , ICON_FILETYPE_BINARY => 200
       , ICON_HEX => 201
       , ICON_SHIELD => 202
       , ICON_FILE_NEW => 203
       , ICON_FOLDER_ADD => 204
       , ICON_ALARM => 205
       , ICON_CPU => 206
       , ICON_ROM => 207
       , ICON_STEP_OVER => 208
       , ICON_STEP_INTO => 209
       , ICON_STEP_OUT => 210
       , ICON_RESTART => 211
       , ICON_BREAKPOINT_ON => 212
       , ICON_BREAKPOINT_OFF => 213
       , ICON_BURGER_MENU => 214
       , ICON_CASE_SENSITIVE => 215
       , ICON_REG_EXP => 216
       , ICON_FOLDER => 217
       , ICON_FILE => 218
       , ICON_SAND_TIMER => 219
       , ICON_220 => 220
       , ICON_221 => 221
       , ICON_222 => 222
       , ICON_223 => 223
       , ICON_224 => 224
       , ICON_225 => 225
       , ICON_226 => 226
       , ICON_227 => 227
       , ICON_228 => 228
       , ICON_229 => 229
       , ICON_230 => 230
       , ICON_231 => 231
       , ICON_232 => 232
       , ICON_233 => 233
       , ICON_234 => 234
       , ICON_235 => 235
       , ICON_236 => 236
       , ICON_237 => 237
       , ICON_238 => 238
       , ICON_239 => 239
       , ICON_240 => 240
       , ICON_241 => 241
       , ICON_242 => 242
       , ICON_243 => 243
       , ICON_244 => 244
       , ICON_245 => 245
       , ICON_246 => 246
       , ICON_247 => 247
       , ICON_248 => 248
       , ICON_249 => 249
       , ICON_250 => 250
       , ICON_251 => 251
       , ICON_252 => 252
       , ICON_253 => 253
       , ICON_254 => 254
       , ICON_255 => 255
     );

   type GuiStyleProp is record
      controlId : Interfaces.C.short; -- Control identifier
      propertyId : Interfaces.C.short; -- Property identifier
      propertyValue : Interfaces.C.int; -- Property value
   end record
      with Convention => C_Pass_By_Copy;

   type GuiTextStyle is record
      size : Interfaces.C.unsigned;
      charSpacing : Interfaces.C.int;
      lineSpacing : Interfaces.C.int;
      alignmentH : Interfaces.C.int;
      alignmentV : Interfaces.C.int;
      padding : Interfaces.C.int;
   end record
      with Convention => C_Pass_By_Copy;

   procedure GuiEnable;
   --  Enable gui controls (global state)
   pragma Import (C, GuiEnable, "GuiEnable");

   procedure GuiDisable;
   --  Disable gui controls (global state)
   pragma Import (C, GuiDisable, "GuiDisable");

   procedure GuiLock;
   --  Lock gui controls (global state)
   pragma Import (C, GuiLock, "GuiLock");

   procedure GuiUnlock;
   --  Unlock gui controls (global state)
   pragma Import (C, GuiUnlock, "GuiUnlock");

   function GuiIsLocked return Interfaces.C.C_bool;
   --  Check if gui is locked (global state)
   pragma Import (C, GuiIsLocked, "GuiIsLocked");

   procedure GuiSetAlpha (alpha : Interfaces.C.C_float);
   --  Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f
   pragma Import (C, GuiSetAlpha, "GuiSetAlpha");

   procedure GuiSetState (state : Interfaces.C.int);
   --  Set gui state (global state)
   pragma Import (C, GuiSetState, "GuiSetState");

   function GuiGetState return Interfaces.C.int;
   --  Get gui state (global state)
   pragma Import (C, GuiGetState, "GuiGetState");

   procedure GuiSetFont (font_p : Font);
   --  Set gui custom font (global state)
   pragma Import (C, GuiSetFont, "GuiSetFont");

   function GuiGetFont return Font;
   --  Get gui custom font (global state)
   pragma Import (C, GuiGetFont, "GuiGetFont");

   procedure GuiSetStyle (control : GuiControl; property : Interfaces.C.int; value : Interfaces.C.int);
   --  Set one style property
   pragma Import (C, GuiSetStyle, "GuiSetStyle");

   function GuiGetStyle (control : GuiControl; property : Interfaces.C.int) return Interfaces.C.int;
   --  Get one style property
   pragma Import (C, GuiGetStyle, "GuiGetStyle");

   procedure GuiLoadStyle (fileName : Interfaces.C.Strings.chars_ptr);
   --  Load style file over global style variable (.rgs)
   pragma Import (C, GuiLoadStyle, "GuiLoadStyle");

   procedure GuiLoadStyle (fileName : String);
   --  Load style file over global style variable (.rgs)

   procedure GuiLoadStyleDefault;
   --  Load style default over global style
   pragma Import (C, GuiLoadStyleDefault, "GuiLoadStyleDefault");

   procedure GuiEnableTooltip;
   --  Enable gui tooltips (global state)
   pragma Import (C, GuiEnableTooltip, "GuiEnableTooltip");

   procedure GuiDisableTooltip;
   --  Disable gui tooltips (global state)
   pragma Import (C, GuiDisableTooltip, "GuiDisableTooltip");

   procedure GuiSetTooltip (tooltip : Interfaces.C.Strings.chars_ptr);
   --  Set tooltip string
   pragma Import (C, GuiSetTooltip, "GuiSetTooltip");

   procedure GuiSetTooltip (tooltip : String);
   --  Set tooltip string

   function GuiIconText (iconId : Interfaces.C.int; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get text with icon id prepended (if supported)
   pragma Import (C, GuiIconText, "GuiIconText");

   function GuiIconText (iconId : Interfaces.C.int; text : String) return String;
   --  Get text with icon id prepended (if supported)

   procedure GuiSetIconScale (scale : Interfaces.C.int);
   --  Set default icon drawing size
   pragma Import (C, GuiSetIconScale, "GuiSetIconScale");

   function GuiGetIcons return access Interfaces.C.unsigned;
   --  Get raygui icons data pointer
   pragma Import (C, GuiGetIcons, "GuiGetIcons");

   function GuiLoadIcons (fileName : Interfaces.C.Strings.chars_ptr; loadIconsName : Interfaces.C.C_bool) return access Interfaces.C.Strings.chars_ptr;
   --  Load raygui icons file (.rgi) into internal icons data
   pragma Import (C, GuiLoadIcons, "GuiLoadIcons");

   function GuiLoadIcons (fileName : String; loadIconsName : Interfaces.C.C_bool) return access Interfaces.C.Strings.chars_ptr;
   --  Load raygui icons file (.rgi) into internal icons data

   procedure GuiDrawIcon (iconId : Interfaces.C.int; posX : Interfaces.C.int; posY : Interfaces.C.int; pixelSize : Interfaces.C.int; color_p : Color);
   --  Draw icon using pixel size at specified position
   pragma Import (C, GuiDrawIcon, "GuiDrawIcon");

   function GuiWindowBox (bounds : Rectangle; title : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Window Box control, shows a window that can be closed
   pragma Import (C, GuiWindowBox, "GuiWindowBox");

   function GuiWindowBox (bounds : Rectangle; title : String) return Interfaces.C.int;
   --  Window Box control, shows a window that can be closed

   function GuiGroupBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Group Box control with text name
   pragma Import (C, GuiGroupBox, "GuiGroupBox");

   function GuiGroupBox (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Group Box control with text name

   function GuiLine (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Line separator control, could contain text
   pragma Import (C, GuiLine, "GuiLine");

   function GuiLine (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Line separator control, could contain text

   function GuiPanel (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Panel control, useful to group controls
   pragma Import (C, GuiPanel, "GuiPanel");

   function GuiPanel (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Panel control, useful to group controls

   function GuiTabBar (bounds : Rectangle; text : access Interfaces.C.Strings.chars_ptr; count : Interfaces.C.int; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Tab Bar control, returns TAB to be closed or -1
   pragma Import (C, GuiTabBar, "GuiTabBar");

   function GuiScrollPanel (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; content : Rectangle; scroll : access Vector2; view : access Rectangle) return Interfaces.C.int;
   --  Scroll Panel control
   pragma Import (C, GuiScrollPanel, "GuiScrollPanel");

   function GuiScrollPanel (bounds : Rectangle; text : String; content : Rectangle; scroll : access Vector2; view : access Rectangle) return Interfaces.C.int;
   --  Scroll Panel control

   function GuiLabel (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Label control, shows text
   pragma Import (C, GuiLabel, "GuiLabel");

   function GuiLabel (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Label control, shows text

   function GuiButton (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Button control, returns true when clicked
   pragma Import (C, GuiButton, "GuiButton");

   function GuiButton (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Button control, returns true when clicked

   function GuiLabelButton (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Label button control, show true when clicked
   pragma Import (C, GuiLabelButton, "GuiLabelButton");

   function GuiLabelButton (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Label button control, show true when clicked

   function GuiToggle (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; active : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Toggle Button control, returns true when active
   pragma Import (C, GuiToggle, "GuiToggle");

   function GuiToggle (bounds : Rectangle; text : String; active : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Toggle Button control, returns true when active

   function GuiToggleGroup (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Toggle Group control, returns active toggle index
   pragma Import (C, GuiToggleGroup, "GuiToggleGroup");

   function GuiToggleGroup (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Toggle Group control, returns active toggle index

   function GuiToggleSlider (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Toggle Slider control, returns true when clicked
   pragma Import (C, GuiToggleSlider, "GuiToggleSlider");

   function GuiToggleSlider (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Toggle Slider control, returns true when clicked

   function GuiCheckBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; checked : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Check Box control, returns true when active
   pragma Import (C, GuiCheckBox, "GuiCheckBox");

   function GuiCheckBox (bounds : Rectangle; text : String; checked : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Check Box control, returns true when active

   function GuiComboBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Combo Box control, returns selected item index
   pragma Import (C, GuiComboBox, "GuiComboBox");

   function GuiComboBox (bounds : Rectangle; text : String; active : access Interfaces.C.int) return Interfaces.C.int;
   --  Combo Box control, returns selected item index

   function GuiDropdownBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; active : access Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Dropdown Box control, returns selected item
   pragma Import (C, GuiDropdownBox, "GuiDropdownBox");

   function GuiDropdownBox (bounds : Rectangle; text : String; active : access Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Dropdown Box control, returns selected item

   function GuiSpinner (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Spinner control, returns selected value
   pragma Import (C, GuiSpinner, "GuiSpinner");

   function GuiSpinner (bounds : Rectangle; text : String; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Spinner control, returns selected value

   function GuiValueBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Value Box control, updates input text with numbers
   pragma Import (C, GuiValueBox, "GuiValueBox");

   function GuiValueBox (bounds : Rectangle; text : String; value : access Interfaces.C.int; minValue : Interfaces.C.int; maxValue : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Value Box control, updates input text with numbers

   function GuiTextBox (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; textSize : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Text Box control, updates input text
   pragma Import (C, GuiTextBox, "GuiTextBox");

   function GuiTextBox (bounds : Rectangle; text : String; textSize : Interfaces.C.int; editMode : Interfaces.C.C_bool) return Interfaces.C.int;
   --  Text Box control, updates input text

   function GuiSlider (bounds : Rectangle; textLeft : Interfaces.C.Strings.chars_ptr; textRight : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Slider control, returns selected value
   pragma Import (C, GuiSlider, "GuiSlider");

   function GuiSlider (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Slider control, returns selected value

   function GuiSliderBar (bounds : Rectangle; textLeft : Interfaces.C.Strings.chars_ptr; textRight : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Slider Bar control, returns selected value
   pragma Import (C, GuiSliderBar, "GuiSliderBar");

   function GuiSliderBar (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Slider Bar control, returns selected value

   function GuiProgressBar (bounds : Rectangle; textLeft : Interfaces.C.Strings.chars_ptr; textRight : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Progress Bar control, shows current progress value
   pragma Import (C, GuiProgressBar, "GuiProgressBar");

   function GuiProgressBar (bounds : Rectangle; textLeft : String; textRight : String; value : access Interfaces.C.C_float; minValue : Interfaces.C.C_float; maxValue : Interfaces.C.C_float) return Interfaces.C.int;
   --  Progress Bar control, shows current progress value

   function GuiStatusBar (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Status Bar control, shows info text
   pragma Import (C, GuiStatusBar, "GuiStatusBar");

   function GuiStatusBar (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Status Bar control, shows info text

   function GuiDummyRec (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Dummy control for placeholders
   pragma Import (C, GuiDummyRec, "GuiDummyRec");

   function GuiDummyRec (bounds : Rectangle; text : String) return Interfaces.C.int;
   --  Dummy control for placeholders

   function GuiGrid (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; spacing : Interfaces.C.C_float; subdivs : Interfaces.C.int; mouseCell : access Vector2) return Interfaces.C.int;
   --  Grid control, returns mouse cell position
   pragma Import (C, GuiGrid, "GuiGrid");

   function GuiGrid (bounds : Rectangle; text : String; spacing : Interfaces.C.C_float; subdivs : Interfaces.C.int; mouseCell : access Vector2) return Interfaces.C.int;
   --  Grid control, returns mouse cell position

   function GuiListView (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; scrollIndex : access Interfaces.C.int; active : access Interfaces.C.int) return Interfaces.C.int;
   --  List View control, returns selected list item index
   pragma Import (C, GuiListView, "GuiListView");

   function GuiListView (bounds : Rectangle; text : String; scrollIndex : access Interfaces.C.int; active : access Interfaces.C.int) return Interfaces.C.int;
   --  List View control, returns selected list item index

   function GuiListViewEx (bounds : Rectangle; text : C_String_Array_Access; count : Interfaces.C.int; scrollIndex : access Interfaces.C.int; active : access Interfaces.C.int; focus : access Interfaces.C.int) return Interfaces.C.int;
   --  List View with extended parameters
   pragma Import (C, GuiListViewEx, "GuiListViewEx");

   function GuiMessageBox (bounds : Rectangle; title : Interfaces.C.Strings.chars_ptr; message : Interfaces.C.Strings.chars_ptr; buttons : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Message Box control, displays a message
   pragma Import (C, GuiMessageBox, "GuiMessageBox");

   function GuiMessageBox (bounds : Rectangle; title : String; message : String; buttons : String) return Interfaces.C.int;
   --  Message Box control, displays a message

   function GuiTextInputBox (bounds : Rectangle; title : Interfaces.C.Strings.chars_ptr; message : Interfaces.C.Strings.chars_ptr; buttons : Interfaces.C.Strings.chars_ptr; text : Interfaces.C.Strings.chars_ptr; textMaxSize : Interfaces.C.int; secretViewActive : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Text Input Box control, ask for text, supports secret
   pragma Import (C, GuiTextInputBox, "GuiTextInputBox");

   function GuiTextInputBox (bounds : Rectangle; title : String; message : String; buttons : String; text : Interfaces.C.Strings.chars_ptr; textMaxSize : Interfaces.C.int; secretViewActive : access Interfaces.C.C_bool) return Interfaces.C.int;
   --  Text Input Box control, ask for text, supports secret

   function GuiColorPicker (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; color_p : access Color) return Interfaces.C.int;
   --  Color Picker control (multiple color controls)
   pragma Import (C, GuiColorPicker, "GuiColorPicker");

   function GuiColorPicker (bounds : Rectangle; text : String; color_p : access Color) return Interfaces.C.int;
   --  Color Picker control (multiple color controls)

   function GuiColorPanel (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; color_p : access Color) return Interfaces.C.int;
   --  Color Panel control
   pragma Import (C, GuiColorPanel, "GuiColorPanel");

   function GuiColorPanel (bounds : Rectangle; text : String; color_p : access Color) return Interfaces.C.int;
   --  Color Panel control

   function GuiColorBarAlpha (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; alpha : access Interfaces.C.C_float) return Interfaces.C.int;
   --  Color Bar Alpha control
   pragma Import (C, GuiColorBarAlpha, "GuiColorBarAlpha");

   function GuiColorBarAlpha (bounds : Rectangle; text : String; alpha : access Interfaces.C.C_float) return Interfaces.C.int;
   --  Color Bar Alpha control

   function GuiColorBarHue (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; value : access Interfaces.C.C_float) return Interfaces.C.int;
   --  Color Bar Hue control
   pragma Import (C, GuiColorBarHue, "GuiColorBarHue");

   function GuiColorBarHue (bounds : Rectangle; text : String; value : access Interfaces.C.C_float) return Interfaces.C.int;
   --  Color Bar Hue control

   function GuiColorPickerHSV (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; colorHsv : access Vector3) return Interfaces.C.int;
   --  Color Picker control that avoids conversion to RGB on each call (multiple color controls)
   pragma Import (C, GuiColorPickerHSV, "GuiColorPickerHSV");

   function GuiColorPickerHSV (bounds : Rectangle; text : String; colorHsv : access Vector3) return Interfaces.C.int;
   --  Color Picker control that avoids conversion to RGB on each call (multiple color controls)

   function GuiColorPanelHSV (bounds : Rectangle; text : Interfaces.C.Strings.chars_ptr; colorHsv : access Vector3) return Interfaces.C.int;
   --  Color Panel control that returns HSV color value, used by GuiColorPickerHSV()
   pragma Import (C, GuiColorPanelHSV, "GuiColorPanelHSV");

   function GuiColorPanelHSV (bounds : Rectangle; text : String; colorHsv : access Vector3) return Interfaces.C.int;
   --  Color Panel control that returns HSV color value, used by GuiColorPickerHSV()

   RAYGUI_VERSION_MAJOR : constant := 4;
   RAYGUI_VERSION_MINOR : constant := 0;
   RAYGUI_VERSION_PATCH : constant := 0;
   RAYGUI_VERSION : constant String := "4.0";
   SCROLLBAR_LEFT_SIDE : constant := 0;
   SCROLLBAR_RIGHT_SIDE : constant := 1;
end Raylib.GUI;
