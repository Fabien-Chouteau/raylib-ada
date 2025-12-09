with Interfaces.C;
with Interfaces.C.Strings;
with System;
package Raylib
  with Preelaborate
is
   pragma Style_Checks ("M2000");
   type Float2 is array (0 .. 1) of Interfaces.C.C_float;
   type Float4 is array (0 .. 3) of Interfaces.C.C_float;
   type Int4 is array (0 .. 3) of Interfaces.C.int;
   subtype String32 is String (1 .. 32);
   type C_String_Array
    is array (Interfaces.C.unsigned) of aliased Interfaces.C.Strings.chars_ptr
    with Convention => C;
   type C_String_Array_Access is access all C_String_Array;

   type ConfigFlags is new Interfaces.C.unsigned;
   --  System/Window config flags

   FLAG_VSYNC_HINT : constant ConfigFlags := 64; -- Set to try enabling V-Sync on GPU
   FLAG_FULLSCREEN_MODE : constant ConfigFlags := 2; -- Set to run program in fullscreen
   FLAG_WINDOW_RESIZABLE : constant ConfigFlags := 4; -- Set to allow resizable window
   FLAG_WINDOW_UNDECORATED : constant ConfigFlags := 8; -- Set to disable window decoration (frame and buttons)
   FLAG_WINDOW_HIDDEN : constant ConfigFlags := 128; -- Set to hide window
   FLAG_WINDOW_MINIMIZED : constant ConfigFlags := 512; -- Set to minimize window (iconify)
   FLAG_WINDOW_MAXIMIZED : constant ConfigFlags := 1024; -- Set to maximize window (expanded to monitor)
   FLAG_WINDOW_UNFOCUSED : constant ConfigFlags := 2048; -- Set to window non focused
   FLAG_WINDOW_TOPMOST : constant ConfigFlags := 4096; -- Set to window always on top
   FLAG_WINDOW_ALWAYS_RUN : constant ConfigFlags := 256; -- Set to allow windows running while minimized
   FLAG_WINDOW_TRANSPARENT : constant ConfigFlags := 16; -- Set to allow transparent framebuffer
   FLAG_WINDOW_HIGHDPI : constant ConfigFlags := 8192; -- Set to support HighDPI
   FLAG_WINDOW_MOUSE_PASSTHROUGH : constant ConfigFlags := 16384; -- Set to support mouse passthrough, only supported when FLAG_WINDOW_UNDECORATED
   FLAG_BORDERLESS_WINDOWED_MODE : constant ConfigFlags := 32768; -- Set to run program in borderless windowed mode
   FLAG_MSAA_4X_HINT : constant ConfigFlags := 32; -- Set to try enabling MSAA 4X
   FLAG_INTERLACED_HINT : constant ConfigFlags := 65536; -- Set to try enabling interlaced video format (for V3D)

   type TraceLogLevel is
     (
         LOG_ALL -- Display all logs
       , LOG_TRACE -- Trace logging, intended for internal use only
       , LOG_DEBUG -- Debug logging, used for internal debugging, it should be disabled on release builds
       , LOG_INFO -- Info logging, used for program execution info
       , LOG_WARNING -- Warning logging, used on recoverable failures
       , LOG_ERROR -- Error logging, used on unrecoverable failures
       , LOG_FATAL -- Fatal logging, used to abort program: exit(EXIT_FAILURE)
       , LOG_NONE -- Disable logging
     )
     with Convention => C;
   --  Trace log level

   for TraceLogLevel use
     (
         LOG_ALL => 0
       , LOG_TRACE => 1
       , LOG_DEBUG => 2
       , LOG_INFO => 3
       , LOG_WARNING => 4
       , LOG_ERROR => 5
       , LOG_FATAL => 6
       , LOG_NONE => 7
     );

   type KeyboardKey is new Interfaces.C.int;
   --  Keyboard keys (US keyboard layout)

   KEY_NULL : constant KeyboardKey := 0; -- Key: NULL, used for no key pressed
   KEY_APOSTROPHE : constant KeyboardKey := 39; -- Key: '
   KEY_COMMA : constant KeyboardKey := 44; -- Key: ,
   KEY_MINUS : constant KeyboardKey := 45; -- Key: -
   KEY_PERIOD : constant KeyboardKey := 46; -- Key: .
   KEY_SLASH : constant KeyboardKey := 47; -- Key: /
   KEY_ZERO : constant KeyboardKey := 48; -- Key: 0
   KEY_ONE : constant KeyboardKey := 49; -- Key: 1
   KEY_TWO : constant KeyboardKey := 50; -- Key: 2
   KEY_THREE : constant KeyboardKey := 51; -- Key: 3
   KEY_FOUR : constant KeyboardKey := 52; -- Key: 4
   KEY_FIVE : constant KeyboardKey := 53; -- Key: 5
   KEY_SIX : constant KeyboardKey := 54; -- Key: 6
   KEY_SEVEN : constant KeyboardKey := 55; -- Key: 7
   KEY_EIGHT : constant KeyboardKey := 56; -- Key: 8
   KEY_NINE : constant KeyboardKey := 57; -- Key: 9
   KEY_SEMICOLON : constant KeyboardKey := 59; -- Key: ;
   KEY_EQUAL : constant KeyboardKey := 61; -- Key: =
   KEY_A : constant KeyboardKey := 65; -- Key: A | a
   KEY_B : constant KeyboardKey := 66; -- Key: B | b
   KEY_C : constant KeyboardKey := 67; -- Key: C | c
   KEY_D : constant KeyboardKey := 68; -- Key: D | d
   KEY_E : constant KeyboardKey := 69; -- Key: E | e
   KEY_F : constant KeyboardKey := 70; -- Key: F | f
   KEY_G : constant KeyboardKey := 71; -- Key: G | g
   KEY_H : constant KeyboardKey := 72; -- Key: H | h
   KEY_I : constant KeyboardKey := 73; -- Key: I | i
   KEY_J : constant KeyboardKey := 74; -- Key: J | j
   KEY_K : constant KeyboardKey := 75; -- Key: K | k
   KEY_L : constant KeyboardKey := 76; -- Key: L | l
   KEY_M : constant KeyboardKey := 77; -- Key: M | m
   KEY_N : constant KeyboardKey := 78; -- Key: N | n
   KEY_O : constant KeyboardKey := 79; -- Key: O | o
   KEY_P : constant KeyboardKey := 80; -- Key: P | p
   KEY_Q : constant KeyboardKey := 81; -- Key: Q | q
   KEY_R : constant KeyboardKey := 82; -- Key: R | r
   KEY_S : constant KeyboardKey := 83; -- Key: S | s
   KEY_T : constant KeyboardKey := 84; -- Key: T | t
   KEY_U : constant KeyboardKey := 85; -- Key: U | u
   KEY_V : constant KeyboardKey := 86; -- Key: V | v
   KEY_W : constant KeyboardKey := 87; -- Key: W | w
   KEY_X : constant KeyboardKey := 88; -- Key: X | x
   KEY_Y : constant KeyboardKey := 89; -- Key: Y | y
   KEY_Z : constant KeyboardKey := 90; -- Key: Z | z
   KEY_LEFT_BRACKET : constant KeyboardKey := 91; -- Key: [
   KEY_BACKSLASH : constant KeyboardKey := 92; -- Key: '\'
   KEY_RIGHT_BRACKET : constant KeyboardKey := 93; -- Key: ]
   KEY_GRAVE : constant KeyboardKey := 96; -- Key: `
   KEY_SPACE : constant KeyboardKey := 32; -- Key: Space
   KEY_ESCAPE : constant KeyboardKey := 256; -- Key: Esc
   KEY_ENTER : constant KeyboardKey := 257; -- Key: Enter
   KEY_TAB : constant KeyboardKey := 258; -- Key: Tab
   KEY_BACKSPACE : constant KeyboardKey := 259; -- Key: Backspace
   KEY_INSERT : constant KeyboardKey := 260; -- Key: Ins
   KEY_DELETE : constant KeyboardKey := 261; -- Key: Del
   KEY_RIGHT : constant KeyboardKey := 262; -- Key: Cursor right
   KEY_LEFT : constant KeyboardKey := 263; -- Key: Cursor left
   KEY_DOWN : constant KeyboardKey := 264; -- Key: Cursor down
   KEY_UP : constant KeyboardKey := 265; -- Key: Cursor up
   KEY_PAGE_UP : constant KeyboardKey := 266; -- Key: Page up
   KEY_PAGE_DOWN : constant KeyboardKey := 267; -- Key: Page down
   KEY_HOME : constant KeyboardKey := 268; -- Key: Home
   KEY_END : constant KeyboardKey := 269; -- Key: End
   KEY_CAPS_LOCK : constant KeyboardKey := 280; -- Key: Caps lock
   KEY_SCROLL_LOCK : constant KeyboardKey := 281; -- Key: Scroll down
   KEY_NUM_LOCK : constant KeyboardKey := 282; -- Key: Num lock
   KEY_PRINT_SCREEN : constant KeyboardKey := 283; -- Key: Print screen
   KEY_PAUSE : constant KeyboardKey := 284; -- Key: Pause
   KEY_F1 : constant KeyboardKey := 290; -- Key: F1
   KEY_F2 : constant KeyboardKey := 291; -- Key: F2
   KEY_F3 : constant KeyboardKey := 292; -- Key: F3
   KEY_F4 : constant KeyboardKey := 293; -- Key: F4
   KEY_F5 : constant KeyboardKey := 294; -- Key: F5
   KEY_F6 : constant KeyboardKey := 295; -- Key: F6
   KEY_F7 : constant KeyboardKey := 296; -- Key: F7
   KEY_F8 : constant KeyboardKey := 297; -- Key: F8
   KEY_F9 : constant KeyboardKey := 298; -- Key: F9
   KEY_F10 : constant KeyboardKey := 299; -- Key: F10
   KEY_F11 : constant KeyboardKey := 300; -- Key: F11
   KEY_F12 : constant KeyboardKey := 301; -- Key: F12
   KEY_LEFT_SHIFT : constant KeyboardKey := 340; -- Key: Shift left
   KEY_LEFT_CONTROL : constant KeyboardKey := 341; -- Key: Control left
   KEY_LEFT_ALT : constant KeyboardKey := 342; -- Key: Alt left
   KEY_LEFT_SUPER : constant KeyboardKey := 343; -- Key: Super left
   KEY_RIGHT_SHIFT : constant KeyboardKey := 344; -- Key: Shift right
   KEY_RIGHT_CONTROL : constant KeyboardKey := 345; -- Key: Control right
   KEY_RIGHT_ALT : constant KeyboardKey := 346; -- Key: Alt right
   KEY_RIGHT_SUPER : constant KeyboardKey := 347; -- Key: Super right
   KEY_KB_MENU : constant KeyboardKey := 348; -- Key: KB menu
   KEY_KP_0 : constant KeyboardKey := 320; -- Key: Keypad 0
   KEY_KP_1 : constant KeyboardKey := 321; -- Key: Keypad 1
   KEY_KP_2 : constant KeyboardKey := 322; -- Key: Keypad 2
   KEY_KP_3 : constant KeyboardKey := 323; -- Key: Keypad 3
   KEY_KP_4 : constant KeyboardKey := 324; -- Key: Keypad 4
   KEY_KP_5 : constant KeyboardKey := 325; -- Key: Keypad 5
   KEY_KP_6 : constant KeyboardKey := 326; -- Key: Keypad 6
   KEY_KP_7 : constant KeyboardKey := 327; -- Key: Keypad 7
   KEY_KP_8 : constant KeyboardKey := 328; -- Key: Keypad 8
   KEY_KP_9 : constant KeyboardKey := 329; -- Key: Keypad 9
   KEY_KP_DECIMAL : constant KeyboardKey := 330; -- Key: Keypad .
   KEY_KP_DIVIDE : constant KeyboardKey := 331; -- Key: Keypad /
   KEY_KP_MULTIPLY : constant KeyboardKey := 332; -- Key: Keypad *
   KEY_KP_SUBTRACT : constant KeyboardKey := 333; -- Key: Keypad -
   KEY_KP_ADD : constant KeyboardKey := 334; -- Key: Keypad +
   KEY_KP_ENTER : constant KeyboardKey := 335; -- Key: Keypad Enter
   KEY_KP_EQUAL : constant KeyboardKey := 336; -- Key: Keypad =
   KEY_BACK : constant KeyboardKey := 4; -- Key: Android back button
   KEY_MENU : constant KeyboardKey := 82; -- Key: Android menu button
   KEY_VOLUME_UP : constant KeyboardKey := 24; -- Key: Android volume up button
   KEY_VOLUME_DOWN : constant KeyboardKey := 25; -- Key: Android volume down button

   type MouseButton is new Interfaces.C.int;
   --  Mouse buttons

   MOUSE_BUTTON_LEFT : constant MouseButton := 0; -- Mouse button left
   MOUSE_BUTTON_RIGHT : constant MouseButton := 1; -- Mouse button right
   MOUSE_BUTTON_MIDDLE : constant MouseButton := 2; -- Mouse button middle (pressed wheel)
   MOUSE_BUTTON_SIDE : constant MouseButton := 3; -- Mouse button side (advanced mouse device)
   MOUSE_BUTTON_EXTRA : constant MouseButton := 4; -- Mouse button extra (advanced mouse device)
   MOUSE_BUTTON_FORWARD : constant MouseButton := 5; -- Mouse button forward (advanced mouse device)
   MOUSE_BUTTON_BACK : constant MouseButton := 6; -- Mouse button back (advanced mouse device)

   type MouseCursor is
     (
         MOUSE_CURSOR_DEFAULT -- Default pointer shape
       , MOUSE_CURSOR_ARROW -- Arrow shape
       , MOUSE_CURSOR_IBEAM -- Text writing cursor shape
       , MOUSE_CURSOR_CROSSHAIR -- Cross shape
       , MOUSE_CURSOR_POINTING_HAND -- Pointing hand cursor
       , MOUSE_CURSOR_RESIZE_EW -- Horizontal resize/move arrow shape
       , MOUSE_CURSOR_RESIZE_NS -- Vertical resize/move arrow shape
       , MOUSE_CURSOR_RESIZE_NWSE -- Top-left to bottom-right diagonal resize/move arrow shape
       , MOUSE_CURSOR_RESIZE_NESW -- The top-right to bottom-left diagonal resize/move arrow shape
       , MOUSE_CURSOR_RESIZE_ALL -- The omnidirectional resize/move cursor shape
       , MOUSE_CURSOR_NOT_ALLOWED -- The operation-not-allowed shape
     )
     with Convention => C;
   --  Mouse cursor

   for MouseCursor use
     (
         MOUSE_CURSOR_DEFAULT => 0
       , MOUSE_CURSOR_ARROW => 1
       , MOUSE_CURSOR_IBEAM => 2
       , MOUSE_CURSOR_CROSSHAIR => 3
       , MOUSE_CURSOR_POINTING_HAND => 4
       , MOUSE_CURSOR_RESIZE_EW => 5
       , MOUSE_CURSOR_RESIZE_NS => 6
       , MOUSE_CURSOR_RESIZE_NWSE => 7
       , MOUSE_CURSOR_RESIZE_NESW => 8
       , MOUSE_CURSOR_RESIZE_ALL => 9
       , MOUSE_CURSOR_NOT_ALLOWED => 10
     );

   type GamepadButton is new Interfaces.C.int;
   --  Gamepad buttons

   GAMEPAD_BUTTON_UNKNOWN : constant GamepadButton := 0; -- Unknown button, just for error checking
   GAMEPAD_BUTTON_LEFT_FACE_UP : constant GamepadButton := 1; -- Gamepad left DPAD up button
   GAMEPAD_BUTTON_LEFT_FACE_RIGHT : constant GamepadButton := 2; -- Gamepad left DPAD right button
   GAMEPAD_BUTTON_LEFT_FACE_DOWN : constant GamepadButton := 3; -- Gamepad left DPAD down button
   GAMEPAD_BUTTON_LEFT_FACE_LEFT : constant GamepadButton := 4; -- Gamepad left DPAD left button
   GAMEPAD_BUTTON_RIGHT_FACE_UP : constant GamepadButton := 5; -- Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
   GAMEPAD_BUTTON_RIGHT_FACE_RIGHT : constant GamepadButton := 6; -- Gamepad right button right (i.e. PS3: Square, Xbox: X)
   GAMEPAD_BUTTON_RIGHT_FACE_DOWN : constant GamepadButton := 7; -- Gamepad right button down (i.e. PS3: Cross, Xbox: A)
   GAMEPAD_BUTTON_RIGHT_FACE_LEFT : constant GamepadButton := 8; -- Gamepad right button left (i.e. PS3: Circle, Xbox: B)
   GAMEPAD_BUTTON_LEFT_TRIGGER_1 : constant GamepadButton := 9; -- Gamepad top/back trigger left (first), it could be a trailing button
   GAMEPAD_BUTTON_LEFT_TRIGGER_2 : constant GamepadButton := 10; -- Gamepad top/back trigger left (second), it could be a trailing button
   GAMEPAD_BUTTON_RIGHT_TRIGGER_1 : constant GamepadButton := 11; -- Gamepad top/back trigger right (one), it could be a trailing button
   GAMEPAD_BUTTON_RIGHT_TRIGGER_2 : constant GamepadButton := 12; -- Gamepad top/back trigger right (second), it could be a trailing button
   GAMEPAD_BUTTON_MIDDLE_LEFT : constant GamepadButton := 13; -- Gamepad center buttons, left one (i.e. PS3: Select)
   GAMEPAD_BUTTON_MIDDLE : constant GamepadButton := 14; -- Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
   GAMEPAD_BUTTON_MIDDLE_RIGHT : constant GamepadButton := 15; -- Gamepad center buttons, right one (i.e. PS3: Start)
   GAMEPAD_BUTTON_LEFT_THUMB : constant GamepadButton := 16; -- Gamepad joystick pressed button left
   GAMEPAD_BUTTON_RIGHT_THUMB : constant GamepadButton := 17; -- Gamepad joystick pressed button right

   type GamepadAxis is new Interfaces.C.int;
   --  Gamepad axis

   GAMEPAD_AXIS_LEFT_X : constant GamepadAxis := 0; -- Gamepad left stick X axis
   GAMEPAD_AXIS_LEFT_Y : constant GamepadAxis := 1; -- Gamepad left stick Y axis
   GAMEPAD_AXIS_RIGHT_X : constant GamepadAxis := 2; -- Gamepad right stick X axis
   GAMEPAD_AXIS_RIGHT_Y : constant GamepadAxis := 3; -- Gamepad right stick Y axis
   GAMEPAD_AXIS_LEFT_TRIGGER : constant GamepadAxis := 4; -- Gamepad back trigger left, pressure level: [1..-1]
   GAMEPAD_AXIS_RIGHT_TRIGGER : constant GamepadAxis := 5; -- Gamepad back trigger right, pressure level: [1..-1]

   type MaterialMapIndex is
     (
         MATERIAL_MAP_ALBEDO -- Albedo material (same as: MATERIAL_MAP_DIFFUSE)
       , MATERIAL_MAP_METALNESS -- Metalness material (same as: MATERIAL_MAP_SPECULAR)
       , MATERIAL_MAP_NORMAL -- Normal material
       , MATERIAL_MAP_ROUGHNESS -- Roughness material
       , MATERIAL_MAP_OCCLUSION -- Ambient occlusion material
       , MATERIAL_MAP_EMISSION -- Emission material
       , MATERIAL_MAP_HEIGHT -- Heightmap material
       , MATERIAL_MAP_CUBEMAP -- Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
       , MATERIAL_MAP_IRRADIANCE -- Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
       , MATERIAL_MAP_PREFILTER -- Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
       , MATERIAL_MAP_BRDF -- Brdf material
     )
     with Convention => C;
   --  Material map index

   for MaterialMapIndex use
     (
         MATERIAL_MAP_ALBEDO => 0
       , MATERIAL_MAP_METALNESS => 1
       , MATERIAL_MAP_NORMAL => 2
       , MATERIAL_MAP_ROUGHNESS => 3
       , MATERIAL_MAP_OCCLUSION => 4
       , MATERIAL_MAP_EMISSION => 5
       , MATERIAL_MAP_HEIGHT => 6
       , MATERIAL_MAP_CUBEMAP => 7
       , MATERIAL_MAP_IRRADIANCE => 8
       , MATERIAL_MAP_PREFILTER => 9
       , MATERIAL_MAP_BRDF => 10
     );

   type ShaderLocationIndex is
     (
         SHADER_LOC_VERTEX_POSITION -- Shader location: vertex attribute: position
       , SHADER_LOC_VERTEX_TEXCOORD01 -- Shader location: vertex attribute: texcoord01
       , SHADER_LOC_VERTEX_TEXCOORD02 -- Shader location: vertex attribute: texcoord02
       , SHADER_LOC_VERTEX_NORMAL -- Shader location: vertex attribute: normal
       , SHADER_LOC_VERTEX_TANGENT -- Shader location: vertex attribute: tangent
       , SHADER_LOC_VERTEX_COLOR -- Shader location: vertex attribute: color
       , SHADER_LOC_MATRIX_MVP -- Shader location: matrix uniform: model-view-projection
       , SHADER_LOC_MATRIX_VIEW -- Shader location: matrix uniform: view (camera transform)
       , SHADER_LOC_MATRIX_PROJECTION -- Shader location: matrix uniform: projection
       , SHADER_LOC_MATRIX_MODEL -- Shader location: matrix uniform: model (transform)
       , SHADER_LOC_MATRIX_NORMAL -- Shader location: matrix uniform: normal
       , SHADER_LOC_VECTOR_VIEW -- Shader location: vector uniform: view
       , SHADER_LOC_COLOR_DIFFUSE -- Shader location: vector uniform: diffuse color
       , SHADER_LOC_COLOR_SPECULAR -- Shader location: vector uniform: specular color
       , SHADER_LOC_COLOR_AMBIENT -- Shader location: vector uniform: ambient color
       , SHADER_LOC_MAP_ALBEDO -- Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
       , SHADER_LOC_MAP_METALNESS -- Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
       , SHADER_LOC_MAP_NORMAL -- Shader location: sampler2d texture: normal
       , SHADER_LOC_MAP_ROUGHNESS -- Shader location: sampler2d texture: roughness
       , SHADER_LOC_MAP_OCCLUSION -- Shader location: sampler2d texture: occlusion
       , SHADER_LOC_MAP_EMISSION -- Shader location: sampler2d texture: emission
       , SHADER_LOC_MAP_HEIGHT -- Shader location: sampler2d texture: height
       , SHADER_LOC_MAP_CUBEMAP -- Shader location: samplerCube texture: cubemap
       , SHADER_LOC_MAP_IRRADIANCE -- Shader location: samplerCube texture: irradiance
       , SHADER_LOC_MAP_PREFILTER -- Shader location: samplerCube texture: prefilter
       , SHADER_LOC_MAP_BRDF -- Shader location: sampler2d texture: brdf
     )
     with Convention => C;
   --  Shader location index

   for ShaderLocationIndex use
     (
         SHADER_LOC_VERTEX_POSITION => 0
       , SHADER_LOC_VERTEX_TEXCOORD01 => 1
       , SHADER_LOC_VERTEX_TEXCOORD02 => 2
       , SHADER_LOC_VERTEX_NORMAL => 3
       , SHADER_LOC_VERTEX_TANGENT => 4
       , SHADER_LOC_VERTEX_COLOR => 5
       , SHADER_LOC_MATRIX_MVP => 6
       , SHADER_LOC_MATRIX_VIEW => 7
       , SHADER_LOC_MATRIX_PROJECTION => 8
       , SHADER_LOC_MATRIX_MODEL => 9
       , SHADER_LOC_MATRIX_NORMAL => 10
       , SHADER_LOC_VECTOR_VIEW => 11
       , SHADER_LOC_COLOR_DIFFUSE => 12
       , SHADER_LOC_COLOR_SPECULAR => 13
       , SHADER_LOC_COLOR_AMBIENT => 14
       , SHADER_LOC_MAP_ALBEDO => 15
       , SHADER_LOC_MAP_METALNESS => 16
       , SHADER_LOC_MAP_NORMAL => 17
       , SHADER_LOC_MAP_ROUGHNESS => 18
       , SHADER_LOC_MAP_OCCLUSION => 19
       , SHADER_LOC_MAP_EMISSION => 20
       , SHADER_LOC_MAP_HEIGHT => 21
       , SHADER_LOC_MAP_CUBEMAP => 22
       , SHADER_LOC_MAP_IRRADIANCE => 23
       , SHADER_LOC_MAP_PREFILTER => 24
       , SHADER_LOC_MAP_BRDF => 25
     );

   type ShaderUniformDataType is
     (
         SHADER_UNIFORM_FLOAT -- Shader uniform type: float
       , SHADER_UNIFORM_VEC2 -- Shader uniform type: vec2 (2 float)
       , SHADER_UNIFORM_VEC3 -- Shader uniform type: vec3 (3 float)
       , SHADER_UNIFORM_VEC4 -- Shader uniform type: vec4 (4 float)
       , SHADER_UNIFORM_INT -- Shader uniform type: int
       , SHADER_UNIFORM_IVEC2 -- Shader uniform type: ivec2 (2 int)
       , SHADER_UNIFORM_IVEC3 -- Shader uniform type: ivec3 (3 int)
       , SHADER_UNIFORM_IVEC4 -- Shader uniform type: ivec4 (4 int)
       , SHADER_UNIFORM_SAMPLER2D -- Shader uniform type: sampler2d
     )
     with Convention => C;
   --  Shader uniform data type

   for ShaderUniformDataType use
     (
         SHADER_UNIFORM_FLOAT => 0
       , SHADER_UNIFORM_VEC2 => 1
       , SHADER_UNIFORM_VEC3 => 2
       , SHADER_UNIFORM_VEC4 => 3
       , SHADER_UNIFORM_INT => 4
       , SHADER_UNIFORM_IVEC2 => 5
       , SHADER_UNIFORM_IVEC3 => 6
       , SHADER_UNIFORM_IVEC4 => 7
       , SHADER_UNIFORM_SAMPLER2D => 8
     );

   type ShaderAttributeDataType is
     (
         SHADER_ATTRIB_FLOAT -- Shader attribute type: float
       , SHADER_ATTRIB_VEC2 -- Shader attribute type: vec2 (2 float)
       , SHADER_ATTRIB_VEC3 -- Shader attribute type: vec3 (3 float)
       , SHADER_ATTRIB_VEC4 -- Shader attribute type: vec4 (4 float)
     )
     with Convention => C;
   --  Shader attribute data types

   for ShaderAttributeDataType use
     (
         SHADER_ATTRIB_FLOAT => 0
       , SHADER_ATTRIB_VEC2 => 1
       , SHADER_ATTRIB_VEC3 => 2
       , SHADER_ATTRIB_VEC4 => 3
     );

   type PixelFormat is
     (
         PIXELFORMAT_UNCOMPRESSED_GRAYSCALE -- 8 bit per pixel (no alpha)
       , PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA -- 8*2 bpp (2 channels)
       , PIXELFORMAT_UNCOMPRESSED_R5G6B5 -- 16 bpp
       , PIXELFORMAT_UNCOMPRESSED_R8G8B8 -- 24 bpp
       , PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 -- 16 bpp (1 bit alpha)
       , PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 -- 16 bpp (4 bit alpha)
       , PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 -- 32 bpp
       , PIXELFORMAT_UNCOMPRESSED_R32 -- 32 bpp (1 channel - float)
       , PIXELFORMAT_UNCOMPRESSED_R32G32B32 -- 32*3 bpp (3 channels - float)
       , PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 -- 32*4 bpp (4 channels - float)
       , PIXELFORMAT_UNCOMPRESSED_R16 -- 16 bpp (1 channel - half float)
       , PIXELFORMAT_UNCOMPRESSED_R16G16B16 -- 16*3 bpp (3 channels - half float)
       , PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 -- 16*4 bpp (4 channels - half float)
       , PIXELFORMAT_COMPRESSED_DXT1_RGB -- 4 bpp (no alpha)
       , PIXELFORMAT_COMPRESSED_DXT1_RGBA -- 4 bpp (1 bit alpha)
       , PIXELFORMAT_COMPRESSED_DXT3_RGBA -- 8 bpp
       , PIXELFORMAT_COMPRESSED_DXT5_RGBA -- 8 bpp
       , PIXELFORMAT_COMPRESSED_ETC1_RGB -- 4 bpp
       , PIXELFORMAT_COMPRESSED_ETC2_RGB -- 4 bpp
       , PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA -- 8 bpp
       , PIXELFORMAT_COMPRESSED_PVRT_RGB -- 4 bpp
       , PIXELFORMAT_COMPRESSED_PVRT_RGBA -- 4 bpp
       , PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA -- 8 bpp
       , PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA -- 2 bpp
     )
     with Convention => C;
   --  Pixel formats

   for PixelFormat use
     (
         PIXELFORMAT_UNCOMPRESSED_GRAYSCALE => 1
       , PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA => 2
       , PIXELFORMAT_UNCOMPRESSED_R5G6B5 => 3
       , PIXELFORMAT_UNCOMPRESSED_R8G8B8 => 4
       , PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 => 5
       , PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 => 6
       , PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 => 7
       , PIXELFORMAT_UNCOMPRESSED_R32 => 8
       , PIXELFORMAT_UNCOMPRESSED_R32G32B32 => 9
       , PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 => 10
       , PIXELFORMAT_UNCOMPRESSED_R16 => 11
       , PIXELFORMAT_UNCOMPRESSED_R16G16B16 => 12
       , PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 => 13
       , PIXELFORMAT_COMPRESSED_DXT1_RGB => 14
       , PIXELFORMAT_COMPRESSED_DXT1_RGBA => 15
       , PIXELFORMAT_COMPRESSED_DXT3_RGBA => 16
       , PIXELFORMAT_COMPRESSED_DXT5_RGBA => 17
       , PIXELFORMAT_COMPRESSED_ETC1_RGB => 18
       , PIXELFORMAT_COMPRESSED_ETC2_RGB => 19
       , PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA => 20
       , PIXELFORMAT_COMPRESSED_PVRT_RGB => 21
       , PIXELFORMAT_COMPRESSED_PVRT_RGBA => 22
       , PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA => 23
       , PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA => 24
     );

   type TextureFilter is
     (
         TEXTURE_FILTER_POINT -- No filter, just pixel approximation
       , TEXTURE_FILTER_BILINEAR -- Linear filtering
       , TEXTURE_FILTER_TRILINEAR -- Trilinear filtering (linear with mipmaps)
       , TEXTURE_FILTER_ANISOTROPIC_4X -- Anisotropic filtering 4x
       , TEXTURE_FILTER_ANISOTROPIC_8X -- Anisotropic filtering 8x
       , TEXTURE_FILTER_ANISOTROPIC_16X -- Anisotropic filtering 16x
     )
     with Convention => C;
   --  Texture parameters: filter mode

   for TextureFilter use
     (
         TEXTURE_FILTER_POINT => 0
       , TEXTURE_FILTER_BILINEAR => 1
       , TEXTURE_FILTER_TRILINEAR => 2
       , TEXTURE_FILTER_ANISOTROPIC_4X => 3
       , TEXTURE_FILTER_ANISOTROPIC_8X => 4
       , TEXTURE_FILTER_ANISOTROPIC_16X => 5
     );

   type TextureWrap is
     (
         TEXTURE_WRAP_REPEAT -- Repeats texture in tiled mode
       , TEXTURE_WRAP_CLAMP -- Clamps texture to edge pixel in tiled mode
       , TEXTURE_WRAP_MIRROR_REPEAT -- Mirrors and repeats the texture in tiled mode
       , TEXTURE_WRAP_MIRROR_CLAMP -- Mirrors and clamps to border the texture in tiled mode
     )
     with Convention => C;
   --  Texture parameters: wrap mode

   for TextureWrap use
     (
         TEXTURE_WRAP_REPEAT => 0
       , TEXTURE_WRAP_CLAMP => 1
       , TEXTURE_WRAP_MIRROR_REPEAT => 2
       , TEXTURE_WRAP_MIRROR_CLAMP => 3
     );

   type CubemapLayout is
     (
         CUBEMAP_LAYOUT_AUTO_DETECT -- Automatically detect layout type
       , CUBEMAP_LAYOUT_LINE_VERTICAL -- Layout is defined by a vertical line with faces
       , CUBEMAP_LAYOUT_LINE_HORIZONTAL -- Layout is defined by a horizontal line with faces
       , CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR -- Layout is defined by a 3x4 cross with cubemap faces
       , CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE -- Layout is defined by a 4x3 cross with cubemap faces
       , CUBEMAP_LAYOUT_PANORAMA -- Layout is defined by a panorama image (equirrectangular map)
     )
     with Convention => C;
   --  Cubemap layouts

   for CubemapLayout use
     (
         CUBEMAP_LAYOUT_AUTO_DETECT => 0
       , CUBEMAP_LAYOUT_LINE_VERTICAL => 1
       , CUBEMAP_LAYOUT_LINE_HORIZONTAL => 2
       , CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR => 3
       , CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE => 4
       , CUBEMAP_LAYOUT_PANORAMA => 5
     );

   type FontType is
     (
         FONT_DEFAULT -- Default font generation, anti-aliased
       , FONT_BITMAP -- Bitmap font generation, no anti-aliasing
       , FONT_SDF -- SDF font generation, requires external shader
     )
     with Convention => C;
   --  Font type, defines generation method

   for FontType use
     (
         FONT_DEFAULT => 0
       , FONT_BITMAP => 1
       , FONT_SDF => 2
     );

   type BlendMode is
     (
         BLEND_ALPHA -- Blend textures considering alpha (default)
       , BLEND_ADDITIVE -- Blend textures adding colors
       , BLEND_MULTIPLIED -- Blend textures multiplying colors
       , BLEND_ADD_COLORS -- Blend textures adding colors (alternative)
       , BLEND_SUBTRACT_COLORS -- Blend textures subtracting colors (alternative)
       , BLEND_ALPHA_PREMULTIPLY -- Blend premultiplied textures considering alpha
       , BLEND_CUSTOM -- Blend textures using custom src/dst factors (use rlSetBlendFactors())
       , BLEND_CUSTOM_SEPARATE -- Blend textures using custom rgb/alpha separate src/dst factors (use rlSetBlendFactorsSeparate())
     )
     with Convention => C;
   --  Color blending modes (pre-defined)

   for BlendMode use
     (
         BLEND_ALPHA => 0
       , BLEND_ADDITIVE => 1
       , BLEND_MULTIPLIED => 2
       , BLEND_ADD_COLORS => 3
       , BLEND_SUBTRACT_COLORS => 4
       , BLEND_ALPHA_PREMULTIPLY => 5
       , BLEND_CUSTOM => 6
       , BLEND_CUSTOM_SEPARATE => 7
     );

   type Gesture is new Interfaces.C.unsigned;
   --  Gesture

   GESTURE_NONE : constant Gesture := 0; -- No gesture
   GESTURE_TAP : constant Gesture := 1; -- Tap gesture
   GESTURE_DOUBLETAP : constant Gesture := 2; -- Double tap gesture
   GESTURE_HOLD : constant Gesture := 4; -- Hold gesture
   GESTURE_DRAG : constant Gesture := 8; -- Drag gesture
   GESTURE_SWIPE_RIGHT : constant Gesture := 16; -- Swipe right gesture
   GESTURE_SWIPE_LEFT : constant Gesture := 32; -- Swipe left gesture
   GESTURE_SWIPE_UP : constant Gesture := 64; -- Swipe up gesture
   GESTURE_SWIPE_DOWN : constant Gesture := 128; -- Swipe down gesture
   GESTURE_PINCH_IN : constant Gesture := 256; -- Pinch in gesture
   GESTURE_PINCH_OUT : constant Gesture := 512; -- Pinch out gesture

   type CameraMode is
     (
         CAMERA_CUSTOM -- Custom camera
       , CAMERA_FREE -- Free camera
       , CAMERA_ORBITAL -- Orbital camera
       , CAMERA_FIRST_PERSON -- First person camera
       , CAMERA_THIRD_PERSON -- Third person camera
     )
     with Convention => C;
   --  Camera system modes

   for CameraMode use
     (
         CAMERA_CUSTOM => 0
       , CAMERA_FREE => 1
       , CAMERA_ORBITAL => 2
       , CAMERA_FIRST_PERSON => 3
       , CAMERA_THIRD_PERSON => 4
     );

   type CameraProjection is
     (
         CAMERA_PERSPECTIVE -- Perspective projection
       , CAMERA_ORTHOGRAPHIC -- Orthographic projection
     )
     with Convention => C;
   --  Camera projection

   for CameraProjection use
     (
         CAMERA_PERSPECTIVE => 0
       , CAMERA_ORTHOGRAPHIC => 1
     );

   type NPatchLayout is
     (
         NPATCH_NINE_PATCH -- Npatch layout: 3x3 tiles
       , NPATCH_THREE_PATCH_VERTICAL -- Npatch layout: 1x3 tiles
       , NPATCH_THREE_PATCH_HORIZONTAL -- Npatch layout: 3x1 tiles
     )
     with Convention => C;
   --  N-patch layout

   for NPatchLayout use
     (
         NPATCH_NINE_PATCH => 0
       , NPATCH_THREE_PATCH_VERTICAL => 1
       , NPATCH_THREE_PATCH_HORIZONTAL => 2
     );

   type ShaderLocationArray is array (ShaderLocationIndex) of Interfaces.C.int
     with Convention => C;

   type LoadFileDataCallback is access    function  (fileName : Interfaces.C.Strings.chars_ptr; dataSize : access Interfaces.C.int) return access Interfaces.C.char

     with Convention => C;
   --  FileIO: Load binary data

   type SaveFileDataCallback is access    function  (fileName : Interfaces.C.Strings.chars_ptr; data : System.Address; dataSize : Interfaces.C.int) return Interfaces.C.C_bool

     with Convention => C;
   --  FileIO: Save binary data

   type LoadFileTextCallback is access    function  (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr

     with Convention => C;
   --  FileIO: Load text data

   type SaveFileTextCallback is access    function  (fileName : Interfaces.C.Strings.chars_ptr; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool

     with Convention => C;
   --  FileIO: Save text data

   type AudioCallback is access    procedure  (bufferData : System.Address; frames : Interfaces.C.unsigned)

     with Convention => C;

   type Vector2 is record
      x : Interfaces.C.C_float; -- Vector x component
      y : Interfaces.C.C_float; -- Vector y component
   end record
      with Convention => C_Pass_By_Copy;

   type Vector3 is record
      x : Interfaces.C.C_float; -- Vector x component
      y : Interfaces.C.C_float; -- Vector y component
      z : Interfaces.C.C_float; -- Vector z component
   end record
      with Convention => C_Pass_By_Copy;

   type Vector4 is record
      x : Interfaces.C.C_float; -- Vector x component
      y : Interfaces.C.C_float; -- Vector y component
      z : Interfaces.C.C_float; -- Vector z component
      w : Interfaces.C.C_float; -- Vector w component
   end record
      with Convention => C_Pass_By_Copy;
   subtype Quaternion is Vector4;

   type Matrix is record
      m0 : Interfaces.C.C_float; -- Matrix first row (4 components)
      m4 : Interfaces.C.C_float; -- Matrix first row (4 components)
      m8 : Interfaces.C.C_float; -- Matrix first row (4 components)
      m12 : Interfaces.C.C_float; -- Matrix first row (4 components)
      m1 : Interfaces.C.C_float; -- Matrix second row (4 components)
      m5 : Interfaces.C.C_float; -- Matrix second row (4 components)
      m9 : Interfaces.C.C_float; -- Matrix second row (4 components)
      m13 : Interfaces.C.C_float; -- Matrix second row (4 components)
      m2 : Interfaces.C.C_float; -- Matrix third row (4 components)
      m6 : Interfaces.C.C_float; -- Matrix third row (4 components)
      m10 : Interfaces.C.C_float; -- Matrix third row (4 components)
      m14 : Interfaces.C.C_float; -- Matrix third row (4 components)
      m3 : Interfaces.C.C_float; -- Matrix fourth row (4 components)
      m7 : Interfaces.C.C_float; -- Matrix fourth row (4 components)
      m11 : Interfaces.C.C_float; -- Matrix fourth row (4 components)
      m15 : Interfaces.C.C_float; -- Matrix fourth row (4 components)
   end record
      with Convention => C_Pass_By_Copy;
   type Matrix4 is array (0 .. 3) of Matrix;
   type Matrix2 is array (0 .. 1) of Matrix;

   type Color is record
      r : Interfaces.C.unsigned_char; -- Color red value
      g : Interfaces.C.unsigned_char; -- Color green value
      b : Interfaces.C.unsigned_char; -- Color blue value
      a : Interfaces.C.unsigned_char; -- Color alpha value
   end record
      with Convention => C_Pass_By_Copy;

   type Rectangle is record
      x : Interfaces.C.C_float; -- Rectangle top-left corner position x
      y : Interfaces.C.C_float; -- Rectangle top-left corner position y
      width : Interfaces.C.C_float; -- Rectangle width
      height : Interfaces.C.C_float; -- Rectangle height
   end record
      with Convention => C_Pass_By_Copy;

   type Image is record
      data : System.Address; -- Image raw data
      width : Interfaces.C.int; -- Image base width
      height : Interfaces.C.int; -- Image base height
      mipmaps : Interfaces.C.int; -- Mipmap levels, 1 by default
      format : PixelFormat; -- Data format (PixelFormat type)
   end record
      with Convention => C_Pass_By_Copy;

   type Texture is record
      id : Interfaces.C.unsigned; -- OpenGL texture id
      width : Interfaces.C.int; -- Texture base width
      height : Interfaces.C.int; -- Texture base height
      mipmaps : Interfaces.C.int; -- Mipmap levels, 1 by default
      format : Interfaces.C.int; -- Data format (PixelFormat type)
   end record
      with Convention => C_Pass_By_Copy;

   type RenderTexture is record
      id : Interfaces.C.unsigned; -- OpenGL framebuffer object id
      texture_f : Texture; -- Color buffer attachment texture
      depth : Texture; -- Depth buffer attachment texture
   end record
      with Convention => C_Pass_By_Copy;

   type NPatchInfo is record
      source : Rectangle; -- Texture source rectangle
      left : Interfaces.C.int; -- Left border offset
      top : Interfaces.C.int; -- Top border offset
      right : Interfaces.C.int; -- Right border offset
      bottom : Interfaces.C.int; -- Bottom border offset
      layout : NPatchLayout; -- Layout of the n-patch: 3x3, 1x3 or 3x1
   end record
      with Convention => C_Pass_By_Copy;

   type GlyphInfo is record
      value : Interfaces.C.int; -- Character value (Unicode)
      offsetX : Interfaces.C.int; -- Character offset X when drawing
      offsetY : Interfaces.C.int; -- Character offset Y when drawing
      advanceX : Interfaces.C.int; -- Character advance position X
      image_f : Image; -- Character image data
   end record
      with Convention => C_Pass_By_Copy;

   type Font is record
      baseSize : Interfaces.C.int; -- Base size (default chars height)
      glyphCount : Interfaces.C.int; -- Number of glyph characters
      glyphPadding : Interfaces.C.int; -- Padding around the glyph characters
      texture_f : Texture; -- Texture atlas containing the glyphs
      recs : access Rectangle; -- Rectangles in texture for the glyphs
      glyphs : access GlyphInfo; -- Glyphs info data
   end record
      with Convention => C_Pass_By_Copy;

   type Camera3D is record
      position : Vector3; -- Camera position
      target : Vector3; -- Camera target it looks-at
      up : Vector3; -- Camera up vector (rotation over its axis)
      fovy : Interfaces.C.C_float; -- Camera field-of-view aperture in Y (degrees) in perspective, used as near plane width in orthographic
      projection : CameraProjection; -- Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
   end record
      with Convention => C_Pass_By_Copy;

   type Camera2D is record
      offset : Vector2; -- Camera offset (displacement from target)
      target : Vector2; -- Camera target (rotation and zoom origin)
      rotation : Interfaces.C.C_float; -- Camera rotation in degrees
      zoom : Interfaces.C.C_float; -- Camera zoom (scaling), should be 1.0f by default
   end record
      with Convention => C_Pass_By_Copy;

   type Mesh is record
      vertexCount : Interfaces.C.int; -- Number of vertices stored in arrays
      triangleCount : Interfaces.C.int; -- Number of triangles stored (indexed or not)
      vertices : access Interfaces.C.C_float; -- Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      texcoords : access Interfaces.C.C_float; -- Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      texcoords2 : access Interfaces.C.C_float; -- Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
      normals : access Interfaces.C.C_float; -- Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
      tangents : access Interfaces.C.C_float; -- Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
      colors : access Interfaces.C.char; -- Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      indices : access Interfaces.C.short; -- Vertex indices (in case vertex data comes indexed)
      animVertices : access Interfaces.C.C_float; -- Animated vertex positions (after bones transformations)
      animNormals : access Interfaces.C.C_float; -- Animated normals (after bones transformations)
      boneIds : access Interfaces.C.char; -- Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning)
      boneWeights : access Interfaces.C.C_float; -- Vertex bone weight, up to 4 bones influence by vertex (skinning)
      vaoId : Interfaces.C.unsigned; -- OpenGL Vertex Array Object id
      vboId : access Interfaces.C.unsigned; -- OpenGL Vertex Buffer Objects id (default vertex data)
   end record
      with Convention => C_Pass_By_Copy;

   type Shader is record
      id : Interfaces.C.unsigned; -- Shader program id
      locs : access ShaderLocationArray; -- Shader locations array (RL_MAX_SHADER_LOCATIONS)
   end record
      with Convention => C_Pass_By_Copy;

   type MaterialMap is record
      texture_f : Texture; -- Material map texture
      color_f : Color; -- Material map color
      value : Interfaces.C.C_float; -- Material map value
   end record
      with Convention => C_Pass_By_Copy;
   type MaterialMapArray is array (MaterialMapIndex) of MaterialMap
     with Convention => C;

   type Material is record
      shader_f : Shader; -- Material shader
      maps : access MaterialMapArray; -- Material maps array (MAX_MATERIAL_MAPS)
      params : Float4; -- Material generic parameters (if required)
   end record
      with Convention => C_Pass_By_Copy;

   type Transform is record
      translation : Vector3; -- Translation
      rotation : Quaternion; -- Rotation
      scale : Vector3; -- Scale
   end record
      with Convention => C_Pass_By_Copy;
   type Tranform_Array is array (Interfaces.C.int) of Transform
     with Convention => C;

   type BoneInfo is record
      name : String32; -- Bone name
      parent : Interfaces.C.int; -- Bone parent
   end record
      with Convention => C_Pass_By_Copy;

   type Model is record
      transform_f : Matrix; -- Local transform matrix
      meshCount : Interfaces.C.int; -- Number of meshes
      materialCount : Interfaces.C.int; -- Number of materials
      meshes : access Mesh; -- Meshes array
      materials : access Material; -- Materials array
      meshMaterial : access Interfaces.C.int; -- Mesh material number
      boneCount : Interfaces.C.int; -- Number of bones
      bones : access BoneInfo; -- Bones information (skeleton)
      bindPose : access Transform; -- Bones base transformation (pose)
   end record
      with Convention => C_Pass_By_Copy;

   type ModelAnimation is record
      boneCount : Interfaces.C.int; -- Number of bones
      frameCount : Interfaces.C.int; -- Number of animation frames
      bones : access BoneInfo; -- Bones information (skeleton)
      framePoses : access Tranform_Array; -- Poses array by frame
      name : String32; -- Animation name
   end record
      with Convention => C_Pass_By_Copy;
   type ModelAnimation_Array is array (Interfaces.C.unsigned) of ModelAnimation
     with Convention => C;

   type Ray is record
      position : Vector3; -- Ray position (origin)
      direction : Vector3; -- Ray direction
   end record
      with Convention => C_Pass_By_Copy;

   type RayCollision is record
      hit : Interfaces.C.C_bool; -- Did the ray hit something?
      distance : Interfaces.C.C_float; -- Distance to the nearest hit
      point : Vector3; -- Point of the nearest hit
      normal : Vector3; -- Surface normal of hit
   end record
      with Convention => C_Pass_By_Copy;

   type BoundingBox is record
      min : Vector3; -- Minimum vertex box-corner
      max : Vector3; -- Maximum vertex box-corner
   end record
      with Convention => C_Pass_By_Copy;

   type Wave is record
      frameCount : Interfaces.C.unsigned; -- Total number of frames (considering channels)
      sampleRate : Interfaces.C.unsigned; -- Frequency (samples per second)
      sampleSize : Interfaces.C.unsigned; -- Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      channels : Interfaces.C.unsigned; -- Number of channels (1-mono, 2-stereo, ...)
      data : System.Address; -- Buffer data pointer
   end record
      with Convention => C_Pass_By_Copy;

   type AudioStream is record
      buffer : System.Address; -- Pointer to internal data used by the audio system
      processor : System.Address; -- Pointer to internal data processor, useful for audio effects
      sampleRate : Interfaces.C.unsigned; -- Frequency (samples per second)
      sampleSize : Interfaces.C.unsigned; -- Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      channels : Interfaces.C.unsigned; -- Number of channels (1-mono, 2-stereo, ...)
   end record
      with Convention => C_Pass_By_Copy;

   type Sound is record
      stream : AudioStream; -- Audio stream
      frameCount : Interfaces.C.unsigned; -- Total number of frames (considering channels)
   end record
      with Convention => C_Pass_By_Copy;

   type Music is record
      stream : AudioStream; -- Audio stream
      frameCount : Interfaces.C.unsigned; -- Total number of frames (considering channels)
      looping : Interfaces.C.C_bool; -- Music looping enable
      ctxType : Interfaces.C.int; -- Type of music context (audio filetype)
      ctxData : System.Address; -- Audio context data, depends on type
   end record
      with Convention => C_Pass_By_Copy;

   type VrDeviceInfo is record
      hResolution : Interfaces.C.int; -- Horizontal resolution in pixels
      vResolution : Interfaces.C.int; -- Vertical resolution in pixels
      hScreenSize : Interfaces.C.C_float; -- Horizontal size in meters
      vScreenSize : Interfaces.C.C_float; -- Vertical size in meters
      vScreenCenter : Interfaces.C.C_float; -- Screen center in meters
      eyeToScreenDistance : Interfaces.C.C_float; -- Distance between eye and display in meters
      lensSeparationDistance : Interfaces.C.C_float; -- Lens separation distance in meters
      interpupillaryDistance : Interfaces.C.C_float; -- IPD (distance between pupils) in meters
      lensDistortionValues : Float4; -- Lens distortion constant parameters
      chromaAbCorrection : Float4; -- Chromatic aberration correction parameters
   end record
      with Convention => C_Pass_By_Copy;

   type VrStereoConfig is record
      projection : Matrix2; -- VR projection matrices (per eye)
      viewOffset : Matrix2; -- VR view offset matrices (per eye)
      leftLensCenter : Float2; -- VR left lens center
      rightLensCenter : Float2; -- VR right lens center
      leftScreenCenter : Float2; -- VR left screen center
      rightScreenCenter : Float2; -- VR right screen center
      scale : Float2; -- VR distortion scale
      scaleIn : Float2; -- VR distortion scale in
   end record
      with Convention => C_Pass_By_Copy;

   type FilePathList is record
      capacity : Interfaces.C.unsigned; -- Filepaths max entries
      count : Interfaces.C.unsigned; -- Filepaths entries count
      paths : access constant C_String_Array; -- Filepaths entries
   end record
      with Convention => C_Pass_By_Copy;

   type AutomationEvent is record
      frame : Interfaces.C.unsigned; -- Event frame
      type_K : Interfaces.C.unsigned; -- Event type (AutomationEventType)
      params : Int4; -- Event parameters (if required)
   end record
      with Convention => C_Pass_By_Copy;
   type AutomationEvent_Array is array (Interfaces.C.unsigned) of AutomationEvent
     with Convention => C;

   type AutomationEventList is record
      capacity : Interfaces.C.unsigned; -- Events max entries (MAX_AUTOMATION_EVENTS)
      count : Interfaces.C.unsigned; -- Events entries count
      events : access AutomationEvent_Array; -- Events entries
   end record
      with Convention => C_Pass_By_Copy;

   procedure InitWindow (width : Interfaces.C.int; height : Interfaces.C.int; title : Interfaces.C.Strings.chars_ptr);
   --  Initialize window and OpenGL context
   pragma Import (C, InitWindow, "InitWindow");

   procedure InitWindow (width : Interfaces.C.int; height : Interfaces.C.int; title : String);
   --  Initialize window and OpenGL context

   procedure CloseWindow;
   --  Close window and unload OpenGL context
   pragma Import (C, CloseWindow, "CloseWindow");

   function WindowShouldClose return Interfaces.C.C_bool;
   --  Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
   pragma Import (C, WindowShouldClose, "WindowShouldClose");

   function IsWindowReady return Interfaces.C.C_bool;
   --  Check if window has been initialized successfully
   pragma Import (C, IsWindowReady, "IsWindowReady");

   function IsWindowFullscreen return Interfaces.C.C_bool;
   --  Check if window is currently fullscreen
   pragma Import (C, IsWindowFullscreen, "IsWindowFullscreen");

   function IsWindowHidden return Interfaces.C.C_bool;
   --  Check if window is currently hidden (only PLATFORM_DESKTOP)
   pragma Import (C, IsWindowHidden, "IsWindowHidden");

   function IsWindowMinimized return Interfaces.C.C_bool;
   --  Check if window is currently minimized (only PLATFORM_DESKTOP)
   pragma Import (C, IsWindowMinimized, "IsWindowMinimized");

   function IsWindowMaximized return Interfaces.C.C_bool;
   --  Check if window is currently maximized (only PLATFORM_DESKTOP)
   pragma Import (C, IsWindowMaximized, "IsWindowMaximized");

   function IsWindowFocused return Interfaces.C.C_bool;
   --  Check if window is currently focused (only PLATFORM_DESKTOP)
   pragma Import (C, IsWindowFocused, "IsWindowFocused");

   function IsWindowResized return Interfaces.C.C_bool;
   --  Check if window has been resized last frame
   pragma Import (C, IsWindowResized, "IsWindowResized");

   function IsWindowState (flag : Interfaces.C.unsigned) return Interfaces.C.C_bool;
   --  Check if one specific window flag is enabled
   pragma Import (C, IsWindowState, "IsWindowState");

   procedure SetWindowState (flags : Interfaces.C.unsigned);
   --  Set window configuration state using flags (only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowState, "SetWindowState");

   procedure ClearWindowState (flags : Interfaces.C.unsigned);
   --  Clear window configuration state flags
   pragma Import (C, ClearWindowState, "ClearWindowState");

   procedure ToggleFullscreen;
   --  Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
   pragma Import (C, ToggleFullscreen, "ToggleFullscreen");

   procedure ToggleBorderlessWindowed;
   --  Toggle window state: borderless windowed (only PLATFORM_DESKTOP)
   pragma Import (C, ToggleBorderlessWindowed, "ToggleBorderlessWindowed");

   procedure MaximizeWindow;
   --  Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
   pragma Import (C, MaximizeWindow, "MaximizeWindow");

   procedure MinimizeWindow;
   --  Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
   pragma Import (C, MinimizeWindow, "MinimizeWindow");

   procedure RestoreWindow;
   --  Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
   pragma Import (C, RestoreWindow, "RestoreWindow");

   procedure SetWindowIcon (image_p : Image);
   --  Set icon for window (single image, RGBA 32bit, only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowIcon, "SetWindowIcon");

   procedure SetWindowIcons (images : access Image; count : Interfaces.C.int);
   --  Set icon for window (multiple images, RGBA 32bit, only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowIcons, "SetWindowIcons");

   procedure SetWindowTitle (title : Interfaces.C.Strings.chars_ptr);
   --  Set title for window (only PLATFORM_DESKTOP and PLATFORM_WEB)
   pragma Import (C, SetWindowTitle, "SetWindowTitle");

   procedure SetWindowTitle (title : String);
   --  Set title for window (only PLATFORM_DESKTOP and PLATFORM_WEB)

   procedure SetWindowPosition (x : Interfaces.C.int; y : Interfaces.C.int);
   --  Set window position on screen (only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowPosition, "SetWindowPosition");

   procedure SetWindowMonitor (monitor : Interfaces.C.int);
   --  Set monitor for the current window
   pragma Import (C, SetWindowMonitor, "SetWindowMonitor");

   procedure SetWindowMinSize (width : Interfaces.C.int; height : Interfaces.C.int);
   --  Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
   pragma Import (C, SetWindowMinSize, "SetWindowMinSize");

   procedure SetWindowMaxSize (width : Interfaces.C.int; height : Interfaces.C.int);
   --  Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
   pragma Import (C, SetWindowMaxSize, "SetWindowMaxSize");

   procedure SetWindowSize (width : Interfaces.C.int; height : Interfaces.C.int);
   --  Set window dimensions
   pragma Import (C, SetWindowSize, "SetWindowSize");

   procedure SetWindowOpacity (opacity : Interfaces.C.C_float);
   --  Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowOpacity, "SetWindowOpacity");

   procedure SetWindowFocused;
   --  Set window focused (only PLATFORM_DESKTOP)
   pragma Import (C, SetWindowFocused, "SetWindowFocused");

   function GetWindowHandle return System.Address;
   --  Get native window handle
   pragma Import (C, GetWindowHandle, "GetWindowHandle");

   function GetScreenWidth return Interfaces.C.int;
   --  Get current screen width
   pragma Import (C, GetScreenWidth, "GetScreenWidth");

   function GetScreenHeight return Interfaces.C.int;
   --  Get current screen height
   pragma Import (C, GetScreenHeight, "GetScreenHeight");

   function GetRenderWidth return Interfaces.C.int;
   --  Get current render width (it considers HiDPI)
   pragma Import (C, GetRenderWidth, "GetRenderWidth");

   function GetRenderHeight return Interfaces.C.int;
   --  Get current render height (it considers HiDPI)
   pragma Import (C, GetRenderHeight, "GetRenderHeight");

   function GetMonitorCount return Interfaces.C.int;
   --  Get number of connected monitors
   pragma Import (C, GetMonitorCount, "GetMonitorCount");

   function GetCurrentMonitor return Interfaces.C.int;
   --  Get current connected monitor
   pragma Import (C, GetCurrentMonitor, "GetCurrentMonitor");

   function GetMonitorPosition (monitor : Interfaces.C.int) return Vector2;
   --  Get specified monitor position
   pragma Import (C, GetMonitorPosition, "GetMonitorPosition");

   function GetMonitorWidth (monitor : Interfaces.C.int) return Interfaces.C.int;
   --  Get specified monitor width (current video mode used by monitor)
   pragma Import (C, GetMonitorWidth, "GetMonitorWidth");

   function GetMonitorHeight (monitor : Interfaces.C.int) return Interfaces.C.int;
   --  Get specified monitor height (current video mode used by monitor)
   pragma Import (C, GetMonitorHeight, "GetMonitorHeight");

   function GetMonitorPhysicalWidth (monitor : Interfaces.C.int) return Interfaces.C.int;
   --  Get specified monitor physical width in millimetres
   pragma Import (C, GetMonitorPhysicalWidth, "GetMonitorPhysicalWidth");

   function GetMonitorPhysicalHeight (monitor : Interfaces.C.int) return Interfaces.C.int;
   --  Get specified monitor physical height in millimetres
   pragma Import (C, GetMonitorPhysicalHeight, "GetMonitorPhysicalHeight");

   function GetMonitorRefreshRate (monitor : Interfaces.C.int) return Interfaces.C.int;
   --  Get specified monitor refresh rate
   pragma Import (C, GetMonitorRefreshRate, "GetMonitorRefreshRate");

   function GetWindowPosition return Vector2;
   --  Get window position XY on monitor
   pragma Import (C, GetWindowPosition, "GetWindowPosition");

   function GetWindowScaleDPI return Vector2;
   --  Get window scale DPI factor
   pragma Import (C, GetWindowScaleDPI, "GetWindowScaleDPI");

   function GetMonitorName (monitor : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Get the human-readable, UTF-8 encoded name of the specified monitor
   pragma Import (C, GetMonitorName, "GetMonitorName");

   procedure SetClipboardText (text : Interfaces.C.Strings.chars_ptr);
   --  Set clipboard text content
   pragma Import (C, SetClipboardText, "SetClipboardText");

   procedure SetClipboardText (text : String);
   --  Set clipboard text content

   function GetClipboardText return Interfaces.C.Strings.chars_ptr;
   --  Get clipboard text content
   pragma Import (C, GetClipboardText, "GetClipboardText");

   function GetClipboardText return String;
   --  Get clipboard text content

   procedure EnableEventWaiting;
   --  Enable waiting for events on EndDrawing(), no automatic event polling
   pragma Import (C, EnableEventWaiting, "EnableEventWaiting");

   procedure DisableEventWaiting;
   --  Disable waiting for events on EndDrawing(), automatic events polling
   pragma Import (C, DisableEventWaiting, "DisableEventWaiting");

   procedure ShowCursor;
   --  Shows cursor
   pragma Import (C, ShowCursor, "ShowCursor");

   procedure HideCursor;
   --  Hides cursor
   pragma Import (C, HideCursor, "HideCursor");

   function IsCursorHidden return Interfaces.C.C_bool;
   --  Check if cursor is not visible
   pragma Import (C, IsCursorHidden, "IsCursorHidden");

   procedure EnableCursor;
   --  Enables cursor (unlock cursor)
   pragma Import (C, EnableCursor, "EnableCursor");

   procedure DisableCursor;
   --  Disables cursor (lock cursor)
   pragma Import (C, DisableCursor, "DisableCursor");

   function IsCursorOnScreen return Interfaces.C.C_bool;
   --  Check if cursor is on the screen
   pragma Import (C, IsCursorOnScreen, "IsCursorOnScreen");

   procedure ClearBackground (color_p : Color);
   --  Set background color (framebuffer clear color)
   pragma Import (C, ClearBackground, "ClearBackground");

   procedure BeginDrawing;
   --  Setup canvas (framebuffer) to start drawing
   pragma Import (C, BeginDrawing, "BeginDrawing");

   procedure EndDrawing;
   --  End canvas drawing and swap buffers (double buffering)
   pragma Import (C, EndDrawing, "EndDrawing");

   procedure BeginMode2D (camera_p : Camera2D);
   --  Begin 2D mode with custom camera (2D)
   pragma Import (C, BeginMode2D, "BeginMode2D");

   procedure EndMode2D;
   --  Ends 2D mode with custom camera
   pragma Import (C, EndMode2D, "EndMode2D");

   procedure BeginMode3D (camera_p : Camera3D);
   --  Begin 3D mode with custom camera (3D)
   pragma Import (C, BeginMode3D, "BeginMode3D");

   procedure EndMode3D;
   --  Ends 3D mode and returns to default 2D orthographic mode
   pragma Import (C, EndMode3D, "EndMode3D");

   procedure BeginTextureMode (target : RenderTexture);
   --  Begin drawing to render texture
   pragma Import (C, BeginTextureMode, "BeginTextureMode");

   procedure EndTextureMode;
   --  Ends drawing to render texture
   pragma Import (C, EndTextureMode, "EndTextureMode");

   procedure BeginShaderMode (shader_p : Shader);
   --  Begin custom shader drawing
   pragma Import (C, BeginShaderMode, "BeginShaderMode");

   procedure EndShaderMode;
   --  End custom shader drawing (use default shader)
   pragma Import (C, EndShaderMode, "EndShaderMode");

   procedure BeginBlendMode (mode : BlendMode);
   --  Begin blending mode (alpha, additive, multiplied, subtract, custom)
   pragma Import (C, BeginBlendMode, "BeginBlendMode");

   procedure EndBlendMode;
   --  End blending mode (reset to default: alpha blending)
   pragma Import (C, EndBlendMode, "EndBlendMode");

   procedure BeginScissorMode (x : Interfaces.C.int; y : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int);
   --  Begin scissor mode (define screen area for following drawing)
   pragma Import (C, BeginScissorMode, "BeginScissorMode");

   procedure EndScissorMode;
   --  End scissor mode
   pragma Import (C, EndScissorMode, "EndScissorMode");

   procedure BeginVrStereoMode (config : VrStereoConfig);
   --  Begin stereo rendering (requires VR simulator)
   pragma Import (C, BeginVrStereoMode, "BeginVrStereoMode");

   procedure EndVrStereoMode;
   --  End stereo rendering (requires VR simulator)
   pragma Import (C, EndVrStereoMode, "EndVrStereoMode");

   function LoadVrStereoConfig (device : VrDeviceInfo) return VrStereoConfig;
   --  Load VR stereo config for VR simulator device parameters
   pragma Import (C, LoadVrStereoConfig, "LoadVrStereoConfig");

   procedure UnloadVrStereoConfig (config : VrStereoConfig);
   --  Unload VR stereo config
   pragma Import (C, UnloadVrStereoConfig, "UnloadVrStereoConfig");

   function LoadShader (vsFileName : Interfaces.C.Strings.chars_ptr; fsFileName : Interfaces.C.Strings.chars_ptr) return Shader;
   --  Load shader from files and bind default locations
   pragma Import (C, LoadShader, "LoadShader");

   function LoadShader (vsFileName : String; fsFileName : String) return Shader;
   --  Load shader from files and bind default locations

   function LoadShaderFromMemory (vsCode : Interfaces.C.Strings.chars_ptr; fsCode : Interfaces.C.Strings.chars_ptr) return Shader;
   --  Load shader from code strings and bind default locations
   pragma Import (C, LoadShaderFromMemory, "LoadShaderFromMemory");

   function LoadShaderFromMemory (vsCode : String; fsCode : String) return Shader;
   --  Load shader from code strings and bind default locations

   function IsShaderReady (shader_p : Shader) return Interfaces.C.C_bool;
   --  Check if a shader is ready
   pragma Import (C, IsShaderReady, "IsShaderReady");

   function GetShaderLocation (shader_p : Shader; uniformName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get shader uniform location
   pragma Import (C, GetShaderLocation, "GetShaderLocation");

   function GetShaderLocation (shader_p : Shader; uniformName : String) return Interfaces.C.int;
   --  Get shader uniform location

   function GetShaderLocationAttrib (shader_p : Shader; attribName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get shader attribute location
   pragma Import (C, GetShaderLocationAttrib, "GetShaderLocationAttrib");

   function GetShaderLocationAttrib (shader_p : Shader; attribName : String) return Interfaces.C.int;
   --  Get shader attribute location

   procedure SetShaderValue (shader_p : Shader; locIndex : Interfaces.C.int; value : System.Address; uniformType : ShaderUniformDataType);
   --  Set shader uniform value
   pragma Import (C, SetShaderValue, "SetShaderValue");

   procedure SetShaderValueV (shader_p : Shader; locIndex : Interfaces.C.int; value : System.Address; uniformType : ShaderUniformDataType; count : Interfaces.C.int);
   --  Set shader uniform value vector
   pragma Import (C, SetShaderValueV, "SetShaderValueV");

   procedure SetShaderValueMatrix (shader_p : Shader; locIndex : Interfaces.C.int; mat : Matrix);
   --  Set shader uniform value (matrix 4x4)
   pragma Import (C, SetShaderValueMatrix, "SetShaderValueMatrix");

   procedure SetShaderValueTexture (shader_p : Shader; locIndex : Interfaces.C.int; texture_p : Texture);
   --  Set shader uniform value for texture (sampler2d)
   pragma Import (C, SetShaderValueTexture, "SetShaderValueTexture");

   procedure UnloadShader (shader_p : Shader);
   --  Unload shader from GPU memory (VRAM)
   pragma Import (C, UnloadShader, "UnloadShader");

   function GetMouseRay (mousePosition : Vector2; camera_p : Camera3D) return Ray;
   --  Get a ray trace from mouse position
   pragma Import (C, GetMouseRay, "GetScreenToWorldRay");

   function GetScreenToWorldRay (mousePosition : Vector2; camera_p : Camera3D) return Ray;
   --  Get a ray trace from mouse position
   pragma Import (C, GetScreenToWorldRay, "GetScreenToWorldRay");

   function GetCameraMatrix (camera_p : Camera3D) return Matrix;
   --  Get camera transform matrix (view matrix)
   pragma Import (C, GetCameraMatrix, "GetCameraMatrix");

   function GetCameraMatrix2D (camera_p : Camera2D) return Matrix;
   --  Get camera 2d transform matrix
   pragma Import (C, GetCameraMatrix2D, "GetCameraMatrix2D");

   function GetWorldToScreen (position : Vector3; camera_p : Camera3D) return Vector2;
   --  Get the screen space position for a 3d world space position
   pragma Import (C, GetWorldToScreen, "GetWorldToScreen");

   function GetScreenToWorld2D (position : Vector2; camera_p : Camera2D) return Vector2;
   --  Get the world space position for a 2d camera screen space position
   pragma Import (C, GetScreenToWorld2D, "GetScreenToWorld2D");

   function GetWorldToScreenEx (position : Vector3; camera_p : Camera3D; width : Interfaces.C.int; height : Interfaces.C.int) return Vector2;
   --  Get size position for a 3d world space position
   pragma Import (C, GetWorldToScreenEx, "GetWorldToScreenEx");

   function GetWorldToScreen2D (position : Vector2; camera_p : Camera2D) return Vector2;
   --  Get the screen space position for a 2d camera world space position
   pragma Import (C, GetWorldToScreen2D, "GetWorldToScreen2D");

   procedure SetTargetFPS (fps : Interfaces.C.int);
   --  Set target FPS (maximum)
   pragma Import (C, SetTargetFPS, "SetTargetFPS");

   function GetFrameTime return Interfaces.C.C_float;
   --  Get time in seconds for last frame drawn (delta time)
   pragma Import (C, GetFrameTime, "GetFrameTime");

   function GetTime return Interfaces.C.double;
   --  Get elapsed time in seconds since InitWindow()
   pragma Import (C, GetTime, "GetTime");

   function GetFPS return Interfaces.C.int;
   --  Get current FPS
   pragma Import (C, GetFPS, "GetFPS");

   procedure SwapScreenBuffer;
   --  Swap back buffer with front buffer (screen drawing)
   pragma Import (C, SwapScreenBuffer, "SwapScreenBuffer");

   procedure PollInputEvents;
   --  Register all input events
   pragma Import (C, PollInputEvents, "PollInputEvents");

   procedure WaitTime (seconds : Interfaces.C.double);
   --  Wait for some time (halt program execution)
   pragma Import (C, WaitTime, "WaitTime");

   procedure SetRandomSeed (seed : Interfaces.C.unsigned);
   --  Set the seed for the random number generator
   pragma Import (C, SetRandomSeed, "SetRandomSeed");

   function GetRandomValue (min : Interfaces.C.int; max : Interfaces.C.int) return Interfaces.C.int;
   --  Get a random value between min and max (both included)
   pragma Import (C, GetRandomValue, "GetRandomValue");

   function LoadRandomSequence (count : Interfaces.C.unsigned; min : Interfaces.C.int; max : Interfaces.C.int) return access Interfaces.C.int;
   --  Load random values sequence, no values repeated
   pragma Import (C, LoadRandomSequence, "LoadRandomSequence");

   procedure UnloadRandomSequence (sequence : access Interfaces.C.int);
   --  Unload random values sequence
   pragma Import (C, UnloadRandomSequence, "UnloadRandomSequence");

   procedure TakeScreenshot (fileName : Interfaces.C.Strings.chars_ptr);
   --  Takes a screenshot of current screen (filename extension defines format)
   pragma Import (C, TakeScreenshot, "TakeScreenshot");

   procedure TakeScreenshot (fileName : String);
   --  Takes a screenshot of current screen (filename extension defines format)

   procedure SetConfigFlags (flags : Interfaces.C.unsigned);
   --  Setup init configuration flags (view FLAGS)
   pragma Import (C, SetConfigFlags, "SetConfigFlags");

   procedure OpenURL (url : Interfaces.C.Strings.chars_ptr);
   --  Open URL with default system browser (if available)
   pragma Import (C, OpenURL, "OpenURL");

   procedure OpenURL (url : String);
   --  Open URL with default system browser (if available)

   function MemAlloc (size : Interfaces.C.unsigned) return System.Address;
   --  Internal memory allocator
   pragma Import (C, MemAlloc, "MemAlloc");

   function MemRealloc (ptr : System.Address; size : Interfaces.C.unsigned) return System.Address;
   --  Internal memory reallocator
   pragma Import (C, MemRealloc, "MemRealloc");

   procedure MemFree (ptr : System.Address);
   --  Internal memory free
   pragma Import (C, MemFree, "MemFree");

   procedure SetLoadFileDataCallback (callback : LoadFileDataCallback);
   --  Set custom file binary data loader
   pragma Import (C, SetLoadFileDataCallback, "SetLoadFileDataCallback");

   procedure SetSaveFileDataCallback (callback : SaveFileDataCallback);
   --  Set custom file binary data saver
   pragma Import (C, SetSaveFileDataCallback, "SetSaveFileDataCallback");

   procedure SetLoadFileTextCallback (callback : LoadFileTextCallback);
   --  Set custom file text data loader
   pragma Import (C, SetLoadFileTextCallback, "SetLoadFileTextCallback");

   procedure SetSaveFileTextCallback (callback : SaveFileTextCallback);
   --  Set custom file text data saver
   pragma Import (C, SetSaveFileTextCallback, "SetSaveFileTextCallback");

   function LoadFileData (fileName : Interfaces.C.Strings.chars_ptr; dataSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Load file data as byte array (read)
   pragma Import (C, LoadFileData, "LoadFileData");

   function LoadFileData (fileName : String; dataSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Load file data as byte array (read)

   procedure UnloadFileData (data : access Interfaces.C.char);
   --  Unload file data allocated by LoadFileData()
   pragma Import (C, UnloadFileData, "UnloadFileData");

   function SaveFileData (fileName : Interfaces.C.Strings.chars_ptr; data : System.Address; dataSize : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Save data to file from byte array (write), returns true on success
   pragma Import (C, SaveFileData, "SaveFileData");

   function SaveFileData (fileName : String; data : System.Address; dataSize : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Save data to file from byte array (write), returns true on success

   function ExportDataAsCode (data : System.Address; dataSize : Interfaces.C.int; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export data to code (.h), returns true on success
   pragma Import (C, ExportDataAsCode, "ExportDataAsCode");

   function ExportDataAsCode (data : System.Address; dataSize : Interfaces.C.int; fileName : String) return Interfaces.C.C_bool;
   --  Export data to code (.h), returns true on success

   function LoadFileText (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Load text data from file (read), returns a '\0' terminated string
   pragma Import (C, LoadFileText, "LoadFileText");

   function LoadFileText (fileName : String) return String;
   --  Load text data from file (read), returns a '\0' terminated string

   procedure UnloadFileText (text : Interfaces.C.Strings.chars_ptr);
   --  Unload file text data allocated by LoadFileText()
   pragma Import (C, UnloadFileText, "UnloadFileText");

   procedure UnloadFileText (text : String);
   --  Unload file text data allocated by LoadFileText()

   function SaveFileText (fileName : Interfaces.C.Strings.chars_ptr; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Save text data to file (write), string must be '\0' terminated, returns true on success
   pragma Import (C, SaveFileText, "SaveFileText");

   function SaveFileText (fileName : String; text : String) return Interfaces.C.C_bool;
   --  Save text data to file (write), string must be '\0' terminated, returns true on success

   function FileExists (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if file exists
   pragma Import (C, FileExists, "FileExists");

   function FileExists (fileName : String) return Interfaces.C.C_bool;
   --  Check if file exists

   function DirectoryExists (dirPath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if a directory path exists
   pragma Import (C, DirectoryExists, "DirectoryExists");

   function DirectoryExists (dirPath : String) return Interfaces.C.C_bool;
   --  Check if a directory path exists

   function IsFileExtension (fileName : Interfaces.C.Strings.chars_ptr; ext : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check file extension (including point: .png, .wav)
   pragma Import (C, IsFileExtension, "IsFileExtension");

   function IsFileExtension (fileName : String; ext : String) return Interfaces.C.C_bool;
   --  Check file extension (including point: .png, .wav)

   function GetFileLength (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
   pragma Import (C, GetFileLength, "GetFileLength");

   function GetFileLength (fileName : String) return Interfaces.C.int;
   --  Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)

   function GetFileExtension (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get pointer to extension for a filename string (includes dot: '.png')
   pragma Import (C, GetFileExtension, "GetFileExtension");

   function GetFileExtension (fileName : String) return String;
   --  Get pointer to extension for a filename string (includes dot: '.png')

   function GetFileName (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get pointer to filename for a path string
   pragma Import (C, GetFileName, "GetFileName");

   function GetFileName (filePath : String) return String;
   --  Get pointer to filename for a path string

   function GetFileNameWithoutExt (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get filename string without extension (uses static string)
   pragma Import (C, GetFileNameWithoutExt, "GetFileNameWithoutExt");

   function GetFileNameWithoutExt (filePath : String) return String;
   --  Get filename string without extension (uses static string)

   function GetDirectoryPath (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get full path for a given fileName with path (uses static string)
   pragma Import (C, GetDirectoryPath, "GetDirectoryPath");

   function GetDirectoryPath (filePath : String) return String;
   --  Get full path for a given fileName with path (uses static string)

   function GetPrevDirectoryPath (dirPath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get previous directory path for a given path (uses static string)
   pragma Import (C, GetPrevDirectoryPath, "GetPrevDirectoryPath");

   function GetPrevDirectoryPath (dirPath : String) return String;
   --  Get previous directory path for a given path (uses static string)

   function GetWorkingDirectory return Interfaces.C.Strings.chars_ptr;
   --  Get current working directory (uses static string)
   pragma Import (C, GetWorkingDirectory, "GetWorkingDirectory");

   function GetWorkingDirectory return String;
   --  Get current working directory (uses static string)

   function GetApplicationDirectory return Interfaces.C.Strings.chars_ptr;
   --  Get the directory of the running application (uses static string)
   pragma Import (C, GetApplicationDirectory, "GetApplicationDirectory");

   function GetApplicationDirectory return String;
   --  Get the directory of the running application (uses static string)

   function ChangeDirectory (dir : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Change working directory, return true on success
   pragma Import (C, ChangeDirectory, "ChangeDirectory");

   function ChangeDirectory (dir : String) return Interfaces.C.C_bool;
   --  Change working directory, return true on success

   function IsPathFile (path : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if a given path is a file or a directory
   pragma Import (C, IsPathFile, "IsPathFile");

   function IsPathFile (path : String) return Interfaces.C.C_bool;
   --  Check if a given path is a file or a directory

   function LoadDirectoryFiles (dirPath : Interfaces.C.Strings.chars_ptr) return FilePathList;
   --  Load directory filepaths
   pragma Import (C, LoadDirectoryFiles, "LoadDirectoryFiles");

   function LoadDirectoryFiles (dirPath : String) return FilePathList;
   --  Load directory filepaths

   function LoadDirectoryFilesEx (basePath : Interfaces.C.Strings.chars_ptr; filter : Interfaces.C.Strings.chars_ptr; scanSubdirs : Interfaces.C.C_bool) return FilePathList;
   --  Load directory filepaths with extension filtering and recursive directory scan
   pragma Import (C, LoadDirectoryFilesEx, "LoadDirectoryFilesEx");

   function LoadDirectoryFilesEx (basePath : String; filter : String; scanSubdirs : Interfaces.C.C_bool) return FilePathList;
   --  Load directory filepaths with extension filtering and recursive directory scan

   procedure UnloadDirectoryFiles (files : FilePathList);
   --  Unload filepaths
   pragma Import (C, UnloadDirectoryFiles, "UnloadDirectoryFiles");

   function IsFileDropped return Interfaces.C.C_bool;
   --  Check if a file has been dropped into window
   pragma Import (C, IsFileDropped, "IsFileDropped");

   function LoadDroppedFiles return FilePathList;
   --  Load dropped filepaths
   pragma Import (C, LoadDroppedFiles, "LoadDroppedFiles");

   procedure UnloadDroppedFiles (files : FilePathList);
   --  Unload dropped filepaths
   pragma Import (C, UnloadDroppedFiles, "UnloadDroppedFiles");

   function GetFileModTime (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.long;
   --  Get file modification time (last write time)
   pragma Import (C, GetFileModTime, "GetFileModTime");

   function GetFileModTime (fileName : String) return Interfaces.C.long;
   --  Get file modification time (last write time)

   function CompressData (data : System.Address; dataSize : Interfaces.C.int; compDataSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Compress data (DEFLATE algorithm), memory must be MemFree()
   pragma Import (C, CompressData, "CompressData");

   function DecompressData (compData : System.Address; compDataSize : Interfaces.C.int; dataSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Decompress data (DEFLATE algorithm), memory must be MemFree()
   pragma Import (C, DecompressData, "DecompressData");

   function EncodeDataBase64 (data : System.Address; dataSize : Interfaces.C.int; outputSize : access Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Encode data to Base64 string, memory must be MemFree()
   pragma Import (C, EncodeDataBase64, "EncodeDataBase64");

   function DecodeDataBase64 (data : System.Address; outputSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Decode Base64 string data, memory must be MemFree()
   pragma Import (C, DecodeDataBase64, "DecodeDataBase64");

   function LoadAutomationEventList (fileName : Interfaces.C.Strings.chars_ptr) return AutomationEventList;
   --  Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
   pragma Import (C, LoadAutomationEventList, "LoadAutomationEventList");

   function LoadAutomationEventList (fileName : String) return AutomationEventList;
   --  Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS

   procedure UnloadAutomationEventList (list : access AutomationEventList);
   --  Unload automation events list from file
   pragma Import (C, UnloadAutomationEventList, "UnloadAutomationEventList");

   function ExportAutomationEventList (list : AutomationEventList; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export automation events list as text file
   pragma Import (C, ExportAutomationEventList, "ExportAutomationEventList");

   function ExportAutomationEventList (list : AutomationEventList; fileName : String) return Interfaces.C.C_bool;
   --  Export automation events list as text file

   procedure SetAutomationEventList (list : access AutomationEventList);
   --  Set automation event list to record to
   pragma Import (C, SetAutomationEventList, "SetAutomationEventList");

   procedure SetAutomationEventBaseFrame (frame : Interfaces.C.int);
   --  Set automation event internal base frame to start recording
   pragma Import (C, SetAutomationEventBaseFrame, "SetAutomationEventBaseFrame");

   procedure StartAutomationEventRecording;
   --  Start recording automation events (AutomationEventList must be set)
   pragma Import (C, StartAutomationEventRecording, "StartAutomationEventRecording");

   procedure StopAutomationEventRecording;
   --  Stop recording automation events
   pragma Import (C, StopAutomationEventRecording, "StopAutomationEventRecording");

   procedure PlayAutomationEvent (event : AutomationEvent);
   --  Play a recorded automation event
   pragma Import (C, PlayAutomationEvent, "PlayAutomationEvent");

   function IsKeyPressed (key : KeyboardKey) return Interfaces.C.C_bool;
   --  Check if a key has been pressed once
   pragma Import (C, IsKeyPressed, "IsKeyPressed");

   function IsKeyPressedRepeat (key : KeyboardKey) return Interfaces.C.C_bool;
   --  Check if a key has been pressed again (Only PLATFORM_DESKTOP)
   pragma Import (C, IsKeyPressedRepeat, "IsKeyPressedRepeat");

   function IsKeyDown (key : KeyboardKey) return Interfaces.C.C_bool;
   --  Check if a key is being pressed
   pragma Import (C, IsKeyDown, "IsKeyDown");

   function IsKeyReleased (key : KeyboardKey) return Interfaces.C.C_bool;
   --  Check if a key has been released once
   pragma Import (C, IsKeyReleased, "IsKeyReleased");

   function IsKeyUp (key : KeyboardKey) return Interfaces.C.C_bool;
   --  Check if a key is NOT being pressed
   pragma Import (C, IsKeyUp, "IsKeyUp");

   function GetKeyPressed return Interfaces.C.int;
   --  Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
   pragma Import (C, GetKeyPressed, "GetKeyPressed");

   function GetCharPressed return Interfaces.C.int;
   --  Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
   pragma Import (C, GetCharPressed, "GetCharPressed");

   procedure SetExitKey (key : KeyboardKey);
   --  Set a custom key to exit program (default is ESC)
   pragma Import (C, SetExitKey, "SetExitKey");

   function IsGamepadAvailable (gamepad : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad is available
   pragma Import (C, IsGamepadAvailable, "IsGamepadAvailable");

   function GetGamepadName (gamepad : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Get gamepad internal name id
   pragma Import (C, GetGamepadName, "GetGamepadName");

   function IsGamepadButtonPressed (gamepad : Interfaces.C.int; button : GamepadButton) return Interfaces.C.C_bool;
   --  Check if a gamepad button has been pressed once
   pragma Import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");

   function IsGamepadButtonDown (gamepad : Interfaces.C.int; button : GamepadButton) return Interfaces.C.C_bool;
   --  Check if a gamepad button is being pressed
   pragma Import (C, IsGamepadButtonDown, "IsGamepadButtonDown");

   function IsGamepadButtonReleased (gamepad : Interfaces.C.int; button : GamepadButton) return Interfaces.C.C_bool;
   --  Check if a gamepad button has been released once
   pragma Import (C, IsGamepadButtonReleased, "IsGamepadButtonReleased");

   function IsGamepadButtonUp (gamepad : Interfaces.C.int; button : GamepadButton) return Interfaces.C.C_bool;
   --  Check if a gamepad button is NOT being pressed
   pragma Import (C, IsGamepadButtonUp, "IsGamepadButtonUp");

   function GetGamepadButtonPressed return Interfaces.C.int;
   --  Get the last gamepad button pressed
   pragma Import (C, GetGamepadButtonPressed, "GetGamepadButtonPressed");

   function GetGamepadAxisCount (gamepad : Interfaces.C.int) return Interfaces.C.int;
   --  Get gamepad axis count for a gamepad
   pragma Import (C, GetGamepadAxisCount, "GetGamepadAxisCount");

   function GetGamepadAxisMovement (gamepad : Interfaces.C.int; axis : GamepadAxis) return Interfaces.C.C_float;
   --  Get axis movement value for a gamepad axis
   pragma Import (C, GetGamepadAxisMovement, "GetGamepadAxisMovement");

   function SetGamepadMappings (mappings : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Set internal gamepad mappings (SDL_GameControllerDB)
   pragma Import (C, SetGamepadMappings, "SetGamepadMappings");

   function SetGamepadMappings (mappings : String) return Interfaces.C.int;
   --  Set internal gamepad mappings (SDL_GameControllerDB)

   function IsMouseButtonPressed (button : MouseButton) return Interfaces.C.C_bool;
   --  Check if a mouse button has been pressed once
   pragma Import (C, IsMouseButtonPressed, "IsMouseButtonPressed");

   function IsMouseButtonDown (button : MouseButton) return Interfaces.C.C_bool;
   --  Check if a mouse button is being pressed
   pragma Import (C, IsMouseButtonDown, "IsMouseButtonDown");

   function IsMouseButtonReleased (button : MouseButton) return Interfaces.C.C_bool;
   --  Check if a mouse button has been released once
   pragma Import (C, IsMouseButtonReleased, "IsMouseButtonReleased");

   function IsMouseButtonUp (button : MouseButton) return Interfaces.C.C_bool;
   --  Check if a mouse button is NOT being pressed
   pragma Import (C, IsMouseButtonUp, "IsMouseButtonUp");

   function GetMouseX return Interfaces.C.int;
   --  Get mouse position X
   pragma Import (C, GetMouseX, "GetMouseX");

   function GetMouseY return Interfaces.C.int;
   --  Get mouse position Y
   pragma Import (C, GetMouseY, "GetMouseY");

   function GetMousePosition return Vector2;
   --  Get mouse position XY
   pragma Import (C, GetMousePosition, "GetMousePosition");

   function GetMouseDelta return Vector2;
   --  Get mouse delta between frames
   pragma Import (C, GetMouseDelta, "GetMouseDelta");

   procedure SetMousePosition (x : Interfaces.C.int; y : Interfaces.C.int);
   --  Set mouse position XY
   pragma Import (C, SetMousePosition, "SetMousePosition");

   procedure SetMouseOffset (offsetX : Interfaces.C.int; offsetY : Interfaces.C.int);
   --  Set mouse offset
   pragma Import (C, SetMouseOffset, "SetMouseOffset");

   procedure SetMouseScale (scaleX : Interfaces.C.C_float; scaleY : Interfaces.C.C_float);
   --  Set mouse scaling
   pragma Import (C, SetMouseScale, "SetMouseScale");

   function GetMouseWheelMove return Interfaces.C.C_float;
   --  Get mouse wheel movement for X or Y, whichever is larger
   pragma Import (C, GetMouseWheelMove, "GetMouseWheelMove");

   function GetMouseWheelMoveV return Vector2;
   --  Get mouse wheel movement for both X and Y
   pragma Import (C, GetMouseWheelMoveV, "GetMouseWheelMoveV");

   procedure SetMouseCursor (cursor : Interfaces.C.int);
   --  Set mouse cursor
   pragma Import (C, SetMouseCursor, "SetMouseCursor");

   function GetTouchX return Interfaces.C.int;
   --  Get touch position X for touch point 0 (relative to screen size)
   pragma Import (C, GetTouchX, "GetTouchX");

   function GetTouchY return Interfaces.C.int;
   --  Get touch position Y for touch point 0 (relative to screen size)
   pragma Import (C, GetTouchY, "GetTouchY");

   function GetTouchPosition (index : Interfaces.C.int) return Vector2;
   --  Get touch position XY for a touch point index (relative to screen size)
   pragma Import (C, GetTouchPosition, "GetTouchPosition");

   function GetTouchPointId (index : Interfaces.C.int) return Interfaces.C.int;
   --  Get touch point identifier for given index
   pragma Import (C, GetTouchPointId, "GetTouchPointId");

   function GetTouchPointCount return Interfaces.C.int;
   --  Get number of touch points
   pragma Import (C, GetTouchPointCount, "GetTouchPointCount");

   procedure SetGesturesEnabled (flags : Interfaces.C.unsigned);
   --  Enable a set of gestures using flags
   pragma Import (C, SetGesturesEnabled, "SetGesturesEnabled");

   function IsGestureDetected (gesture : Interfaces.C.unsigned) return Interfaces.C.C_bool;
   --  Check if a gesture have been detected
   pragma Import (C, IsGestureDetected, "IsGestureDetected");

   function GetGestureDetected return Interfaces.C.int;
   --  Get latest detected gesture
   pragma Import (C, GetGestureDetected, "GetGestureDetected");

   function GetGestureHoldDuration return Interfaces.C.C_float;
   --  Get gesture hold time in milliseconds
   pragma Import (C, GetGestureHoldDuration, "GetGestureHoldDuration");

   function GetGestureDragVector return Vector2;
   --  Get gesture drag vector
   pragma Import (C, GetGestureDragVector, "GetGestureDragVector");

   function GetGestureDragAngle return Interfaces.C.C_float;
   --  Get gesture drag angle
   pragma Import (C, GetGestureDragAngle, "GetGestureDragAngle");

   function GetGesturePinchVector return Vector2;
   --  Get gesture pinch delta
   pragma Import (C, GetGesturePinchVector, "GetGesturePinchVector");

   function GetGesturePinchAngle return Interfaces.C.C_float;
   --  Get gesture pinch angle
   pragma Import (C, GetGesturePinchAngle, "GetGesturePinchAngle");

   procedure UpdateCamera (camera_p : access Camera3D; mode : CameraMode);
   --  Update camera position for selected mode
   pragma Import (C, UpdateCamera, "UpdateCamera");

   procedure UpdateCameraPro (camera_p : access Camera3D; movement : Vector3; rotation : Vector3; zoom : Interfaces.C.C_float);
   --  Update camera movement/rotation
   pragma Import (C, UpdateCameraPro, "UpdateCameraPro");

   procedure SetShapesTexture (texture_p : Texture; source : Rectangle);
   --  Set texture and rectangle to be used on shapes drawing
   pragma Import (C, SetShapesTexture, "SetShapesTexture");

   procedure DrawPixel (posX : Interfaces.C.int; posY : Interfaces.C.int; color_p : Color);
   --  Draw a pixel
   pragma Import (C, DrawPixel, "DrawPixel");

   procedure DrawPixelV (position : Vector2; color_p : Color);
   --  Draw a pixel (Vector version)
   pragma Import (C, DrawPixelV, "DrawPixelV");

   procedure DrawLine (startPosX : Interfaces.C.int; startPosY : Interfaces.C.int; endPosX : Interfaces.C.int; endPosY : Interfaces.C.int; color_p : Color);
   --  Draw a line
   pragma Import (C, DrawLine, "DrawLine");

   procedure DrawLineV (startPos : Vector2; endPos : Vector2; color_p : Color);
   --  Draw a line (using gl lines)
   pragma Import (C, DrawLineV, "DrawLineV");

   procedure DrawLineEx (startPos : Vector2; endPos : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw a line (using triangles/quads)
   pragma Import (C, DrawLineEx, "DrawLineEx");

   procedure DrawLineStrip (points : access Vector2; pointCount : Interfaces.C.int; color_p : Color);
   --  Draw lines sequence (using gl lines)
   pragma Import (C, DrawLineStrip, "DrawLineStrip");

   procedure DrawLineBezier (startPos : Vector2; endPos : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw line segment cubic-bezier in-out interpolation
   pragma Import (C, DrawLineBezier, "DrawLineBezier");

   procedure DrawCircle (centerX : Interfaces.C.int; centerY : Interfaces.C.int; radius : Interfaces.C.C_float; color_p : Color);
   --  Draw a color-filled circle
   pragma Import (C, DrawCircle, "DrawCircle");

   procedure DrawCircleSector (center : Vector2; radius : Interfaces.C.C_float; startAngle : Interfaces.C.C_float; endAngle : Interfaces.C.C_float; segments : Interfaces.C.int; color_p : Color);
   --  Draw a piece of a circle
   pragma Import (C, DrawCircleSector, "DrawCircleSector");

   procedure DrawCircleSectorLines (center : Vector2; radius : Interfaces.C.C_float; startAngle : Interfaces.C.C_float; endAngle : Interfaces.C.C_float; segments : Interfaces.C.int; color_p : Color);
   --  Draw circle sector outline
   pragma Import (C, DrawCircleSectorLines, "DrawCircleSectorLines");

   procedure DrawCircleGradient (centerX : Interfaces.C.int; centerY : Interfaces.C.int; radius : Interfaces.C.C_float; color1 : Color; color2 : Color);
   --  Draw a gradient-filled circle
   pragma Import (C, DrawCircleGradient, "DrawCircleGradient");

   procedure DrawCircleV (center : Vector2; radius : Interfaces.C.C_float; color_p : Color);
   --  Draw a color-filled circle (Vector version)
   pragma Import (C, DrawCircleV, "DrawCircleV");

   procedure DrawCircleLines (centerX : Interfaces.C.int; centerY : Interfaces.C.int; radius : Interfaces.C.C_float; color_p : Color);
   --  Draw circle outline
   pragma Import (C, DrawCircleLines, "DrawCircleLines");

   procedure DrawCircleLinesV (center : Vector2; radius : Interfaces.C.C_float; color_p : Color);
   --  Draw circle outline (Vector version)
   pragma Import (C, DrawCircleLinesV, "DrawCircleLinesV");

   procedure DrawEllipse (centerX : Interfaces.C.int; centerY : Interfaces.C.int; radiusH : Interfaces.C.C_float; radiusV : Interfaces.C.C_float; color_p : Color);
   --  Draw ellipse
   pragma Import (C, DrawEllipse, "DrawEllipse");

   procedure DrawEllipseLines (centerX : Interfaces.C.int; centerY : Interfaces.C.int; radiusH : Interfaces.C.C_float; radiusV : Interfaces.C.C_float; color_p : Color);
   --  Draw ellipse outline
   pragma Import (C, DrawEllipseLines, "DrawEllipseLines");

   procedure DrawRing (center : Vector2; innerRadius : Interfaces.C.C_float; outerRadius : Interfaces.C.C_float; startAngle : Interfaces.C.C_float; endAngle : Interfaces.C.C_float; segments : Interfaces.C.int; color_p : Color);
   --  Draw ring
   pragma Import (C, DrawRing, "DrawRing");

   procedure DrawRingLines (center : Vector2; innerRadius : Interfaces.C.C_float; outerRadius : Interfaces.C.C_float; startAngle : Interfaces.C.C_float; endAngle : Interfaces.C.C_float; segments : Interfaces.C.int; color_p : Color);
   --  Draw ring outline
   pragma Import (C, DrawRingLines, "DrawRingLines");

   procedure DrawRectangle (posX : Interfaces.C.int; posY : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int; color_p : Color);
   --  Draw a color-filled rectangle
   pragma Import (C, DrawRectangle, "DrawRectangle");

   procedure DrawRectangleV (position : Vector2; size : Vector2; color_p : Color);
   --  Draw a color-filled rectangle (Vector version)
   pragma Import (C, DrawRectangleV, "DrawRectangleV");

   procedure DrawRectangleRec (rec : Rectangle; color_p : Color);
   --  Draw a color-filled rectangle
   pragma Import (C, DrawRectangleRec, "DrawRectangleRec");

   procedure DrawRectanglePro (rec : Rectangle; origin : Vector2; rotation : Interfaces.C.C_float; color_p : Color);
   --  Draw a color-filled rectangle with pro parameters
   pragma Import (C, DrawRectanglePro, "DrawRectanglePro");

   procedure DrawRectangleGradientV (posX : Interfaces.C.int; posY : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int; color1 : Color; color2 : Color);
   --  Draw a vertical-gradient-filled rectangle
   pragma Import (C, DrawRectangleGradientV, "DrawRectangleGradientV");

   procedure DrawRectangleGradientH (posX : Interfaces.C.int; posY : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int; color1 : Color; color2 : Color);
   --  Draw a horizontal-gradient-filled rectangle
   pragma Import (C, DrawRectangleGradientH, "DrawRectangleGradientH");

   procedure DrawRectangleGradientEx (rec : Rectangle; col1 : Color; col2 : Color; col3 : Color; col4 : Color);
   --  Draw a gradient-filled rectangle with custom vertex colors
   pragma Import (C, DrawRectangleGradientEx, "DrawRectangleGradientEx");

   procedure DrawRectangleLines (posX : Interfaces.C.int; posY : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int; color_p : Color);
   --  Draw rectangle outline
   pragma Import (C, DrawRectangleLines, "DrawRectangleLines");

   procedure DrawRectangleLinesEx (rec : Rectangle; lineThick : Interfaces.C.C_float; color_p : Color);
   --  Draw rectangle outline with extended parameters
   pragma Import (C, DrawRectangleLinesEx, "DrawRectangleLinesEx");

   procedure DrawRectangleRounded (rec : Rectangle; roundness : Interfaces.C.C_float; segments : Interfaces.C.int; color_p : Color);
   --  Draw rectangle with rounded edges
   pragma Import (C, DrawRectangleRounded, "DrawRectangleRounded");

   procedure DrawRectangleRoundedLines (rec : Rectangle; roundness : Interfaces.C.C_float; segments : Interfaces.C.int; lineThick : Interfaces.C.C_float; color_p : Color);
   --  Draw rectangle with rounded edges outline
   pragma Import (C, DrawRectangleRoundedLines, "DrawRectangleRoundedLines");

   procedure DrawTriangle (v1 : Vector2; v2 : Vector2; v3 : Vector2; color_p : Color);
   --  Draw a color-filled triangle (vertex in counter-clockwise order!)
   pragma Import (C, DrawTriangle, "DrawTriangle");

   procedure DrawTriangleLines (v1 : Vector2; v2 : Vector2; v3 : Vector2; color_p : Color);
   --  Draw triangle outline (vertex in counter-clockwise order!)
   pragma Import (C, DrawTriangleLines, "DrawTriangleLines");

   procedure DrawTriangleFan (points : access Vector2; pointCount : Interfaces.C.int; color_p : Color);
   --  Draw a triangle fan defined by points (first vertex is the center)
   pragma Import (C, DrawTriangleFan, "DrawTriangleFan");

   procedure DrawTriangleStrip (points : access Vector2; pointCount : Interfaces.C.int; color_p : Color);
   --  Draw a triangle strip defined by points
   pragma Import (C, DrawTriangleStrip, "DrawTriangleStrip");

   procedure DrawPoly (center : Vector2; sides : Interfaces.C.int; radius : Interfaces.C.C_float; rotation : Interfaces.C.C_float; color_p : Color);
   --  Draw a regular polygon (Vector version)
   pragma Import (C, DrawPoly, "DrawPoly");

   procedure DrawPolyLines (center : Vector2; sides : Interfaces.C.int; radius : Interfaces.C.C_float; rotation : Interfaces.C.C_float; color_p : Color);
   --  Draw a polygon outline of n sides
   pragma Import (C, DrawPolyLines, "DrawPolyLines");

   procedure DrawPolyLinesEx (center : Vector2; sides : Interfaces.C.int; radius : Interfaces.C.C_float; rotation : Interfaces.C.C_float; lineThick : Interfaces.C.C_float; color_p : Color);
   --  Draw a polygon outline of n sides with extended parameters
   pragma Import (C, DrawPolyLinesEx, "DrawPolyLinesEx");

   procedure DrawSplineLinear (points : access Vector2; pointCount : Interfaces.C.int; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline: Linear, minimum 2 points
   pragma Import (C, DrawSplineLinear, "DrawSplineLinear");

   procedure DrawSplineBasis (points : access Vector2; pointCount : Interfaces.C.int; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline: B-Spline, minimum 4 points
   pragma Import (C, DrawSplineBasis, "DrawSplineBasis");

   procedure DrawSplineCatmullRom (points : access Vector2; pointCount : Interfaces.C.int; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline: Catmull-Rom, minimum 4 points
   pragma Import (C, DrawSplineCatmullRom, "DrawSplineCatmullRom");

   procedure DrawSplineBezierQuadratic (points : access Vector2; pointCount : Interfaces.C.int; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]
   pragma Import (C, DrawSplineBezierQuadratic, "DrawSplineBezierQuadratic");

   procedure DrawSplineBezierCubic (points : access Vector2; pointCount : Interfaces.C.int; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]
   pragma Import (C, DrawSplineBezierCubic, "DrawSplineBezierCubic");

   procedure DrawSplineSegmentLinear (p1 : Vector2; p2 : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline segment: Linear, 2 points
   pragma Import (C, DrawSplineSegmentLinear, "DrawSplineSegmentLinear");

   procedure DrawSplineSegmentBasis (p1 : Vector2; p2 : Vector2; p3 : Vector2; p4 : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline segment: B-Spline, 4 points
   pragma Import (C, DrawSplineSegmentBasis, "DrawSplineSegmentBasis");

   procedure DrawSplineSegmentCatmullRom (p1 : Vector2; p2 : Vector2; p3 : Vector2; p4 : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline segment: Catmull-Rom, 4 points
   pragma Import (C, DrawSplineSegmentCatmullRom, "DrawSplineSegmentCatmullRom");

   procedure DrawSplineSegmentBezierQuadratic (p1 : Vector2; c2 : Vector2; p3 : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline segment: Quadratic Bezier, 2 points, 1 control point
   pragma Import (C, DrawSplineSegmentBezierQuadratic, "DrawSplineSegmentBezierQuadratic");

   procedure DrawSplineSegmentBezierCubic (p1 : Vector2; c2 : Vector2; c3 : Vector2; p4 : Vector2; thick : Interfaces.C.C_float; color_p : Color);
   --  Draw spline segment: Cubic Bezier, 2 points, 2 control points
   pragma Import (C, DrawSplineSegmentBezierCubic, "DrawSplineSegmentBezierCubic");

   function GetSplinePointLinear (startPos : Vector2; endPos : Vector2; t : Interfaces.C.C_float) return Vector2;
   --  Get (evaluate) spline point: Linear
   pragma Import (C, GetSplinePointLinear, "GetSplinePointLinear");

   function GetSplinePointBasis (p1 : Vector2; p2 : Vector2; p3 : Vector2; p4 : Vector2; t : Interfaces.C.C_float) return Vector2;
   --  Get (evaluate) spline point: B-Spline
   pragma Import (C, GetSplinePointBasis, "GetSplinePointBasis");

   function GetSplinePointCatmullRom (p1 : Vector2; p2 : Vector2; p3 : Vector2; p4 : Vector2; t : Interfaces.C.C_float) return Vector2;
   --  Get (evaluate) spline point: Catmull-Rom
   pragma Import (C, GetSplinePointCatmullRom, "GetSplinePointCatmullRom");

   function GetSplinePointBezierQuad (p1 : Vector2; c2 : Vector2; p3 : Vector2; t : Interfaces.C.C_float) return Vector2;
   --  Get (evaluate) spline point: Quadratic Bezier
   pragma Import (C, GetSplinePointBezierQuad, "GetSplinePointBezierQuad");

   function GetSplinePointBezierCubic (p1 : Vector2; c2 : Vector2; c3 : Vector2; p4 : Vector2; t : Interfaces.C.C_float) return Vector2;
   --  Get (evaluate) spline point: Cubic Bezier
   pragma Import (C, GetSplinePointBezierCubic, "GetSplinePointBezierCubic");

   function CheckCollisionRecs (rec1 : Rectangle; rec2 : Rectangle) return Interfaces.C.C_bool;
   --  Check collision between two rectangles
   pragma Import (C, CheckCollisionRecs, "CheckCollisionRecs");

   function CheckCollisionCircles (center1 : Vector2; radius1 : Interfaces.C.C_float; center2 : Vector2; radius2 : Interfaces.C.C_float) return Interfaces.C.C_bool;
   --  Check collision between two circles
   pragma Import (C, CheckCollisionCircles, "CheckCollisionCircles");

   function CheckCollisionCircleRec (center : Vector2; radius : Interfaces.C.C_float; rec : Rectangle) return Interfaces.C.C_bool;
   --  Check collision between circle and rectangle
   pragma Import (C, CheckCollisionCircleRec, "CheckCollisionCircleRec");

   function CheckCollisionPointRec (point : Vector2; rec : Rectangle) return Interfaces.C.C_bool;
   --  Check if point is inside rectangle
   pragma Import (C, CheckCollisionPointRec, "CheckCollisionPointRec");

   function CheckCollisionPointCircle (point : Vector2; center : Vector2; radius : Interfaces.C.C_float) return Interfaces.C.C_bool;
   --  Check if point is inside circle
   pragma Import (C, CheckCollisionPointCircle, "CheckCollisionPointCircle");

   function CheckCollisionPointTriangle (point : Vector2; p1 : Vector2; p2 : Vector2; p3 : Vector2) return Interfaces.C.C_bool;
   --  Check if point is inside a triangle
   pragma Import (C, CheckCollisionPointTriangle, "CheckCollisionPointTriangle");

   function CheckCollisionPointPoly (point : Vector2; points : access Vector2; pointCount : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if point is within a polygon described by array of vertices
   pragma Import (C, CheckCollisionPointPoly, "CheckCollisionPointPoly");

   function CheckCollisionLines (startPos1 : Vector2; endPos1 : Vector2; startPos2 : Vector2; endPos2 : Vector2; collisionPoint : access Vector2) return Interfaces.C.C_bool;
   --  Check the collision between two lines defined by two points each, returns collision point by reference
   pragma Import (C, CheckCollisionLines, "CheckCollisionLines");

   function CheckCollisionPointLine (point : Vector2; p1 : Vector2; p2 : Vector2; threshold : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
   pragma Import (C, CheckCollisionPointLine, "CheckCollisionPointLine");

   function GetCollisionRec (rec1 : Rectangle; rec2 : Rectangle) return Rectangle;
   --  Get collision rectangle for two rectangles collision
   pragma Import (C, GetCollisionRec, "GetCollisionRec");

   function LoadImage (fileName : Interfaces.C.Strings.chars_ptr) return Image;
   --  Load image from file into CPU memory (RAM)
   pragma Import (C, LoadImage, "LoadImage");

   function LoadImage (fileName : String) return Image;
   --  Load image from file into CPU memory (RAM)

   function LoadImageRaw (fileName : Interfaces.C.Strings.chars_ptr; width : Interfaces.C.int; height : Interfaces.C.int; format : PixelFormat; headerSize : Interfaces.C.int) return Image;
   --  Load image from RAW file data
   pragma Import (C, LoadImageRaw, "LoadImageRaw");

   function LoadImageRaw (fileName : String; width : Interfaces.C.int; height : Interfaces.C.int; format : PixelFormat; headerSize : Interfaces.C.int) return Image;
   --  Load image from RAW file data

   function LoadImageAnim (fileName : Interfaces.C.Strings.chars_ptr; frames : access Interfaces.C.int) return Image;
   --  Load image sequence from file (frames appended to image.data)
   pragma Import (C, LoadImageAnim, "LoadImageAnim");

   function LoadImageAnim (fileName : String; frames : access Interfaces.C.int) return Image;
   --  Load image sequence from file (frames appended to image.data)

   function LoadImageFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int) return Image;
   --  Load image from memory buffer, fileType refers to extension: i.e. '.png'
   pragma Import (C, LoadImageFromMemory, "LoadImageFromMemory");

   function LoadImageFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int) return Image;
   --  Load image from memory buffer, fileType refers to extension: i.e. '.png'

   function LoadImageFromTexture (texture_p : Texture) return Image;
   --  Load image from GPU texture data
   pragma Import (C, LoadImageFromTexture, "LoadImageFromTexture");

   function LoadImageFromScreen return Image;
   --  Load image from screen buffer and (screenshot)
   pragma Import (C, LoadImageFromScreen, "LoadImageFromScreen");

   function IsImageReady (image_p : Image) return Interfaces.C.C_bool;
   --  Check if an image is ready
   pragma Import (C, IsImageReady, "IsImageReady");

   procedure UnloadImage (image_p : Image);
   --  Unload image from CPU memory (RAM)
   pragma Import (C, UnloadImage, "UnloadImage");

   function ExportImage (image_p : Image; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export image data to file, returns true on success
   pragma Import (C, ExportImage, "ExportImage");

   function ExportImage (image_p : Image; fileName : String) return Interfaces.C.C_bool;
   --  Export image data to file, returns true on success

   function ExportImageToMemory (image_p : Image; fileType : Interfaces.C.Strings.chars_ptr; fileSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Export image to memory buffer
   pragma Import (C, ExportImageToMemory, "ExportImageToMemory");

   function ExportImageToMemory (image_p : Image; fileType : String; fileSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Export image to memory buffer

   function ExportImageAsCode (image_p : Image; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export image as code file defining an array of bytes, returns true on success
   pragma Import (C, ExportImageAsCode, "ExportImageAsCode");

   function ExportImageAsCode (image_p : Image; fileName : String) return Interfaces.C.C_bool;
   --  Export image as code file defining an array of bytes, returns true on success

   function GenImageColor (width : Interfaces.C.int; height : Interfaces.C.int; color_p : Color) return Image;
   --  Generate image: plain color
   pragma Import (C, GenImageColor, "GenImageColor");

   function GenImageGradientLinear (width : Interfaces.C.int; height : Interfaces.C.int; direction : Interfaces.C.int; start : Color; end_p : Color) return Image;
   --  Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient
   pragma Import (C, GenImageGradientLinear, "GenImageGradientLinear");

   function GenImageGradientRadial (width : Interfaces.C.int; height : Interfaces.C.int; density : Interfaces.C.C_float; inner : Color; outer : Color) return Image;
   --  Generate image: radial gradient
   pragma Import (C, GenImageGradientRadial, "GenImageGradientRadial");

   function GenImageGradientSquare (width : Interfaces.C.int; height : Interfaces.C.int; density : Interfaces.C.C_float; inner : Color; outer : Color) return Image;
   --  Generate image: square gradient
   pragma Import (C, GenImageGradientSquare, "GenImageGradientSquare");

   function GenImageChecked (width : Interfaces.C.int; height : Interfaces.C.int; checksX : Interfaces.C.int; checksY : Interfaces.C.int; col1 : Color; col2 : Color) return Image;
   --  Generate image: checked
   pragma Import (C, GenImageChecked, "GenImageChecked");

   function GenImageWhiteNoise (width : Interfaces.C.int; height : Interfaces.C.int; factor : Interfaces.C.C_float) return Image;
   --  Generate image: white noise
   pragma Import (C, GenImageWhiteNoise, "GenImageWhiteNoise");

   function GenImagePerlinNoise (width : Interfaces.C.int; height : Interfaces.C.int; offsetX : Interfaces.C.int; offsetY : Interfaces.C.int; scale : Interfaces.C.C_float) return Image;
   --  Generate image: perlin noise
   pragma Import (C, GenImagePerlinNoise, "GenImagePerlinNoise");

   function GenImageCellular (width : Interfaces.C.int; height : Interfaces.C.int; tileSize : Interfaces.C.int) return Image;
   --  Generate image: cellular algorithm, bigger tileSize means bigger cells
   pragma Import (C, GenImageCellular, "GenImageCellular");

   function GenImageText (width : Interfaces.C.int; height : Interfaces.C.int; text : Interfaces.C.Strings.chars_ptr) return Image;
   --  Generate image: grayscale image from text data
   pragma Import (C, GenImageText, "GenImageText");

   function GenImageText (width : Interfaces.C.int; height : Interfaces.C.int; text : String) return Image;
   --  Generate image: grayscale image from text data

   function ImageCopy (image_p : Image) return Image;
   --  Create an image duplicate (useful for transformations)
   pragma Import (C, ImageCopy, "ImageCopy");

   function ImageFromImage (image_p : Image; rec : Rectangle) return Image;
   --  Create an image from another image piece
   pragma Import (C, ImageFromImage, "ImageFromImage");

   function ImageText (text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.int; color_p : Color) return Image;
   --  Create an image from text (default font)
   pragma Import (C, ImageText, "ImageText");

   function ImageText (text : String; fontSize : Interfaces.C.int; color_p : Color) return Image;
   --  Create an image from text (default font)

   function ImageTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) return Image;
   --  Create an image from text (custom sprite font)
   pragma Import (C, ImageTextEx, "ImageTextEx");

   function ImageTextEx (font_p : Font; text : String; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) return Image;
   --  Create an image from text (custom sprite font)

   procedure ImageFormat (image_p : access Image; newFormat : Interfaces.C.int);
   --  Convert image data to desired format
   pragma Import (C, ImageFormat, "ImageFormat");

   procedure ImageToPOT (image_p : access Image; fill : Color);
   --  Convert image to POT (power-of-two)
   pragma Import (C, ImageToPOT, "ImageToPOT");

   procedure ImageCrop (image_p : access Image; crop : Rectangle);
   --  Crop an image to a defined rectangle
   pragma Import (C, ImageCrop, "ImageCrop");

   procedure ImageAlphaCrop (image_p : access Image; threshold : Interfaces.C.C_float);
   --  Crop image depending on alpha value
   pragma Import (C, ImageAlphaCrop, "ImageAlphaCrop");

   procedure ImageAlphaClear (image_p : access Image; color_p : Color; threshold : Interfaces.C.C_float);
   --  Clear alpha channel to desired color
   pragma Import (C, ImageAlphaClear, "ImageAlphaClear");

   procedure ImageAlphaMask (image_p : access Image; alphaMask : Image);
   --  Apply alpha mask to image
   pragma Import (C, ImageAlphaMask, "ImageAlphaMask");

   procedure ImageAlphaPremultiply (image_p : access Image);
   --  Premultiply alpha channel
   pragma Import (C, ImageAlphaPremultiply, "ImageAlphaPremultiply");

   procedure ImageBlurGaussian (image_p : access Image; blurSize : Interfaces.C.int);
   --  Apply Gaussian blur using a box blur approximation
   pragma Import (C, ImageBlurGaussian, "ImageBlurGaussian");

   procedure ImageResize (image_p : access Image; newWidth : Interfaces.C.int; newHeight : Interfaces.C.int);
   --  Resize image (Bicubic scaling algorithm)
   pragma Import (C, ImageResize, "ImageResize");

   procedure ImageResizeNN (image_p : access Image; newWidth : Interfaces.C.int; newHeight : Interfaces.C.int);
   --  Resize image (Nearest-Neighbor scaling algorithm)
   pragma Import (C, ImageResizeNN, "ImageResizeNN");

   procedure ImageResizeCanvas (image_p : access Image; newWidth : Interfaces.C.int; newHeight : Interfaces.C.int; offsetX : Interfaces.C.int; offsetY : Interfaces.C.int; fill : Color);
   --  Resize canvas and fill with color
   pragma Import (C, ImageResizeCanvas, "ImageResizeCanvas");

   procedure ImageMipmaps (image_p : access Image);
   --  Compute all mipmap levels for a provided image
   pragma Import (C, ImageMipmaps, "ImageMipmaps");

   procedure ImageDither (image_p : access Image; rBpp : Interfaces.C.int; gBpp : Interfaces.C.int; bBpp : Interfaces.C.int; aBpp : Interfaces.C.int);
   --  Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
   pragma Import (C, ImageDither, "ImageDither");

   procedure ImageFlipVertical (image_p : access Image);
   --  Flip image vertically
   pragma Import (C, ImageFlipVertical, "ImageFlipVertical");

   procedure ImageFlipHorizontal (image_p : access Image);
   --  Flip image horizontally
   pragma Import (C, ImageFlipHorizontal, "ImageFlipHorizontal");

   procedure ImageRotate (image_p : access Image; degrees : Interfaces.C.int);
   --  Rotate image by input angle in degrees (-359 to 359)
   pragma Import (C, ImageRotate, "ImageRotate");

   procedure ImageRotateCW (image_p : access Image);
   --  Rotate image clockwise 90deg
   pragma Import (C, ImageRotateCW, "ImageRotateCW");

   procedure ImageRotateCCW (image_p : access Image);
   --  Rotate image counter-clockwise 90deg
   pragma Import (C, ImageRotateCCW, "ImageRotateCCW");

   procedure ImageColorTint (image_p : access Image; color_p : Color);
   --  Modify image color: tint
   pragma Import (C, ImageColorTint, "ImageColorTint");

   procedure ImageColorInvert (image_p : access Image);
   --  Modify image color: invert
   pragma Import (C, ImageColorInvert, "ImageColorInvert");

   procedure ImageColorGrayscale (image_p : access Image);
   --  Modify image color: grayscale
   pragma Import (C, ImageColorGrayscale, "ImageColorGrayscale");

   procedure ImageColorContrast (image_p : access Image; contrast : Interfaces.C.C_float);
   --  Modify image color: contrast (-100 to 100)
   pragma Import (C, ImageColorContrast, "ImageColorContrast");

   procedure ImageColorBrightness (image_p : access Image; brightness : Interfaces.C.int);
   --  Modify image color: brightness (-255 to 255)
   pragma Import (C, ImageColorBrightness, "ImageColorBrightness");

   procedure ImageColorReplace (image_p : access Image; color_p : Color; replace : Color);
   --  Modify image color: replace color
   pragma Import (C, ImageColorReplace, "ImageColorReplace");

   function LoadImageColors (image_p : Image) return access Color;
   --  Load color data from image as a Color array (RGBA - 32bit)
   pragma Import (C, LoadImageColors, "LoadImageColors");

   function LoadImagePalette (image_p : Image; maxPaletteSize : Interfaces.C.int; colorCount : access Interfaces.C.int) return access Color;
   --  Load colors palette from image as a Color array (RGBA - 32bit)
   pragma Import (C, LoadImagePalette, "LoadImagePalette");

   procedure UnloadImageColors (colors : access Color);
   --  Unload color data loaded with LoadImageColors()
   pragma Import (C, UnloadImageColors, "UnloadImageColors");

   procedure UnloadImagePalette (colors : access Color);
   --  Unload colors palette loaded with LoadImagePalette()
   pragma Import (C, UnloadImagePalette, "UnloadImagePalette");

   function GetImageAlphaBorder (image_p : Image; threshold : Interfaces.C.C_float) return Rectangle;
   --  Get image alpha border rectangle
   pragma Import (C, GetImageAlphaBorder, "GetImageAlphaBorder");

   function GetImageColor (image_p : Image; x : Interfaces.C.int; y : Interfaces.C.int) return Color;
   --  Get image pixel color at (x, y) position
   pragma Import (C, GetImageColor, "GetImageColor");

   procedure ImageClearBackground (dst : access Image; color_p : Color);
   --  Clear image background with given color
   pragma Import (C, ImageClearBackground, "ImageClearBackground");

   procedure ImageDrawPixel (dst : access Image; posX : Interfaces.C.int; posY : Interfaces.C.int; color_p : Color);
   --  Draw pixel within an image
   pragma Import (C, ImageDrawPixel, "ImageDrawPixel");

   procedure ImageDrawPixelV (dst : access Image; position : Vector2; color_p : Color);
   --  Draw pixel within an image (Vector version)
   pragma Import (C, ImageDrawPixelV, "ImageDrawPixelV");

   procedure ImageDrawLine (dst : access Image; startPosX : Interfaces.C.int; startPosY : Interfaces.C.int; endPosX : Interfaces.C.int; endPosY : Interfaces.C.int; color_p : Color);
   --  Draw line within an image
   pragma Import (C, ImageDrawLine, "ImageDrawLine");

   procedure ImageDrawLineV (dst : access Image; start : Vector2; end_p : Vector2; color_p : Color);
   --  Draw line within an image (Vector version)
   pragma Import (C, ImageDrawLineV, "ImageDrawLineV");

   procedure ImageDrawCircle (dst : access Image; centerX : Interfaces.C.int; centerY : Interfaces.C.int; radius : Interfaces.C.int; color_p : Color);
   --  Draw a filled circle within an image
   pragma Import (C, ImageDrawCircle, "ImageDrawCircle");

   procedure ImageDrawCircleV (dst : access Image; center : Vector2; radius : Interfaces.C.int; color_p : Color);
   --  Draw a filled circle within an image (Vector version)
   pragma Import (C, ImageDrawCircleV, "ImageDrawCircleV");

   procedure ImageDrawCircleLines (dst : access Image; centerX : Interfaces.C.int; centerY : Interfaces.C.int; radius : Interfaces.C.int; color_p : Color);
   --  Draw circle outline within an image
   pragma Import (C, ImageDrawCircleLines, "ImageDrawCircleLines");

   procedure ImageDrawCircleLinesV (dst : access Image; center : Vector2; radius : Interfaces.C.int; color_p : Color);
   --  Draw circle outline within an image (Vector version)
   pragma Import (C, ImageDrawCircleLinesV, "ImageDrawCircleLinesV");

   procedure ImageDrawRectangle (dst : access Image; posX : Interfaces.C.int; posY : Interfaces.C.int; width : Interfaces.C.int; height : Interfaces.C.int; color_p : Color);
   --  Draw rectangle within an image
   pragma Import (C, ImageDrawRectangle, "ImageDrawRectangle");

   procedure ImageDrawRectangleV (dst : access Image; position : Vector2; size : Vector2; color_p : Color);
   --  Draw rectangle within an image (Vector version)
   pragma Import (C, ImageDrawRectangleV, "ImageDrawRectangleV");

   procedure ImageDrawRectangleRec (dst : access Image; rec : Rectangle; color_p : Color);
   --  Draw rectangle within an image
   pragma Import (C, ImageDrawRectangleRec, "ImageDrawRectangleRec");

   procedure ImageDrawRectangleLines (dst : access Image; rec : Rectangle; thick : Interfaces.C.int; color_p : Color);
   --  Draw rectangle lines within an image
   pragma Import (C, ImageDrawRectangleLines, "ImageDrawRectangleLines");

   procedure ImageDraw (dst : access Image; src : Image; srcRec : Rectangle; dstRec : Rectangle; tint : Color);
   --  Draw a source image within a destination image (tint applied to source)
   pragma Import (C, ImageDraw, "ImageDraw");

   procedure ImageDrawText (dst : access Image; text : Interfaces.C.Strings.chars_ptr; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color);
   --  Draw text (using default font) within an image (destination)
   pragma Import (C, ImageDrawText, "ImageDrawText");

   procedure ImageDrawText (dst : access Image; text : String; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color);
   --  Draw text (using default font) within an image (destination)

   procedure ImageDrawTextEx (dst : access Image; font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text (custom sprite font) within an image (destination)
   pragma Import (C, ImageDrawTextEx, "ImageDrawTextEx");

   procedure ImageDrawTextEx (dst : access Image; font_p : Font; text : String; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text (custom sprite font) within an image (destination)

   function LoadTexture (fileName : Interfaces.C.Strings.chars_ptr) return Texture;
   --  Load texture from file into GPU memory (VRAM)
   pragma Import (C, LoadTexture, "LoadTexture");

   function LoadTexture (fileName : String) return Texture;
   --  Load texture from file into GPU memory (VRAM)

   function LoadTextureFromImage (image_p : Image) return Texture;
   --  Load texture from image data
   pragma Import (C, LoadTextureFromImage, "LoadTextureFromImage");

   function LoadTextureCubemap (image_p : Image; layout : CubemapLayout) return Texture;
   --  Load cubemap from image, multiple image cubemap layouts supported
   pragma Import (C, LoadTextureCubemap, "LoadTextureCubemap");

   function LoadRenderTexture (width : Interfaces.C.int; height : Interfaces.C.int) return RenderTexture;
   --  Load texture for rendering (framebuffer)
   pragma Import (C, LoadRenderTexture, "LoadRenderTexture");

   function IsTextureReady (texture_p : Texture) return Interfaces.C.C_bool;
   --  Check if a texture is ready
   pragma Import (C, IsTextureReady, "IsTextureReady");

   procedure UnloadTexture (texture_p : Texture);
   --  Unload texture from GPU memory (VRAM)
   pragma Import (C, UnloadTexture, "UnloadTexture");

   function IsRenderTextureReady (target : RenderTexture) return Interfaces.C.C_bool;
   --  Check if a render texture is ready
   pragma Import (C, IsRenderTextureReady, "IsRenderTextureReady");

   procedure UnloadRenderTexture (target : RenderTexture);
   --  Unload render texture from GPU memory (VRAM)
   pragma Import (C, UnloadRenderTexture, "UnloadRenderTexture");

   procedure UpdateTexture (texture_p : Texture; pixels : System.Address);
   --  Update GPU texture with new data
   pragma Import (C, UpdateTexture, "UpdateTexture");

   procedure UpdateTextureRec (texture_p : Texture; rec : Rectangle; pixels : System.Address);
   --  Update GPU texture rectangle with new data
   pragma Import (C, UpdateTextureRec, "UpdateTextureRec");

   procedure GenTextureMipmaps (texture_p : access Texture);
   --  Generate GPU mipmaps for a texture
   pragma Import (C, GenTextureMipmaps, "GenTextureMipmaps");

   procedure SetTextureFilter (texture_p : Texture; filter : TextureFilter);
   --  Set texture scaling filter mode
   pragma Import (C, SetTextureFilter, "SetTextureFilter");

   procedure SetTextureWrap (texture_p : Texture; wrap : TextureWrap);
   --  Set texture wrapping mode
   pragma Import (C, SetTextureWrap, "SetTextureWrap");

   procedure DrawTexture (texture_p : Texture; posX : Interfaces.C.int; posY : Interfaces.C.int; tint : Color);
   --  Draw a Texture2D
   pragma Import (C, DrawTexture, "DrawTexture");

   procedure DrawTextureV (texture_p : Texture; position : Vector2; tint : Color);
   --  Draw a Texture2D with position defined as Vector2
   pragma Import (C, DrawTextureV, "DrawTextureV");

   procedure DrawTextureEx (texture_p : Texture; position : Vector2; rotation : Interfaces.C.C_float; scale : Interfaces.C.C_float; tint : Color);
   --  Draw a Texture2D with extended parameters
   pragma Import (C, DrawTextureEx, "DrawTextureEx");

   procedure DrawTextureRec (texture_p : Texture; source : Rectangle; position : Vector2; tint : Color);
   --  Draw a part of a texture defined by a rectangle
   pragma Import (C, DrawTextureRec, "DrawTextureRec");

   procedure DrawTexturePro (texture_p : Texture; source : Rectangle; dest : Rectangle; origin : Vector2; rotation : Interfaces.C.C_float; tint : Color);
   --  Draw a part of a texture defined by a rectangle with 'pro' parameters
   pragma Import (C, DrawTexturePro, "DrawTexturePro");

   procedure DrawTextureNPatch (texture_p : Texture; nPatchInfo_p : NPatchInfo; dest : Rectangle; origin : Vector2; rotation : Interfaces.C.C_float; tint : Color);
   --  Draws a texture (or part of it) that stretches or shrinks nicely
   pragma Import (C, DrawTextureNPatch, "DrawTextureNPatch");

   function Fade (color_p : Color; alpha : Interfaces.C.C_float) return Color;
   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f
   pragma Import (C, Fade, "Fade");

   function ColorToInt (color_p : Color) return Interfaces.C.int;
   --  Get hexadecimal value for a Color
   pragma Import (C, ColorToInt, "ColorToInt");

   function ColorNormalize (color_p : Color) return Vector4;
   --  Get Color normalized as float [0..1]
   pragma Import (C, ColorNormalize, "ColorNormalize");

   function ColorFromNormalized (normalized : Vector4) return Color;
   --  Get Color from normalized values [0..1]
   pragma Import (C, ColorFromNormalized, "ColorFromNormalized");

   function ColorToHSV (color_p : Color) return Vector3;
   --  Get HSV values for a Color, hue [0..360], saturation/value [0..1]
   pragma Import (C, ColorToHSV, "ColorToHSV");

   function ColorFromHSV (hue : Interfaces.C.C_float; saturation : Interfaces.C.C_float; value : Interfaces.C.C_float) return Color;
   --  Get a Color from HSV values, hue [0..360], saturation/value [0..1]
   pragma Import (C, ColorFromHSV, "ColorFromHSV");

   function ColorTint (color_p : Color; tint : Color) return Color;
   --  Get color multiplied with another color
   pragma Import (C, ColorTint, "ColorTint");

   function ColorBrightness (color_p : Color; factor : Interfaces.C.C_float) return Color;
   --  Get color with brightness correction, brightness factor goes from -1.0f to 1.0f
   pragma Import (C, ColorBrightness, "ColorBrightness");

   function ColorContrast (color_p : Color; contrast : Interfaces.C.C_float) return Color;
   --  Get color with contrast correction, contrast values between -1.0f and 1.0f
   pragma Import (C, ColorContrast, "ColorContrast");

   function ColorAlpha (color_p : Color; alpha : Interfaces.C.C_float) return Color;
   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f
   pragma Import (C, ColorAlpha, "ColorAlpha");

   function ColorAlphaBlend (dst : Color; src : Color; tint : Color) return Color;
   --  Get src alpha-blended into dst color with tint
   pragma Import (C, ColorAlphaBlend, "ColorAlphaBlend");

   function GetColor (hexValue : Interfaces.C.unsigned) return Color;
   --  Get Color structure from hexadecimal value
   function GetColor (hexValue : Interfaces.C.int) return Color;
   pragma Import (C, GetColor, "GetColor");

   function GetPixelColor (srcPtr : System.Address; format : PixelFormat) return Color;
   --  Get Color from a source pixel pointer of certain format
   pragma Import (C, GetPixelColor, "GetPixelColor");

   procedure SetPixelColor (dstPtr : System.Address; color_p : Color; format : PixelFormat);
   --  Set color formatted into destination pixel pointer
   pragma Import (C, SetPixelColor, "SetPixelColor");

   function GetPixelDataSize (width : Interfaces.C.int; height : Interfaces.C.int; format : PixelFormat) return Interfaces.C.int;
   --  Get pixel data size in bytes for certain format
   pragma Import (C, GetPixelDataSize, "GetPixelDataSize");

   function GetFontDefault return Font;
   --  Get the default Font
   pragma Import (C, GetFontDefault, "GetFontDefault");

   function LoadFont (fileName : Interfaces.C.Strings.chars_ptr) return Font;
   --  Load font from file into GPU memory (VRAM)
   pragma Import (C, LoadFont, "LoadFont");

   function LoadFont (fileName : String) return Font;
   --  Load font from file into GPU memory (VRAM)

   function LoadFontEx (fileName : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character setFont
   pragma Import (C, LoadFontEx, "LoadFontEx");

   function LoadFontEx (fileName : String; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character setFont

   function LoadFontFromImage (image_p : Image; key : Color; firstChar : Interfaces.C.int) return Font;
   --  Load font from Image (XNA style)
   pragma Import (C, LoadFontFromImage, "LoadFontFromImage");

   function LoadFontFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
   pragma Import (C, LoadFontFromMemory, "LoadFontFromMemory");

   function LoadFontFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from memory buffer, fileType refers to extension: i.e. '.ttf'

   function IsFontReady (font_p : Font) return Interfaces.C.C_bool;
   --  Check if a font is ready
   pragma Import (C, IsFontReady, "IsFontReady");

   function LoadFontData (fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int; type_p : FontType) return access GlyphInfo;
   --  Load font data for further use
   pragma Import (C, LoadFontData, "LoadFontData");

   procedure UnloadFontData (glyphs : access GlyphInfo; glyphCount : Interfaces.C.int);
   --  Unload font chars info data (RAM)
   pragma Import (C, UnloadFontData, "UnloadFontData");

   procedure UnloadFont (font_p : Font);
   --  Unload font from GPU memory (VRAM)
   pragma Import (C, UnloadFont, "UnloadFont");

   function ExportFontAsCode (font_p : Font; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export font as code file, returns true on success
   pragma Import (C, ExportFontAsCode, "ExportFontAsCode");

   function ExportFontAsCode (font_p : Font; fileName : String) return Interfaces.C.C_bool;
   --  Export font as code file, returns true on success

   procedure DrawFPS (posX : Interfaces.C.int; posY : Interfaces.C.int);
   --  Draw current FPS
   pragma Import (C, DrawFPS, "DrawFPS");

   procedure DrawText (text : Interfaces.C.Strings.chars_ptr; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color);
   --  Draw text (using default font)
   pragma Import (C, DrawText, "DrawText");

   procedure DrawText (text : String; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color);
   --  Draw text (using default font)

   procedure DrawTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using font and additional parameters
   pragma Import (C, DrawTextEx, "DrawTextEx");

   procedure DrawTextEx (font_p : Font; text : String; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using font and additional parameters

   procedure DrawTextPro (font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; origin : Vector2; rotation : Interfaces.C.C_float; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using Font and pro parameters (rotation)
   pragma Import (C, DrawTextPro, "DrawTextPro");

   procedure DrawTextPro (font_p : Font; text : String; position : Vector2; origin : Vector2; rotation : Interfaces.C.C_float; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using Font and pro parameters (rotation)

   procedure DrawTextCodepoint (font_p : Font; codepoint : Interfaces.C.int; position : Vector2; fontSize : Interfaces.C.C_float; tint : Color);
   --  Draw one character (codepoint)
   pragma Import (C, DrawTextCodepoint, "DrawTextCodepoint");

   procedure DrawTextCodepoints (font_p : Font; codepoints : access constant Interfaces.C.int; codepointCount : Interfaces.C.int; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw multiple character (codepoint)
   pragma Import (C, DrawTextCodepoints, "DrawTextCodepoints");

   procedure SetTextLineSpacing (spacing : Interfaces.C.int);
   --  Set vertical line spacing when drawing with line-breaks
   pragma Import (C, SetTextLineSpacing, "SetTextLineSpacing");

   function MeasureText (text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.int) return Interfaces.C.int;
   --  Measure string width for default font
   pragma Import (C, MeasureText, "MeasureText");

   function MeasureText (text : String; fontSize : Interfaces.C.int) return Interfaces.C.int;
   --  Measure string width for default font

   function MeasureTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float) return Vector2;
   --  Measure string size for Font
   pragma Import (C, MeasureTextEx, "MeasureTextEx");

   function MeasureTextEx (font_p : Font; text : String; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float) return Vector2;
   --  Measure string size for Font

   function GetGlyphIndex (font_p : Font; codepoint : Interfaces.C.int) return Interfaces.C.int;
   --  Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
   pragma Import (C, GetGlyphIndex, "GetGlyphIndex");

   function GetGlyphInfo (font_p : Font; codepoint : Interfaces.C.int) return GlyphInfo;
   --  Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
   pragma Import (C, GetGlyphInfo, "GetGlyphInfo");

   function GetGlyphAtlasRec (font_p : Font; codepoint : Interfaces.C.int) return Rectangle;
   --  Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
   pragma Import (C, GetGlyphAtlasRec, "GetGlyphAtlasRec");

   function LoadUTF8 (codepoints : access constant Interfaces.C.int; length : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Load UTF-8 text encoded from codepoints array
   pragma Import (C, LoadUTF8, "LoadUTF8");

   procedure UnloadUTF8 (text : Interfaces.C.Strings.chars_ptr);
   --  Unload UTF-8 text encoded from codepoints array
   pragma Import (C, UnloadUTF8, "UnloadUTF8");

   procedure UnloadUTF8 (text : String);
   --  Unload UTF-8 text encoded from codepoints array

   function LoadCodepoints (text : Interfaces.C.Strings.chars_ptr; count : access Interfaces.C.int) return access Interfaces.C.int;
   --  Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
   pragma Import (C, LoadCodepoints, "LoadCodepoints");

   function LoadCodepoints (text : String; count : access Interfaces.C.int) return access Interfaces.C.int;
   --  Load all codepoints from a UTF-8 text string, codepoints count returned by parameter

   procedure UnloadCodepoints (codepoints : access Interfaces.C.int);
   --  Unload codepoints data from memory
   pragma Import (C, UnloadCodepoints, "UnloadCodepoints");

   function GetCodepointCount (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get total number of codepoints in a UTF-8 encoded string
   pragma Import (C, GetCodepointCount, "GetCodepointCount");

   function GetCodepointCount (text : String) return Interfaces.C.int;
   --  Get total number of codepoints in a UTF-8 encoded string

   function GetCodepoint (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepoint, "GetCodepoint");

   function GetCodepoint (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure

   function GetCodepointNext (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepointNext, "GetCodepointNext");

   function GetCodepointNext (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure

   function GetCodepointPrevious (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepointPrevious, "GetCodepointPrevious");

   function GetCodepointPrevious (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure

   function CodepointToUTF8 (codepoint : Interfaces.C.int; utf8Size : access Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Encode one codepoint into UTF-8 byte array (array length returned as parameter)
   pragma Import (C, CodepointToUTF8, "CodepointToUTF8");

   function TextCopy (dst : Interfaces.C.Strings.chars_ptr; src : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Copy one string to another, returns bytes copied
   pragma Import (C, TextCopy, "TextCopy");

   function TextCopy (dst : String; src : String) return Interfaces.C.int;
   --  Copy one string to another, returns bytes copied

   function TextIsEqual (text1 : Interfaces.C.Strings.chars_ptr; text2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if two text string are equal
   pragma Import (C, TextIsEqual, "TextIsEqual");

   function TextIsEqual (text1 : String; text2 : String) return Interfaces.C.C_bool;
   --  Check if two text string are equal

   function TextLength (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.unsigned;
   --  Get text length, checks for '\0' ending
   pragma Import (C, TextLength, "TextLength");

   function TextLength (text : String) return Interfaces.C.unsigned;
   --  Get text length, checks for '\0' ending

   function TextSubtext (text : Interfaces.C.Strings.chars_ptr; position : Interfaces.C.int; length : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Get a piece of a text string
   pragma Import (C, TextSubtext, "TextSubtext");

   function TextSubtext (text : String; position : Interfaces.C.int; length : Interfaces.C.int) return String;
   --  Get a piece of a text string

   function TextReplace (text : Interfaces.C.Strings.chars_ptr; replace : Interfaces.C.Strings.chars_ptr; by : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Replace text string (WARNING: memory must be freed!)
   pragma Import (C, TextReplace, "TextReplace");

   function TextReplace (text : String; replace : String; by : String) return String;
   --  Replace text string (WARNING: memory must be freed!)

   function TextInsert (text : Interfaces.C.Strings.chars_ptr; insert : Interfaces.C.Strings.chars_ptr; position : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Insert text in a position (WARNING: memory must be freed!)
   pragma Import (C, TextInsert, "TextInsert");

   function TextInsert (text : String; insert : String; position : Interfaces.C.int) return String;
   --  Insert text in a position (WARNING: memory must be freed!)

   function TextJoin (textList : access Interfaces.C.Strings.chars_ptr; count : Interfaces.C.int; delimiter : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Join text strings with delimiter
   pragma Import (C, TextJoin, "TextJoin");

   function TextJoin (textList : access Interfaces.C.Strings.chars_ptr; count : Interfaces.C.int; delimiter : String) return String;
   --  Join text strings with delimiter

   function TextSplit (text : Interfaces.C.Strings.chars_ptr; delimiter : Interfaces.C.char; count : access Interfaces.C.int) return access Interfaces.C.Strings.chars_ptr;
   --  Split text into multiple strings
   pragma Import (C, TextSplit, "TextSplit");

   function TextSplit (text : String; delimiter : Interfaces.C.char; count : access Interfaces.C.int) return access Interfaces.C.Strings.chars_ptr;
   --  Split text into multiple strings

   procedure TextAppend (text : Interfaces.C.Strings.chars_ptr; append : Interfaces.C.Strings.chars_ptr; position : access Interfaces.C.int);
   --  Append text at specific position and move cursor!
   pragma Import (C, TextAppend, "TextAppend");

   procedure TextAppend (text : String; append : String; position : access Interfaces.C.int);
   --  Append text at specific position and move cursor!

   function TextFindIndex (text : Interfaces.C.Strings.chars_ptr; find : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Find first text occurrence within a string
   pragma Import (C, TextFindIndex, "TextFindIndex");

   function TextFindIndex (text : String; find : String) return Interfaces.C.int;
   --  Find first text occurrence within a string

   function TextToUpper (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get upper case version of provided string
   pragma Import (C, TextToUpper, "TextToUpper");

   function TextToUpper (text : String) return String;
   --  Get upper case version of provided string

   function TextToLower (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get lower case version of provided string
   pragma Import (C, TextToLower, "TextToLower");

   function TextToLower (text : String) return String;
   --  Get lower case version of provided string

   function TextToPascal (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get Pascal case notation version of provided string
   pragma Import (C, TextToPascal, "TextToPascal");

   function TextToPascal (text : String) return String;
   --  Get Pascal case notation version of provided string

   function TextToInteger (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get integer value from text (negative values not supported)
   pragma Import (C, TextToInteger, "TextToInteger");

   function TextToInteger (text : String) return Interfaces.C.int;
   --  Get integer value from text (negative values not supported)

   procedure DrawLine3D (startPos : Vector3; endPos : Vector3; color_p : Color);
   --  Draw a line in 3D world space
   pragma Import (C, DrawLine3D, "DrawLine3D");

   procedure DrawPoint3D (position : Vector3; color_p : Color);
   --  Draw a point in 3D space, actually a small line
   pragma Import (C, DrawPoint3D, "DrawPoint3D");

   procedure DrawCircle3D (center : Vector3; radius : Interfaces.C.C_float; rotationAxis : Vector3; rotationAngle : Interfaces.C.C_float; color_p : Color);
   --  Draw a circle in 3D world space
   pragma Import (C, DrawCircle3D, "DrawCircle3D");

   procedure DrawTriangle3D (v1 : Vector3; v2 : Vector3; v3 : Vector3; color_p : Color);
   --  Draw a color-filled triangle (vertex in counter-clockwise order!)
   pragma Import (C, DrawTriangle3D, "DrawTriangle3D");

   procedure DrawTriangleStrip3D (points : access Vector3; pointCount : Interfaces.C.int; color_p : Color);
   --  Draw a triangle strip defined by points
   pragma Import (C, DrawTriangleStrip3D, "DrawTriangleStrip3D");

   procedure DrawCube (position : Vector3; width : Interfaces.C.C_float; height : Interfaces.C.C_float; length : Interfaces.C.C_float; color_p : Color);
   --  Draw cube
   pragma Import (C, DrawCube, "DrawCube");

   procedure DrawCubeV (position : Vector3; size : Vector3; color_p : Color);
   --  Draw cube (Vector version)
   pragma Import (C, DrawCubeV, "DrawCubeV");

   procedure DrawCubeWires (position : Vector3; width : Interfaces.C.C_float; height : Interfaces.C.C_float; length : Interfaces.C.C_float; color_p : Color);
   --  Draw cube wires
   pragma Import (C, DrawCubeWires, "DrawCubeWires");

   procedure DrawCubeWiresV (position : Vector3; size : Vector3; color_p : Color);
   --  Draw cube wires (Vector version)
   pragma Import (C, DrawCubeWiresV, "DrawCubeWiresV");

   procedure DrawSphere (centerPos : Vector3; radius : Interfaces.C.C_float; color_p : Color);
   --  Draw sphere
   pragma Import (C, DrawSphere, "DrawSphere");

   procedure DrawSphereEx (centerPos : Vector3; radius : Interfaces.C.C_float; rings : Interfaces.C.int; slices : Interfaces.C.int; color_p : Color);
   --  Draw sphere with extended parameters
   pragma Import (C, DrawSphereEx, "DrawSphereEx");

   procedure DrawSphereWires (centerPos : Vector3; radius : Interfaces.C.C_float; rings : Interfaces.C.int; slices : Interfaces.C.int; color_p : Color);
   --  Draw sphere wires
   pragma Import (C, DrawSphereWires, "DrawSphereWires");

   procedure DrawCylinder (position : Vector3; radiusTop : Interfaces.C.C_float; radiusBottom : Interfaces.C.C_float; height : Interfaces.C.C_float; slices : Interfaces.C.int; color_p : Color);
   --  Draw a cylinder/cone
   pragma Import (C, DrawCylinder, "DrawCylinder");

   procedure DrawCylinderEx (startPos : Vector3; endPos : Vector3; startRadius : Interfaces.C.C_float; endRadius : Interfaces.C.C_float; sides : Interfaces.C.int; color_p : Color);
   --  Draw a cylinder with base at startPos and top at endPos
   pragma Import (C, DrawCylinderEx, "DrawCylinderEx");

   procedure DrawCylinderWires (position : Vector3; radiusTop : Interfaces.C.C_float; radiusBottom : Interfaces.C.C_float; height : Interfaces.C.C_float; slices : Interfaces.C.int; color_p : Color);
   --  Draw a cylinder/cone wires
   pragma Import (C, DrawCylinderWires, "DrawCylinderWires");

   procedure DrawCylinderWiresEx (startPos : Vector3; endPos : Vector3; startRadius : Interfaces.C.C_float; endRadius : Interfaces.C.C_float; sides : Interfaces.C.int; color_p : Color);
   --  Draw a cylinder wires with base at startPos and top at endPos
   pragma Import (C, DrawCylinderWiresEx, "DrawCylinderWiresEx");

   procedure DrawCapsule (startPos : Vector3; endPos : Vector3; radius : Interfaces.C.C_float; slices : Interfaces.C.int; rings : Interfaces.C.int; color_p : Color);
   --  Draw a capsule with the center of its sphere caps at startPos and endPos
   pragma Import (C, DrawCapsule, "DrawCapsule");

   procedure DrawCapsuleWires (startPos : Vector3; endPos : Vector3; radius : Interfaces.C.C_float; slices : Interfaces.C.int; rings : Interfaces.C.int; color_p : Color);
   --  Draw capsule wireframe with the center of its sphere caps at startPos and endPos
   pragma Import (C, DrawCapsuleWires, "DrawCapsuleWires");

   procedure DrawPlane (centerPos : Vector3; size : Vector2; color_p : Color);
   --  Draw a plane XZ
   pragma Import (C, DrawPlane, "DrawPlane");

   procedure DrawRay (ray_p : Ray; color_p : Color);
   --  Draw a ray line
   pragma Import (C, DrawRay, "DrawRay");

   procedure DrawGrid (slices : Interfaces.C.int; spacing : Interfaces.C.C_float);
   --  Draw a grid (centered at (0, 0, 0))
   pragma Import (C, DrawGrid, "DrawGrid");

   function LoadModel (fileName : Interfaces.C.Strings.chars_ptr) return Model;
   --  Load model from files (meshes and materials)
   pragma Import (C, LoadModel, "LoadModel");

   function LoadModel (fileName : String) return Model;
   --  Load model from files (meshes and materials)

   function LoadModelFromMesh (mesh_p : Mesh) return Model;
   --  Load model from generated mesh (default material)
   pragma Import (C, LoadModelFromMesh, "LoadModelFromMesh");

   function IsModelReady (model_p : Model) return Interfaces.C.C_bool;
   --  Check if a model is ready
   pragma Import (C, IsModelReady, "IsModelReady");

   procedure UnloadModel (model_p : Model);
   --  Unload model (including meshes) from memory (RAM and/or VRAM)
   pragma Import (C, UnloadModel, "UnloadModel");

   function GetModelBoundingBox (model_p : Model) return BoundingBox;
   --  Compute model bounding box limits (considers all meshes)
   pragma Import (C, GetModelBoundingBox, "GetModelBoundingBox");

   procedure DrawModel (model_p : Model; position : Vector3; scale : Interfaces.C.C_float; tint : Color);
   --  Draw a model (with texture if set)
   pragma Import (C, DrawModel, "DrawModel");

   procedure DrawModelEx (model_p : Model; position : Vector3; rotationAxis : Vector3; rotationAngle : Interfaces.C.C_float; scale : Vector3; tint : Color);
   --  Draw a model with extended parameters
   pragma Import (C, DrawModelEx, "DrawModelEx");

   procedure DrawModelWires (model_p : Model; position : Vector3; scale : Interfaces.C.C_float; tint : Color);
   --  Draw a model wires (with texture if set)
   pragma Import (C, DrawModelWires, "DrawModelWires");

   procedure DrawModelWiresEx (model_p : Model; position : Vector3; rotationAxis : Vector3; rotationAngle : Interfaces.C.C_float; scale : Vector3; tint : Color);
   --  Draw a model wires (with texture if set) with extended parameters
   pragma Import (C, DrawModelWiresEx, "DrawModelWiresEx");

   procedure DrawBoundingBox (box : BoundingBox; color_p : Color);
   --  Draw bounding box (wires)
   pragma Import (C, DrawBoundingBox, "DrawBoundingBox");

   procedure DrawBillboard (camera_p : Camera3D; texture_p : Texture; position : Vector3; size : Interfaces.C.C_float; tint : Color);
   --  Draw a billboard texture
   pragma Import (C, DrawBillboard, "DrawBillboard");

   procedure DrawBillboardRec (camera_p : Camera3D; texture_p : Texture; source : Rectangle; position : Vector3; size : Vector2; tint : Color);
   --  Draw a billboard texture defined by source
   pragma Import (C, DrawBillboardRec, "DrawBillboardRec");

   procedure DrawBillboardPro (camera_p : Camera3D; texture_p : Texture; source : Rectangle; position : Vector3; up : Vector3; size : Vector2; origin : Vector2; rotation : Interfaces.C.C_float; tint : Color);
   --  Draw a billboard texture defined by source and rotation
   pragma Import (C, DrawBillboardPro, "DrawBillboardPro");

   procedure UploadMesh (mesh_p : access Mesh; dynamic : Interfaces.C.C_bool);
   --  Upload mesh vertex data in GPU and provide VAO/VBO ids
   pragma Import (C, UploadMesh, "UploadMesh");

   procedure UpdateMeshBuffer (mesh_p : Mesh; index : Interfaces.C.int; data : System.Address; dataSize : Interfaces.C.int; offset : Interfaces.C.int);
   --  Update mesh vertex data in GPU for a specific buffer index
   pragma Import (C, UpdateMeshBuffer, "UpdateMeshBuffer");

   procedure UnloadMesh (mesh_p : Mesh);
   --  Unload mesh data from CPU and GPU
   pragma Import (C, UnloadMesh, "UnloadMesh");

   procedure DrawMesh (mesh_p : Mesh; material_p : Material; transform_p : Matrix);
   --  Draw a 3d mesh with material and transform
   pragma Import (C, DrawMesh, "DrawMesh");

   procedure DrawMeshInstanced (mesh_p : Mesh; material_p : Material; transforms : access Matrix; instances : Interfaces.C.int);
   --  Draw multiple mesh instances with material and different transforms
   pragma Import (C, DrawMeshInstanced, "DrawMeshInstanced");

   function ExportMesh (mesh_p : Mesh; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export mesh data to file, returns true on success
   pragma Import (C, ExportMesh, "ExportMesh");

   function ExportMesh (mesh_p : Mesh; fileName : String) return Interfaces.C.C_bool;
   --  Export mesh data to file, returns true on success

   function GetMeshBoundingBox (mesh_p : Mesh) return BoundingBox;
   --  Compute mesh bounding box limits
   pragma Import (C, GetMeshBoundingBox, "GetMeshBoundingBox");

   procedure GenMeshTangents (mesh_p : access Mesh);
   --  Compute mesh tangents
   pragma Import (C, GenMeshTangents, "GenMeshTangents");

   function GenMeshPoly (sides : Interfaces.C.int; radius : Interfaces.C.C_float) return Mesh;
   --  Generate polygonal mesh
   pragma Import (C, GenMeshPoly, "GenMeshPoly");

   function GenMeshPlane (width : Interfaces.C.C_float; length : Interfaces.C.C_float; resX : Interfaces.C.int; resZ : Interfaces.C.int) return Mesh;
   --  Generate plane mesh (with subdivisions)
   pragma Import (C, GenMeshPlane, "GenMeshPlane");

   function GenMeshCube (width : Interfaces.C.C_float; height : Interfaces.C.C_float; length : Interfaces.C.C_float) return Mesh;
   --  Generate cuboid mesh
   pragma Import (C, GenMeshCube, "GenMeshCube");

   function GenMeshSphere (radius : Interfaces.C.C_float; rings : Interfaces.C.int; slices : Interfaces.C.int) return Mesh;
   --  Generate sphere mesh (standard sphere)
   pragma Import (C, GenMeshSphere, "GenMeshSphere");

   function GenMeshHemiSphere (radius : Interfaces.C.C_float; rings : Interfaces.C.int; slices : Interfaces.C.int) return Mesh;
   --  Generate half-sphere mesh (no bottom cap)
   pragma Import (C, GenMeshHemiSphere, "GenMeshHemiSphere");

   function GenMeshCylinder (radius : Interfaces.C.C_float; height : Interfaces.C.C_float; slices : Interfaces.C.int) return Mesh;
   --  Generate cylinder mesh
   pragma Import (C, GenMeshCylinder, "GenMeshCylinder");

   function GenMeshCone (radius : Interfaces.C.C_float; height : Interfaces.C.C_float; slices : Interfaces.C.int) return Mesh;
   --  Generate cone/pyramid mesh
   pragma Import (C, GenMeshCone, "GenMeshCone");

   function GenMeshTorus (radius : Interfaces.C.C_float; size : Interfaces.C.C_float; radSeg : Interfaces.C.int; sides : Interfaces.C.int) return Mesh;
   --  Generate torus mesh
   pragma Import (C, GenMeshTorus, "GenMeshTorus");

   function GenMeshKnot (radius : Interfaces.C.C_float; size : Interfaces.C.C_float; radSeg : Interfaces.C.int; sides : Interfaces.C.int) return Mesh;
   --  Generate trefoil knot mesh
   pragma Import (C, GenMeshKnot, "GenMeshKnot");

   function GenMeshHeightmap (heightmap : Image; size : Vector3) return Mesh;
   --  Generate heightmap mesh from image data
   pragma Import (C, GenMeshHeightmap, "GenMeshHeightmap");

   function GenMeshCubicmap (cubicmap : Image; cubeSize : Vector3) return Mesh;
   --  Generate cubes-based map mesh from image data
   pragma Import (C, GenMeshCubicmap, "GenMeshCubicmap");

   function LoadMaterials (fileName : Interfaces.C.Strings.chars_ptr; materialCount : access Interfaces.C.int) return access Material;
   --  Load materials from model file
   pragma Import (C, LoadMaterials, "LoadMaterials");

   function LoadMaterials (fileName : String; materialCount : access Interfaces.C.int) return access Material;
   --  Load materials from model file

   function LoadMaterialDefault return Material;
   --  Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
   pragma Import (C, LoadMaterialDefault, "LoadMaterialDefault");

   function IsMaterialReady (material_p : Material) return Interfaces.C.C_bool;
   --  Check if a material is ready
   pragma Import (C, IsMaterialReady, "IsMaterialReady");

   procedure UnloadMaterial (material_p : Material);
   --  Unload material from GPU memory (VRAM)
   pragma Import (C, UnloadMaterial, "UnloadMaterial");

   procedure SetMaterialTexture (material_p : access Material; mapType : Interfaces.C.int; texture_p : Texture);
   --  Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
   pragma Import (C, SetMaterialTexture, "SetMaterialTexture");

   procedure SetModelMeshMaterial (model_p : access Model; meshId : Interfaces.C.int; materialId : Interfaces.C.int);
   --  Set material for a mesh
   pragma Import (C, SetModelMeshMaterial, "SetModelMeshMaterial");

   function LoadModelAnimations (fileName : Interfaces.C.Strings.chars_ptr; animCount : access Interfaces.C.int) return access ModelAnimation_Array;
   --  Load model animations from file
   pragma Import (C, LoadModelAnimations, "LoadModelAnimations");

   function LoadModelAnimations (fileName : String; animCount : access Interfaces.C.int) return access ModelAnimation_Array;
   --  Load model animations from file

   procedure UpdateModelAnimation (model_p : Model; anim : ModelAnimation; frame : Interfaces.C.int);
   --  Update model animation pose
   pragma Import (C, UpdateModelAnimation, "UpdateModelAnimation");

   procedure UnloadModelAnimation (anim : ModelAnimation);
   --  Unload animation data
   pragma Import (C, UnloadModelAnimation, "UnloadModelAnimation");

   procedure UnloadModelAnimations (animations : access ModelAnimation_Array; animCount : Interfaces.C.int);
   --  Unload animation array data
   pragma Import (C, UnloadModelAnimations, "UnloadModelAnimations");

   function IsModelAnimationValid (model_p : Model; anim : ModelAnimation) return Interfaces.C.C_bool;
   --  Check model animation skeleton match
   pragma Import (C, IsModelAnimationValid, "IsModelAnimationValid");

   function CheckCollisionSpheres (center1 : Vector3; radius1 : Interfaces.C.C_float; center2 : Vector3; radius2 : Interfaces.C.C_float) return Interfaces.C.C_bool;
   --  Check collision between two spheres
   pragma Import (C, CheckCollisionSpheres, "CheckCollisionSpheres");

   function CheckCollisionBoxes (box1 : BoundingBox; box2 : BoundingBox) return Interfaces.C.C_bool;
   --  Check collision between two bounding boxes
   pragma Import (C, CheckCollisionBoxes, "CheckCollisionBoxes");

   function CheckCollisionBoxSphere (box : BoundingBox; center : Vector3; radius : Interfaces.C.C_float) return Interfaces.C.C_bool;
   --  Check collision between box and sphere
   pragma Import (C, CheckCollisionBoxSphere, "CheckCollisionBoxSphere");

   function GetRayCollisionSphere (ray_p : Ray; center : Vector3; radius : Interfaces.C.C_float) return RayCollision;
   --  Get collision info between ray and sphere
   pragma Import (C, GetRayCollisionSphere, "GetRayCollisionSphere");

   function GetRayCollisionBox (ray_p : Ray; box : BoundingBox) return RayCollision;
   --  Get collision info between ray and box
   pragma Import (C, GetRayCollisionBox, "GetRayCollisionBox");

   function GetRayCollisionMesh (ray_p : Ray; mesh_p : Mesh; transform_p : Matrix) return RayCollision;
   --  Get collision info between ray and mesh
   pragma Import (C, GetRayCollisionMesh, "GetRayCollisionMesh");

   function GetRayCollisionTriangle (ray_p : Ray; p1 : Vector3; p2 : Vector3; p3 : Vector3) return RayCollision;
   --  Get collision info between ray and triangle
   pragma Import (C, GetRayCollisionTriangle, "GetRayCollisionTriangle");

   function GetRayCollisionQuad (ray_p : Ray; p1 : Vector3; p2 : Vector3; p3 : Vector3; p4 : Vector3) return RayCollision;
   --  Get collision info between ray and quad
   pragma Import (C, GetRayCollisionQuad, "GetRayCollisionQuad");

   procedure InitAudioDevice;
   --  Initialize audio device and context
   pragma Import (C, InitAudioDevice, "InitAudioDevice");

   procedure CloseAudioDevice;
   --  Close the audio device and context
   pragma Import (C, CloseAudioDevice, "CloseAudioDevice");

   function IsAudioDeviceReady return Interfaces.C.C_bool;
   --  Check if audio device has been initialized successfully
   pragma Import (C, IsAudioDeviceReady, "IsAudioDeviceReady");

   procedure SetMasterVolume (volume : Interfaces.C.C_float);
   --  Set master volume (listener)
   pragma Import (C, SetMasterVolume, "SetMasterVolume");

   function GetMasterVolume return Interfaces.C.C_float;
   --  Get master volume (listener)
   pragma Import (C, GetMasterVolume, "GetMasterVolume");

   function LoadWave (fileName : Interfaces.C.Strings.chars_ptr) return Wave;
   --  Load wave data from file
   pragma Import (C, LoadWave, "LoadWave");

   function LoadWave (fileName : String) return Wave;
   --  Load wave data from file

   function LoadWaveFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int) return Wave;
   --  Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
   pragma Import (C, LoadWaveFromMemory, "LoadWaveFromMemory");

   function LoadWaveFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int) return Wave;
   --  Load wave from memory buffer, fileType refers to extension: i.e. '.wav'

   function IsWaveReady (wave_p : Wave) return Interfaces.C.C_bool;
   --  Checks if wave data is ready
   pragma Import (C, IsWaveReady, "IsWaveReady");

   function LoadSound (fileName : Interfaces.C.Strings.chars_ptr) return Sound;
   --  Load sound from file
   pragma Import (C, LoadSound, "LoadSound");

   function LoadSound (fileName : String) return Sound;
   --  Load sound from file

   function LoadSoundFromWave (wave_p : Wave) return Sound;
   --  Load sound from wave data
   pragma Import (C, LoadSoundFromWave, "LoadSoundFromWave");

   function LoadSoundAlias (source : Sound) return Sound;
   --  Create a new sound that shares the same sample data as the source sound, does not own the sound data
   pragma Import (C, LoadSoundAlias, "LoadSoundAlias");

   function IsSoundReady (sound_p : Sound) return Interfaces.C.C_bool;
   --  Checks if a sound is ready
   pragma Import (C, IsSoundReady, "IsSoundReady");

   procedure UpdateSound (sound_p : Sound; data : System.Address; sampleCount : Interfaces.C.int);
   --  Update sound buffer with new data
   pragma Import (C, UpdateSound, "UpdateSound");

   procedure UnloadWave (wave_p : Wave);
   --  Unload wave data
   pragma Import (C, UnloadWave, "UnloadWave");

   procedure UnloadSound (sound_p : Sound);
   --  Unload sound
   pragma Import (C, UnloadSound, "UnloadSound");

   procedure UnloadSoundAlias (alias : Sound);
   --  Unload a sound alias (does not deallocate sample data)
   pragma Import (C, UnloadSoundAlias, "UnloadSoundAlias");

   function ExportWave (wave_p : Wave; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export wave data to file, returns true on success
   pragma Import (C, ExportWave, "ExportWave");

   function ExportWave (wave_p : Wave; fileName : String) return Interfaces.C.C_bool;
   --  Export wave data to file, returns true on success

   function ExportWaveAsCode (wave_p : Wave; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export wave sample data to code (.h), returns true on success
   pragma Import (C, ExportWaveAsCode, "ExportWaveAsCode");

   function ExportWaveAsCode (wave_p : Wave; fileName : String) return Interfaces.C.C_bool;
   --  Export wave sample data to code (.h), returns true on success

   procedure PlaySound (sound_p : Sound);
   --  Play a sound
   pragma Import (C, PlaySound, "PlaySound");

   procedure StopSound (sound_p : Sound);
   --  Stop playing a sound
   pragma Import (C, StopSound, "StopSound");

   procedure PauseSound (sound_p : Sound);
   --  Pause a sound
   pragma Import (C, PauseSound, "PauseSound");

   procedure ResumeSound (sound_p : Sound);
   --  Resume a paused sound
   pragma Import (C, ResumeSound, "ResumeSound");

   function IsSoundPlaying (sound_p : Sound) return Interfaces.C.C_bool;
   --  Check if a sound is currently playing
   pragma Import (C, IsSoundPlaying, "IsSoundPlaying");

   procedure SetSoundVolume (sound_p : Sound; volume : Interfaces.C.C_float);
   --  Set volume for a sound (1.0 is max level)
   pragma Import (C, SetSoundVolume, "SetSoundVolume");

   procedure SetSoundPitch (sound_p : Sound; pitch : Interfaces.C.C_float);
   --  Set pitch for a sound (1.0 is base level)
   pragma Import (C, SetSoundPitch, "SetSoundPitch");

   procedure SetSoundPan (sound_p : Sound; pan : Interfaces.C.C_float);
   --  Set pan for a sound (0.5 is center)
   pragma Import (C, SetSoundPan, "SetSoundPan");

   function WaveCopy (wave_p : Wave) return Wave;
   --  Copy a wave to a new wave
   pragma Import (C, WaveCopy, "WaveCopy");

   procedure WaveCrop (wave_p : access Wave; initSample : Interfaces.C.int; finalSample : Interfaces.C.int);
   --  Crop a wave to defined samples range
   pragma Import (C, WaveCrop, "WaveCrop");

   procedure WaveFormat (wave_p : access Wave; sampleRate : Interfaces.C.int; sampleSize : Interfaces.C.int; channels : Interfaces.C.int);
   --  Convert wave data to desired format
   pragma Import (C, WaveFormat, "WaveFormat");

   function LoadWaveSamples (wave_p : Wave) return access Interfaces.C.C_float;
   --  Load samples data from wave as a 32bit float data array
   pragma Import (C, LoadWaveSamples, "LoadWaveSamples");

   procedure UnloadWaveSamples (samples : access Interfaces.C.C_float);
   --  Unload samples data loaded with LoadWaveSamples()
   pragma Import (C, UnloadWaveSamples, "UnloadWaveSamples");

   function LoadMusicStream (fileName : Interfaces.C.Strings.chars_ptr) return Music;
   --  Load music stream from file
   pragma Import (C, LoadMusicStream, "LoadMusicStream");

   function LoadMusicStream (fileName : String) return Music;
   --  Load music stream from file

   function LoadMusicStreamFromMemory (fileType : Interfaces.C.Strings.chars_ptr; data : System.Address; dataSize : Interfaces.C.int) return Music;
   --  Load music stream from data
   pragma Import (C, LoadMusicStreamFromMemory, "LoadMusicStreamFromMemory");

   function LoadMusicStreamFromMemory (fileType : String; data : System.Address; dataSize : Interfaces.C.int) return Music;
   --  Load music stream from data

   function IsMusicReady (music_p : Music) return Interfaces.C.C_bool;
   --  Checks if a music stream is ready
   pragma Import (C, IsMusicReady, "IsMusicReady");

   procedure UnloadMusicStream (music_p : Music);
   --  Unload music stream
   pragma Import (C, UnloadMusicStream, "UnloadMusicStream");

   procedure PlayMusicStream (music_p : Music);
   --  Start music playing
   pragma Import (C, PlayMusicStream, "PlayMusicStream");

   function IsMusicStreamPlaying (music_p : Music) return Interfaces.C.C_bool;
   --  Check if music is playing
   pragma Import (C, IsMusicStreamPlaying, "IsMusicStreamPlaying");

   procedure UpdateMusicStream (music_p : Music);
   --  Updates buffers for music streaming
   pragma Import (C, UpdateMusicStream, "UpdateMusicStream");

   procedure StopMusicStream (music_p : Music);
   --  Stop music playing
   pragma Import (C, StopMusicStream, "StopMusicStream");

   procedure PauseMusicStream (music_p : Music);
   --  Pause music playing
   pragma Import (C, PauseMusicStream, "PauseMusicStream");

   procedure ResumeMusicStream (music_p : Music);
   --  Resume playing paused music
   pragma Import (C, ResumeMusicStream, "ResumeMusicStream");

   procedure SeekMusicStream (music_p : Music; position : Interfaces.C.C_float);
   --  Seek music to a position (in seconds)
   pragma Import (C, SeekMusicStream, "SeekMusicStream");

   procedure SetMusicVolume (music_p : Music; volume : Interfaces.C.C_float);
   --  Set volume for music (1.0 is max level)
   pragma Import (C, SetMusicVolume, "SetMusicVolume");

   procedure SetMusicPitch (music_p : Music; pitch : Interfaces.C.C_float);
   --  Set pitch for a music (1.0 is base level)
   pragma Import (C, SetMusicPitch, "SetMusicPitch");

   procedure SetMusicPan (music_p : Music; pan : Interfaces.C.C_float);
   --  Set pan for a music (0.5 is center)
   pragma Import (C, SetMusicPan, "SetMusicPan");

   function GetMusicTimeLength (music_p : Music) return Interfaces.C.C_float;
   --  Get music time length (in seconds)
   pragma Import (C, GetMusicTimeLength, "GetMusicTimeLength");

   function GetMusicTimePlayed (music_p : Music) return Interfaces.C.C_float;
   --  Get current music time played (in seconds)
   pragma Import (C, GetMusicTimePlayed, "GetMusicTimePlayed");

   function LoadAudioStream (sampleRate : Interfaces.C.unsigned; sampleSize : Interfaces.C.unsigned; channels : Interfaces.C.unsigned) return AudioStream;
   --  Load audio stream (to stream raw audio pcm data)
   pragma Import (C, LoadAudioStream, "LoadAudioStream");

   function IsAudioStreamReady (stream : AudioStream) return Interfaces.C.C_bool;
   --  Checks if an audio stream is ready
   pragma Import (C, IsAudioStreamReady, "IsAudioStreamReady");

   procedure UnloadAudioStream (stream : AudioStream);
   --  Unload audio stream and free memory
   pragma Import (C, UnloadAudioStream, "UnloadAudioStream");

   procedure UpdateAudioStream (stream : AudioStream; data : System.Address; frameCount : Interfaces.C.int);
   --  Update audio stream buffers with data
   pragma Import (C, UpdateAudioStream, "UpdateAudioStream");

   function IsAudioStreamProcessed (stream : AudioStream) return Interfaces.C.C_bool;
   --  Check if any audio stream buffers requires refill
   pragma Import (C, IsAudioStreamProcessed, "IsAudioStreamProcessed");

   procedure PlayAudioStream (stream : AudioStream);
   --  Play audio stream
   pragma Import (C, PlayAudioStream, "PlayAudioStream");

   procedure PauseAudioStream (stream : AudioStream);
   --  Pause audio stream
   pragma Import (C, PauseAudioStream, "PauseAudioStream");

   procedure ResumeAudioStream (stream : AudioStream);
   --  Resume audio stream
   pragma Import (C, ResumeAudioStream, "ResumeAudioStream");

   function IsAudioStreamPlaying (stream : AudioStream) return Interfaces.C.C_bool;
   --  Check if audio stream is playing
   pragma Import (C, IsAudioStreamPlaying, "IsAudioStreamPlaying");

   procedure StopAudioStream (stream : AudioStream);
   --  Stop audio stream
   pragma Import (C, StopAudioStream, "StopAudioStream");

   procedure SetAudioStreamVolume (stream : AudioStream; volume : Interfaces.C.C_float);
   --  Set volume for audio stream (1.0 is max level)
   pragma Import (C, SetAudioStreamVolume, "SetAudioStreamVolume");

   procedure SetAudioStreamPitch (stream : AudioStream; pitch : Interfaces.C.C_float);
   --  Set pitch for audio stream (1.0 is base level)
   pragma Import (C, SetAudioStreamPitch, "SetAudioStreamPitch");

   procedure SetAudioStreamPan (stream : AudioStream; pan : Interfaces.C.C_float);
   --  Set pan for audio stream (0.5 is centered)
   pragma Import (C, SetAudioStreamPan, "SetAudioStreamPan");

   procedure SetAudioStreamBufferSizeDefault (size : Interfaces.C.int);
   --  Default size for new audio streams
   pragma Import (C, SetAudioStreamBufferSizeDefault, "SetAudioStreamBufferSizeDefault");

   procedure SetAudioStreamCallback (stream : AudioStream; callback : AudioCallback);
   --  Audio thread callback to request new data
   pragma Import (C, SetAudioStreamCallback, "SetAudioStreamCallback");

   procedure AttachAudioStreamProcessor (stream : AudioStream; processor : AudioCallback);
   --  Attach audio stream processor to stream, receives the samples as <float>s
   pragma Import (C, AttachAudioStreamProcessor, "AttachAudioStreamProcessor");

   procedure DetachAudioStreamProcessor (stream : AudioStream; processor : AudioCallback);
   --  Detach audio stream processor from stream
   pragma Import (C, DetachAudioStreamProcessor, "DetachAudioStreamProcessor");

   procedure AttachAudioMixedProcessor (processor : AudioCallback);
   --  Attach audio stream processor to the entire audio pipeline, receives the samples as <float>s
   pragma Import (C, AttachAudioMixedProcessor, "AttachAudioMixedProcessor");

   procedure DetachAudioMixedProcessor (processor : AudioCallback);
   --  Detach audio stream processor from the entire audio pipeline
   pragma Import (C, DetachAudioMixedProcessor, "DetachAudioMixedProcessor");

   procedure EnableBackfaceCulling;
   --  Enable backface culling globally
   pragma Import (C, EnableBackfaceCulling, "rlEnableBackfaceCulling");

   procedure DisableBackfaceCulling;
   --  Disable backface culling globally
   pragma Import (C, DisableBackfaceCulling, "rlDisableBackfaceCulling");

   RAYLIB_VERSION_MAJOR : constant := 5;
   RAYLIB_VERSION_MINOR : constant := 0;
   RAYLIB_VERSION_PATCH : constant := 0;
   RAYLIB_VERSION : constant String := "5.0";
   PI : constant := 3.141592653589793;
   LIGHTGRAY : constant Color := (200, 200, 200, 255);
   GRAY : constant Color := (130, 130, 130, 255);
   DARKGRAY : constant Color := (80, 80, 80, 255);
   YELLOW : constant Color := (253, 249, 0, 255);
   GOLD : constant Color := (255, 203, 0, 255);
   ORANGE : constant Color := (255, 161, 0, 255);
   PINK : constant Color := (255, 109, 194, 255);
   RED : constant Color := (230, 41, 55, 255);
   MAROON : constant Color := (190, 33, 55, 255);
   GREEN : constant Color := (0, 228, 48, 255);
   LIME : constant Color := (0, 158, 47, 255);
   DARKGREEN : constant Color := (0, 117, 44, 255);
   SKYBLUE : constant Color := (102, 191, 255, 255);
   BLUE : constant Color := (0, 121, 241, 255);
   DARKBLUE : constant Color := (0, 82, 172, 255);
   PURPLE : constant Color := (200, 122, 255, 255);
   VIOLET : constant Color := (135, 60, 190, 255);
   DARKPURPLE : constant Color := (112, 31, 126, 255);
   BEIGE : constant Color := (211, 176, 131, 255);
   BROWN : constant Color := (127, 106, 79, 255);
   DARKBROWN : constant Color := (76, 63, 47, 255);
   WHITE : constant Color := (255, 255, 255, 255);
   BLACK : constant Color := (0, 0, 0, 255);
   BLANK : constant Color := (0, 0, 0, 0);
   MAGENTA : constant Color := (255, 0, 255, 255);
   RAYWHITE : constant Color := (245, 245, 245, 255);
end Raylib;
