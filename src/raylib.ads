with System;
with Interfaces.C;
with Interfaces.C.Strings;
package Raylib
  with Preelaborate
is
   pragma Style_Checks ("M2000");
   type Float2 is array (0 .. 1) of Interfaces.C.C_float;
   type Float4 is array (0 .. 3) of Interfaces.C.C_float;
   type Int4 is array (0 .. 3) of Interfaces.C.int;
   subtype String32 is String (1 .. 32);
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
      format : Interfaces.C.int; -- Data format (PixelFormat type)
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
      layout : Interfaces.C.int; -- Layout of the n-patch: 3x3, 1x3 or 3x1
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
      projection : Interfaces.C.int; -- Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
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
      locs : access Interfaces.C.int; -- Shader locations array (RL_MAX_SHADER_LOCATIONS)
   end record
      with Convention => C_Pass_By_Copy;

   type MaterialMap is record
      texture_f : Texture; -- Material map texture
      color_f : Color; -- Material map color
      value : Interfaces.C.C_float; -- Material map value
   end record
      with Convention => C_Pass_By_Copy;

   type Material is record
      shader_f : Shader; -- Material shader
      maps : access MaterialMap; -- Material maps array (MAX_MATERIAL_MAPS)
      params : Float4; -- Material generic parameters (if required)
   end record
      with Convention => C_Pass_By_Copy;

   type Transform is record
      translation : Vector3; -- Translation
      rotation : Quaternion; -- Rotation
      scale : Vector3; -- Scale
   end record
      with Convention => C_Pass_By_Copy;

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

   type VrDeviceInfo is record
      hResolution : Interfaces.C.int; -- Horizontal resolution in pixels
      vResolution : Interfaces.C.int; -- Vertical resolution in pixels
      hScreenSize : Interfaces.C.C_float; -- Horizontal size in meters
      vScreenSize : Interfaces.C.C_float; -- Vertical size in meters
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

   type AutomationEvent is record
      frame : Interfaces.C.unsigned; -- Event frame
      type_K : Interfaces.C.unsigned; -- Event type (AutomationEventType)
      params : Int4; -- Event parameters (if required)
   end record
      with Convention => C_Pass_By_Copy;

   type AutomationEventList is record
      capacity : Interfaces.C.unsigned; -- Events max entries (MAX_AUTOMATION_EVENTS)
      count : Interfaces.C.unsigned; -- Events entries count
      events : access AutomationEvent; -- Events entries
   end record
      with Convention => C_Pass_By_Copy;

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

   type TraceLogLevel is new Interfaces.C.unsigned;
   --  Trace log level

   LOG_ALL : constant TraceLogLevel := 0; -- Display all logs
   LOG_TRACE : constant TraceLogLevel := 1; -- Trace logging, intended for internal use only
   LOG_DEBUG : constant TraceLogLevel := 2; -- Debug logging, used for internal debugging, it should be disabled on release builds
   LOG_INFO : constant TraceLogLevel := 3; -- Info logging, used for program execution info
   LOG_WARNING : constant TraceLogLevel := 4; -- Warning logging, used on recoverable failures
   LOG_ERROR : constant TraceLogLevel := 5; -- Error logging, used on unrecoverable failures
   LOG_FATAL : constant TraceLogLevel := 6; -- Fatal logging, used to abort program: exit(EXIT_FAILURE)
   LOG_NONE : constant TraceLogLevel := 7; -- Disable logging

   type KeyboardKey is new Interfaces.C.unsigned;
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
   KEY_MENU : constant KeyboardKey := 5; -- Key: Android menu button
   KEY_VOLUME_UP : constant KeyboardKey := 24; -- Key: Android volume up button
   KEY_VOLUME_DOWN : constant KeyboardKey := 25; -- Key: Android volume down button

   type MouseButton is new Interfaces.C.unsigned;
   --  Mouse buttons

   MOUSE_BUTTON_LEFT : constant MouseButton := 0; -- Mouse button left
   MOUSE_BUTTON_RIGHT : constant MouseButton := 1; -- Mouse button right
   MOUSE_BUTTON_MIDDLE : constant MouseButton := 2; -- Mouse button middle (pressed wheel)
   MOUSE_BUTTON_SIDE : constant MouseButton := 3; -- Mouse button side (advanced mouse device)
   MOUSE_BUTTON_EXTRA : constant MouseButton := 4; -- Mouse button extra (advanced mouse device)
   MOUSE_BUTTON_FORWARD : constant MouseButton := 5; -- Mouse button forward (advanced mouse device)
   MOUSE_BUTTON_BACK : constant MouseButton := 6; -- Mouse button back (advanced mouse device)

   type MouseCursor is new Interfaces.C.unsigned;
   --  Mouse cursor

   MOUSE_CURSOR_DEFAULT : constant MouseCursor := 0; -- Default pointer shape
   MOUSE_CURSOR_ARROW : constant MouseCursor := 1; -- Arrow shape
   MOUSE_CURSOR_IBEAM : constant MouseCursor := 2; -- Text writing cursor shape
   MOUSE_CURSOR_CROSSHAIR : constant MouseCursor := 3; -- Cross shape
   MOUSE_CURSOR_POINTING_HAND : constant MouseCursor := 4; -- Pointing hand cursor
   MOUSE_CURSOR_RESIZE_EW : constant MouseCursor := 5; -- Horizontal resize/move arrow shape
   MOUSE_CURSOR_RESIZE_NS : constant MouseCursor := 6; -- Vertical resize/move arrow shape
   MOUSE_CURSOR_RESIZE_NWSE : constant MouseCursor := 7; -- Top-left to bottom-right diagonal resize/move arrow shape
   MOUSE_CURSOR_RESIZE_NESW : constant MouseCursor := 8; -- The top-right to bottom-left diagonal resize/move arrow shape
   MOUSE_CURSOR_RESIZE_ALL : constant MouseCursor := 9; -- The omnidirectional resize/move cursor shape
   MOUSE_CURSOR_NOT_ALLOWED : constant MouseCursor := 10; -- The operation-not-allowed shape

   type GamepadButton is new Interfaces.C.unsigned;
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

   type GamepadAxis is new Interfaces.C.unsigned;
   --  Gamepad axis

   GAMEPAD_AXIS_LEFT_X : constant GamepadAxis := 0; -- Gamepad left stick X axis
   GAMEPAD_AXIS_LEFT_Y : constant GamepadAxis := 1; -- Gamepad left stick Y axis
   GAMEPAD_AXIS_RIGHT_X : constant GamepadAxis := 2; -- Gamepad right stick X axis
   GAMEPAD_AXIS_RIGHT_Y : constant GamepadAxis := 3; -- Gamepad right stick Y axis
   GAMEPAD_AXIS_LEFT_TRIGGER : constant GamepadAxis := 4; -- Gamepad back trigger left, pressure level: [1..-1]
   GAMEPAD_AXIS_RIGHT_TRIGGER : constant GamepadAxis := 5; -- Gamepad back trigger right, pressure level: [1..-1]

   type MaterialMapIndex is new Interfaces.C.unsigned;
   --  Material map index

   MATERIAL_MAP_ALBEDO : constant MaterialMapIndex := 0; -- Albedo material (same as: MATERIAL_MAP_DIFFUSE)
   MATERIAL_MAP_METALNESS : constant MaterialMapIndex := 1; -- Metalness material (same as: MATERIAL_MAP_SPECULAR)
   MATERIAL_MAP_NORMAL : constant MaterialMapIndex := 2; -- Normal material
   MATERIAL_MAP_ROUGHNESS : constant MaterialMapIndex := 3; -- Roughness material
   MATERIAL_MAP_OCCLUSION : constant MaterialMapIndex := 4; -- Ambient occlusion material
   MATERIAL_MAP_EMISSION : constant MaterialMapIndex := 5; -- Emission material
   MATERIAL_MAP_HEIGHT : constant MaterialMapIndex := 6; -- Heightmap material
   MATERIAL_MAP_CUBEMAP : constant MaterialMapIndex := 7; -- Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   MATERIAL_MAP_IRRADIANCE : constant MaterialMapIndex := 8; -- Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   MATERIAL_MAP_PREFILTER : constant MaterialMapIndex := 9; -- Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   MATERIAL_MAP_BRDF : constant MaterialMapIndex := 10; -- Brdf material

   type ShaderLocationIndex is new Interfaces.C.unsigned;
   --  Shader location index

   SHADER_LOC_VERTEX_POSITION : constant ShaderLocationIndex := 0; -- Shader location: vertex attribute: position
   SHADER_LOC_VERTEX_TEXCOORD01 : constant ShaderLocationIndex := 1; -- Shader location: vertex attribute: texcoord01
   SHADER_LOC_VERTEX_TEXCOORD02 : constant ShaderLocationIndex := 2; -- Shader location: vertex attribute: texcoord02
   SHADER_LOC_VERTEX_NORMAL : constant ShaderLocationIndex := 3; -- Shader location: vertex attribute: normal
   SHADER_LOC_VERTEX_TANGENT : constant ShaderLocationIndex := 4; -- Shader location: vertex attribute: tangent
   SHADER_LOC_VERTEX_COLOR : constant ShaderLocationIndex := 5; -- Shader location: vertex attribute: color
   SHADER_LOC_MATRIX_MVP : constant ShaderLocationIndex := 6; -- Shader location: matrix uniform: model-view-projection
   SHADER_LOC_MATRIX_VIEW : constant ShaderLocationIndex := 7; -- Shader location: matrix uniform: view (camera transform)
   SHADER_LOC_MATRIX_PROJECTION : constant ShaderLocationIndex := 8; -- Shader location: matrix uniform: projection
   SHADER_LOC_MATRIX_MODEL : constant ShaderLocationIndex := 9; -- Shader location: matrix uniform: model (transform)
   SHADER_LOC_MATRIX_NORMAL : constant ShaderLocationIndex := 10; -- Shader location: matrix uniform: normal
   SHADER_LOC_VECTOR_VIEW : constant ShaderLocationIndex := 11; -- Shader location: vector uniform: view
   SHADER_LOC_COLOR_DIFFUSE : constant ShaderLocationIndex := 12; -- Shader location: vector uniform: diffuse color
   SHADER_LOC_COLOR_SPECULAR : constant ShaderLocationIndex := 13; -- Shader location: vector uniform: specular color
   SHADER_LOC_COLOR_AMBIENT : constant ShaderLocationIndex := 14; -- Shader location: vector uniform: ambient color
   SHADER_LOC_MAP_ALBEDO : constant ShaderLocationIndex := 15; -- Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
   SHADER_LOC_MAP_METALNESS : constant ShaderLocationIndex := 16; -- Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
   SHADER_LOC_MAP_NORMAL : constant ShaderLocationIndex := 17; -- Shader location: sampler2d texture: normal
   SHADER_LOC_MAP_ROUGHNESS : constant ShaderLocationIndex := 18; -- Shader location: sampler2d texture: roughness
   SHADER_LOC_MAP_OCCLUSION : constant ShaderLocationIndex := 19; -- Shader location: sampler2d texture: occlusion
   SHADER_LOC_MAP_EMISSION : constant ShaderLocationIndex := 20; -- Shader location: sampler2d texture: emission
   SHADER_LOC_MAP_HEIGHT : constant ShaderLocationIndex := 21; -- Shader location: sampler2d texture: height
   SHADER_LOC_MAP_CUBEMAP : constant ShaderLocationIndex := 22; -- Shader location: samplerCube texture: cubemap
   SHADER_LOC_MAP_IRRADIANCE : constant ShaderLocationIndex := 23; -- Shader location: samplerCube texture: irradiance
   SHADER_LOC_MAP_PREFILTER : constant ShaderLocationIndex := 24; -- Shader location: samplerCube texture: prefilter
   SHADER_LOC_MAP_BRDF : constant ShaderLocationIndex := 25; -- Shader location: sampler2d texture: brdf

   type ShaderUniformDataType is new Interfaces.C.unsigned;
   --  Shader uniform data type

   SHADER_UNIFORM_FLOAT : constant ShaderUniformDataType := 0; -- Shader uniform type: float
   SHADER_UNIFORM_VEC2 : constant ShaderUniformDataType := 1; -- Shader uniform type: vec2 (2 float)
   SHADER_UNIFORM_VEC3 : constant ShaderUniformDataType := 2; -- Shader uniform type: vec3 (3 float)
   SHADER_UNIFORM_VEC4 : constant ShaderUniformDataType := 3; -- Shader uniform type: vec4 (4 float)
   SHADER_UNIFORM_INT : constant ShaderUniformDataType := 4; -- Shader uniform type: int
   SHADER_UNIFORM_IVEC2 : constant ShaderUniformDataType := 5; -- Shader uniform type: ivec2 (2 int)
   SHADER_UNIFORM_IVEC3 : constant ShaderUniformDataType := 6; -- Shader uniform type: ivec3 (3 int)
   SHADER_UNIFORM_IVEC4 : constant ShaderUniformDataType := 7; -- Shader uniform type: ivec4 (4 int)
   SHADER_UNIFORM_SAMPLER2D : constant ShaderUniformDataType := 8; -- Shader uniform type: sampler2d

   type ShaderAttributeDataType is new Interfaces.C.unsigned;
   --  Shader attribute data types

   SHADER_ATTRIB_FLOAT : constant ShaderAttributeDataType := 0; -- Shader attribute type: float
   SHADER_ATTRIB_VEC2 : constant ShaderAttributeDataType := 1; -- Shader attribute type: vec2 (2 float)
   SHADER_ATTRIB_VEC3 : constant ShaderAttributeDataType := 2; -- Shader attribute type: vec3 (3 float)
   SHADER_ATTRIB_VEC4 : constant ShaderAttributeDataType := 3; -- Shader attribute type: vec4 (4 float)

   type PixelFormat is new Interfaces.C.unsigned;
   --  Pixel formats

   PIXELFORMAT_UNCOMPRESSED_GRAYSCALE : constant PixelFormat := 1; -- 8 bit per pixel (no alpha)
   PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA : constant PixelFormat := 2; -- 8*2 bpp (2 channels)
   PIXELFORMAT_UNCOMPRESSED_R5G6B5 : constant PixelFormat := 3; -- 16 bpp
   PIXELFORMAT_UNCOMPRESSED_R8G8B8 : constant PixelFormat := 4; -- 24 bpp
   PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 : constant PixelFormat := 5; -- 16 bpp (1 bit alpha)
   PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 : constant PixelFormat := 6; -- 16 bpp (4 bit alpha)
   PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 : constant PixelFormat := 7; -- 32 bpp
   PIXELFORMAT_UNCOMPRESSED_R32 : constant PixelFormat := 8; -- 32 bpp (1 channel - float)
   PIXELFORMAT_UNCOMPRESSED_R32G32B32 : constant PixelFormat := 9; -- 32*3 bpp (3 channels - float)
   PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 : constant PixelFormat := 10; -- 32*4 bpp (4 channels - float)
   PIXELFORMAT_UNCOMPRESSED_R16 : constant PixelFormat := 11; -- 16 bpp (1 channel - half float)
   PIXELFORMAT_UNCOMPRESSED_R16G16B16 : constant PixelFormat := 12; -- 16*3 bpp (3 channels - half float)
   PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 : constant PixelFormat := 13; -- 16*4 bpp (4 channels - half float)
   PIXELFORMAT_COMPRESSED_DXT1_RGB : constant PixelFormat := 14; -- 4 bpp (no alpha)
   PIXELFORMAT_COMPRESSED_DXT1_RGBA : constant PixelFormat := 15; -- 4 bpp (1 bit alpha)
   PIXELFORMAT_COMPRESSED_DXT3_RGBA : constant PixelFormat := 16; -- 8 bpp
   PIXELFORMAT_COMPRESSED_DXT5_RGBA : constant PixelFormat := 17; -- 8 bpp
   PIXELFORMAT_COMPRESSED_ETC1_RGB : constant PixelFormat := 18; -- 4 bpp
   PIXELFORMAT_COMPRESSED_ETC2_RGB : constant PixelFormat := 19; -- 4 bpp
   PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA : constant PixelFormat := 20; -- 8 bpp
   PIXELFORMAT_COMPRESSED_PVRT_RGB : constant PixelFormat := 21; -- 4 bpp
   PIXELFORMAT_COMPRESSED_PVRT_RGBA : constant PixelFormat := 22; -- 4 bpp
   PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA : constant PixelFormat := 23; -- 8 bpp
   PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA : constant PixelFormat := 24; -- 2 bpp

   type TextureFilter is new Interfaces.C.unsigned;
   --  Texture parameters: filter mode

   TEXTURE_FILTER_POINT : constant TextureFilter := 0; -- No filter, just pixel approximation
   TEXTURE_FILTER_BILINEAR : constant TextureFilter := 1; -- Linear filtering
   TEXTURE_FILTER_TRILINEAR : constant TextureFilter := 2; -- Trilinear filtering (linear with mipmaps)
   TEXTURE_FILTER_ANISOTROPIC_4X : constant TextureFilter := 3; -- Anisotropic filtering 4x
   TEXTURE_FILTER_ANISOTROPIC_8X : constant TextureFilter := 4; -- Anisotropic filtering 8x
   TEXTURE_FILTER_ANISOTROPIC_16X : constant TextureFilter := 5; -- Anisotropic filtering 16x

   type TextureWrap is new Interfaces.C.unsigned;
   --  Texture parameters: wrap mode

   TEXTURE_WRAP_REPEAT : constant TextureWrap := 0; -- Repeats texture in tiled mode
   TEXTURE_WRAP_CLAMP : constant TextureWrap := 1; -- Clamps texture to edge pixel in tiled mode
   TEXTURE_WRAP_MIRROR_REPEAT : constant TextureWrap := 2; -- Mirrors and repeats the texture in tiled mode
   TEXTURE_WRAP_MIRROR_CLAMP : constant TextureWrap := 3; -- Mirrors and clamps to border the texture in tiled mode

   type CubemapLayout is new Interfaces.C.unsigned;
   --  Cubemap layouts

   CUBEMAP_LAYOUT_AUTO_DETECT : constant CubemapLayout := 0; -- Automatically detect layout type
   CUBEMAP_LAYOUT_LINE_VERTICAL : constant CubemapLayout := 1; -- Layout is defined by a vertical line with faces
   CUBEMAP_LAYOUT_LINE_HORIZONTAL : constant CubemapLayout := 2; -- Layout is defined by a horizontal line with faces
   CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR : constant CubemapLayout := 3; -- Layout is defined by a 3x4 cross with cubemap faces
   CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE : constant CubemapLayout := 4; -- Layout is defined by a 4x3 cross with cubemap faces
   CUBEMAP_LAYOUT_PANORAMA : constant CubemapLayout := 5; -- Layout is defined by a panorama image (equirrectangular map)

   type FontType is new Interfaces.C.unsigned;
   --  Font type, defines generation method

   FONT_DEFAULT : constant FontType := 0; -- Default font generation, anti-aliased
   FONT_BITMAP : constant FontType := 1; -- Bitmap font generation, no anti-aliasing
   FONT_SDF : constant FontType := 2; -- SDF font generation, requires external shader

   type BlendMode is new Interfaces.C.unsigned;
   --  Color blending modes (pre-defined)

   BLEND_ALPHA : constant BlendMode := 0; -- Blend textures considering alpha (default)
   BLEND_ADDITIVE : constant BlendMode := 1; -- Blend textures adding colors
   BLEND_MULTIPLIED : constant BlendMode := 2; -- Blend textures multiplying colors
   BLEND_ADD_COLORS : constant BlendMode := 3; -- Blend textures adding colors (alternative)
   BLEND_SUBTRACT_COLORS : constant BlendMode := 4; -- Blend textures subtracting colors (alternative)
   BLEND_ALPHA_PREMULTIPLY : constant BlendMode := 5; -- Blend premultiplied textures considering alpha
   BLEND_CUSTOM : constant BlendMode := 6; -- Blend textures using custom src/dst factors (use rlSetBlendFactors())
   BLEND_CUSTOM_SEPARATE : constant BlendMode := 7; -- Blend textures using custom rgb/alpha separate src/dst factors (use rlSetBlendFactorsSeparate())

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

   type CameraMode is new Interfaces.C.unsigned;
   --  Camera system modes

   CAMERA_CUSTOM : constant CameraMode := 0; -- Custom camera
   CAMERA_FREE : constant CameraMode := 1; -- Free camera
   CAMERA_ORBITAL : constant CameraMode := 2; -- Orbital camera
   CAMERA_FIRST_PERSON : constant CameraMode := 3; -- First person camera
   CAMERA_THIRD_PERSON : constant CameraMode := 4; -- Third person camera

   type CameraProjection is new Interfaces.C.unsigned;
   --  Camera projection

   CAMERA_PERSPECTIVE : constant CameraProjection := 0; -- Perspective projection
   CAMERA_ORTHOGRAPHIC : constant CameraProjection := 1; -- Orthographic projection

   type NPatchLayout is new Interfaces.C.unsigned;
   --  N-patch layout

   NPATCH_NINE_PATCH : constant NPatchLayout := 0; -- Npatch layout: 3x3 tiles
   NPATCH_THREE_PATCH_VERTICAL : constant NPatchLayout := 1; -- Npatch layout: 1x3 tiles
   NPATCH_THREE_PATCH_HORIZONTAL : constant NPatchLayout := 2; -- Npatch layout: 3x1 tiles

   procedure InitWindow (width : Interfaces.C.int; height : Interfaces.C.int; title : Interfaces.C.Strings.chars_ptr);
   --  Initialize window and OpenGL context
   pragma Import (C, InitWindow, "InitWindow");

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

   function GetClipboardText return Interfaces.C.Strings.chars_ptr;
   --  Get clipboard text content
   pragma Import (C, GetClipboardText, "GetClipboardText");

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

   procedure BeginBlendMode (mode : Interfaces.C.int);
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

   function LoadShaderFromMemory (vsCode : Interfaces.C.Strings.chars_ptr; fsCode : Interfaces.C.Strings.chars_ptr) return Shader;
   --  Load shader from code strings and bind default locations
   pragma Import (C, LoadShaderFromMemory, "LoadShaderFromMemory");

   function IsShaderReady (shader_p : Shader) return Interfaces.C.C_bool;
   --  Check if a shader is ready
   pragma Import (C, IsShaderReady, "IsShaderReady");

   function GetShaderLocation (shader_p : Shader; uniformName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get shader uniform location
   pragma Import (C, GetShaderLocation, "GetShaderLocation");

   function GetShaderLocationAttrib (shader_p : Shader; attribName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get shader attribute location
   pragma Import (C, GetShaderLocationAttrib, "GetShaderLocationAttrib");

   procedure SetShaderValue (shader_p : Shader; locIndex : Interfaces.C.int; value : System.Address; uniformType : Interfaces.C.int);
   --  Set shader uniform value
   pragma Import (C, SetShaderValue, "SetShaderValue");

   procedure SetShaderValueV (shader_p : Shader; locIndex : Interfaces.C.int; value : System.Address; uniformType : Interfaces.C.int; count : Interfaces.C.int);
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

   function GetScreenToWorldRay (mousePosition : Vector2; camera_p : Camera3D) return Ray;
   --  Get a ray trace from mouse position
   pragma Import (C, GetScreenToWorldRay, "GetScreenToWorldRay");

   function GetScreenToWorldRayEx (mousePosition : Vector2; camera_p : Camera3D; width : Interfaces.C.C_float; height : Interfaces.C.C_float) return Ray;
   --  Get a ray trace from mouse position in a viewport
   pragma Import (C, GetScreenToWorldRayEx, "GetScreenToWorldRayEx");

   function GetWorldToScreen (position : Vector3; camera_p : Camera3D) return Vector2;
   --  Get the screen space position for a 3d world space position
   pragma Import (C, GetWorldToScreen, "GetWorldToScreen");

   function GetWorldToScreenEx (position : Vector3; camera_p : Camera3D; width : Interfaces.C.int; height : Interfaces.C.int) return Vector2;
   --  Get size position for a 3d world space position
   pragma Import (C, GetWorldToScreenEx, "GetWorldToScreenEx");

   function GetWorldToScreen2D (position : Vector2; camera_p : Camera2D) return Vector2;
   --  Get the screen space position for a 2d camera world space position
   pragma Import (C, GetWorldToScreen2D, "GetWorldToScreen2D");

   function GetScreenToWorld2D (position : Vector2; camera_p : Camera2D) return Vector2;
   --  Get the world space position for a 2d camera screen space position
   pragma Import (C, GetScreenToWorld2D, "GetScreenToWorld2D");

   function GetCameraMatrix (camera_p : Camera3D) return Matrix;
   --  Get camera transform matrix (view matrix)
   pragma Import (C, GetCameraMatrix, "GetCameraMatrix");

   function GetCameraMatrix2D (camera_p : Camera2D) return Matrix;
   --  Get camera 2d transform matrix
   pragma Import (C, GetCameraMatrix2D, "GetCameraMatrix2D");

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

   procedure SetConfigFlags (flags : Interfaces.C.unsigned);
   --  Setup init configuration flags (view FLAGS)
   pragma Import (C, SetConfigFlags, "SetConfigFlags");

   procedure OpenURL (url : Interfaces.C.Strings.chars_ptr);
   --  Open URL with default system browser (if available)
   pragma Import (C, OpenURL, "OpenURL");

   procedure SetTraceLogLevel (logLevel : Interfaces.C.int);
   --  Set the current threshold (minimum) log level
   pragma Import (C, SetTraceLogLevel, "SetTraceLogLevel");

   function MemAlloc (size : Interfaces.C.unsigned) return System.Address;
   --  Internal memory allocator
   pragma Import (C, MemAlloc, "MemAlloc");

   function MemRealloc (ptr : System.Address; size : Interfaces.C.unsigned) return System.Address;
   --  Internal memory reallocator
   pragma Import (C, MemRealloc, "MemRealloc");

   procedure MemFree (ptr : System.Address);
   --  Internal memory free
   pragma Import (C, MemFree, "MemFree");

   function LoadFileData (fileName : Interfaces.C.Strings.chars_ptr; dataSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Load file data as byte array (read)
   pragma Import (C, LoadFileData, "LoadFileData");

   procedure UnloadFileData (data : access Interfaces.C.char);
   --  Unload file data allocated by LoadFileData()
   pragma Import (C, UnloadFileData, "UnloadFileData");

   function SaveFileData (fileName : Interfaces.C.Strings.chars_ptr; data : System.Address; dataSize : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Save data to file from byte array (write), returns true on success
   pragma Import (C, SaveFileData, "SaveFileData");

   function ExportDataAsCode (data : System.Address; dataSize : Interfaces.C.int; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export data to code (.h), returns true on success
   pragma Import (C, ExportDataAsCode, "ExportDataAsCode");

   function LoadFileText (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Load text data from file (read), returns a '\0' terminated string
   pragma Import (C, LoadFileText, "LoadFileText");

   procedure UnloadFileText (text : Interfaces.C.Strings.chars_ptr);
   --  Unload file text data allocated by LoadFileText()
   pragma Import (C, UnloadFileText, "UnloadFileText");

   function SaveFileText (fileName : Interfaces.C.Strings.chars_ptr; text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Save text data to file (write), string must be '\0' terminated, returns true on success
   pragma Import (C, SaveFileText, "SaveFileText");

   function FileExists (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if file exists
   pragma Import (C, FileExists, "FileExists");

   function DirectoryExists (dirPath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if a directory path exists
   pragma Import (C, DirectoryExists, "DirectoryExists");

   function IsFileExtension (fileName : Interfaces.C.Strings.chars_ptr; ext : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check file extension (including point: .png, .wav)
   pragma Import (C, IsFileExtension, "IsFileExtension");

   function GetFileLength (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
   pragma Import (C, GetFileLength, "GetFileLength");

   function GetFileExtension (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get pointer to extension for a filename string (includes dot: '.png')
   pragma Import (C, GetFileExtension, "GetFileExtension");

   function GetFileName (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get pointer to filename for a path string
   pragma Import (C, GetFileName, "GetFileName");

   function GetFileNameWithoutExt (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get filename string without extension (uses static string)
   pragma Import (C, GetFileNameWithoutExt, "GetFileNameWithoutExt");

   function GetDirectoryPath (filePath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get full path for a given fileName with path (uses static string)
   pragma Import (C, GetDirectoryPath, "GetDirectoryPath");

   function GetPrevDirectoryPath (dirPath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get previous directory path for a given path (uses static string)
   pragma Import (C, GetPrevDirectoryPath, "GetPrevDirectoryPath");

   function GetWorkingDirectory return Interfaces.C.Strings.chars_ptr;
   --  Get current working directory (uses static string)
   pragma Import (C, GetWorkingDirectory, "GetWorkingDirectory");

   function GetApplicationDirectory return Interfaces.C.Strings.chars_ptr;
   --  Get the directory of the running application (uses static string)
   pragma Import (C, GetApplicationDirectory, "GetApplicationDirectory");

   function ChangeDirectory (dir : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Change working directory, return true on success
   pragma Import (C, ChangeDirectory, "ChangeDirectory");

   function IsPathFile (path : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if a given path is a file or a directory
   pragma Import (C, IsPathFile, "IsPathFile");

   function IsFileDropped return Interfaces.C.C_bool;
   --  Check if a file has been dropped into window
   pragma Import (C, IsFileDropped, "IsFileDropped");

   function GetFileModTime (fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.long;
   --  Get file modification time (last write time)
   pragma Import (C, GetFileModTime, "GetFileModTime");

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

   function IsKeyPressed (key : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a key has been pressed once
   pragma Import (C, IsKeyPressed, "IsKeyPressed");

   function IsKeyPressedRepeat (key : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a key has been pressed again (Only PLATFORM_DESKTOP)
   pragma Import (C, IsKeyPressedRepeat, "IsKeyPressedRepeat");

   function IsKeyDown (key : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a key is being pressed
   pragma Import (C, IsKeyDown, "IsKeyDown");

   function IsKeyReleased (key : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a key has been released once
   pragma Import (C, IsKeyReleased, "IsKeyReleased");

   function IsKeyUp (key : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a key is NOT being pressed
   pragma Import (C, IsKeyUp, "IsKeyUp");

   function GetKeyPressed return Interfaces.C.int;
   --  Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
   pragma Import (C, GetKeyPressed, "GetKeyPressed");

   function GetCharPressed return Interfaces.C.int;
   --  Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
   pragma Import (C, GetCharPressed, "GetCharPressed");

   procedure SetExitKey (key : Interfaces.C.int);
   --  Set a custom key to exit program (default is ESC)
   pragma Import (C, SetExitKey, "SetExitKey");

   function IsGamepadAvailable (gamepad : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad is available
   pragma Import (C, IsGamepadAvailable, "IsGamepadAvailable");

   function GetGamepadName (gamepad : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Get gamepad internal name id
   pragma Import (C, GetGamepadName, "GetGamepadName");

   function IsGamepadButtonPressed (gamepad : Interfaces.C.int; button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad button has been pressed once
   pragma Import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");

   function IsGamepadButtonDown (gamepad : Interfaces.C.int; button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad button is being pressed
   pragma Import (C, IsGamepadButtonDown, "IsGamepadButtonDown");

   function IsGamepadButtonReleased (gamepad : Interfaces.C.int; button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad button has been released once
   pragma Import (C, IsGamepadButtonReleased, "IsGamepadButtonReleased");

   function IsGamepadButtonUp (gamepad : Interfaces.C.int; button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a gamepad button is NOT being pressed
   pragma Import (C, IsGamepadButtonUp, "IsGamepadButtonUp");

   function GetGamepadButtonPressed return Interfaces.C.int;
   --  Get the last gamepad button pressed
   pragma Import (C, GetGamepadButtonPressed, "GetGamepadButtonPressed");

   function GetGamepadAxisCount (gamepad : Interfaces.C.int) return Interfaces.C.int;
   --  Get gamepad axis count for a gamepad
   pragma Import (C, GetGamepadAxisCount, "GetGamepadAxisCount");

   function GetGamepadAxisMovement (gamepad : Interfaces.C.int; axis : Interfaces.C.int) return Interfaces.C.C_float;
   --  Get axis movement value for a gamepad axis
   pragma Import (C, GetGamepadAxisMovement, "GetGamepadAxisMovement");

   function SetGamepadMappings (mappings : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Set internal gamepad mappings (SDL_GameControllerDB)
   pragma Import (C, SetGamepadMappings, "SetGamepadMappings");

   procedure SetGamepadVibration (gamepad : Interfaces.C.int; leftMotor : Interfaces.C.C_float; rightMotor : Interfaces.C.C_float);
   --  Set gamepad vibration for both motors
   pragma Import (C, SetGamepadVibration, "SetGamepadVibration");

   function IsMouseButtonPressed (button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a mouse button has been pressed once
   pragma Import (C, IsMouseButtonPressed, "IsMouseButtonPressed");

   function IsMouseButtonDown (button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a mouse button is being pressed
   pragma Import (C, IsMouseButtonDown, "IsMouseButtonDown");

   function IsMouseButtonReleased (button : Interfaces.C.int) return Interfaces.C.C_bool;
   --  Check if a mouse button has been released once
   pragma Import (C, IsMouseButtonReleased, "IsMouseButtonReleased");

   function IsMouseButtonUp (button : Interfaces.C.int) return Interfaces.C.C_bool;
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

   procedure UpdateCamera (camera_p : access Camera3D; mode : Interfaces.C.int);
   --  Update camera position for selected mode
   pragma Import (C, UpdateCamera, "UpdateCamera");

   procedure UpdateCameraPro (camera_p : access Camera3D; movement : Vector3; rotation : Vector3; zoom : Interfaces.C.C_float);
   --  Update camera movement/rotation
   pragma Import (C, UpdateCameraPro, "UpdateCameraPro");

   procedure SetShapesTexture (texture_p : Texture; source : Rectangle);
   --  Set texture and rectangle to be used on shapes drawing
   pragma Import (C, SetShapesTexture, "SetShapesTexture");

   function GetShapesTexture return Texture;
   --  Get texture that is used for shapes drawing
   pragma Import (C, GetShapesTexture, "GetShapesTexture");

   function GetShapesTextureRectangle return Rectangle;
   --  Get texture source rectangle that is used for shapes drawing
   pragma Import (C, GetShapesTextureRectangle, "GetShapesTextureRectangle");

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

   function LoadImageRaw (fileName : Interfaces.C.Strings.chars_ptr; width : Interfaces.C.int; height : Interfaces.C.int; format : Interfaces.C.int; headerSize : Interfaces.C.int) return Image;
   --  Load image from RAW file data
   pragma Import (C, LoadImageRaw, "LoadImageRaw");

   function LoadImageSvg (fileNameOrString : Interfaces.C.Strings.chars_ptr; width : Interfaces.C.int; height : Interfaces.C.int) return Image;
   --  Load image from SVG file data or string with specified size
   pragma Import (C, LoadImageSvg, "LoadImageSvg");

   function LoadImageAnim (fileName : Interfaces.C.Strings.chars_ptr; frames : access Interfaces.C.int) return Image;
   --  Load image sequence from file (frames appended to image.data)
   pragma Import (C, LoadImageAnim, "LoadImageAnim");

   function LoadImageAnimFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int; frames : access Interfaces.C.int) return Image;
   --  Load image sequence from memory buffer
   pragma Import (C, LoadImageAnimFromMemory, "LoadImageAnimFromMemory");

   function LoadImageFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int) return Image;
   --  Load image from memory buffer, fileType refers to extension: i.e. '.png'
   pragma Import (C, LoadImageFromMemory, "LoadImageFromMemory");

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

   function ExportImageToMemory (image_p : Image; fileType : Interfaces.C.Strings.chars_ptr; fileSize : access Interfaces.C.int) return access Interfaces.C.char;
   --  Export image to memory buffer
   pragma Import (C, ExportImageToMemory, "ExportImageToMemory");

   function ExportImageAsCode (image_p : Image; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export image as code file defining an array of bytes, returns true on success
   pragma Import (C, ExportImageAsCode, "ExportImageAsCode");

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

   function ImageCopy (image_p : Image) return Image;
   --  Create an image duplicate (useful for transformations)
   pragma Import (C, ImageCopy, "ImageCopy");

   function ImageFromImage (image_p : Image; rec : Rectangle) return Image;
   --  Create an image from another image piece
   pragma Import (C, ImageFromImage, "ImageFromImage");

   function ImageText (text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.int; color_p : Color) return Image;
   --  Create an image from text (default font)
   pragma Import (C, ImageText, "ImageText");

   function ImageTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) return Image;
   --  Create an image from text (custom sprite font)
   pragma Import (C, ImageTextEx, "ImageTextEx");

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

   procedure ImageKernelConvolution (image_p : access Image; kernel : access Interfaces.C.C_float; kernelSize : Interfaces.C.int);
   --  Apply Custom Square image convolution kernel
   pragma Import (C, ImageKernelConvolution, "ImageKernelConvolution");

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

   procedure ImageDrawTextEx (dst : access Image; font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text (custom sprite font) within an image (destination)
   pragma Import (C, ImageDrawTextEx, "ImageDrawTextEx");

   function LoadTexture (fileName : Interfaces.C.Strings.chars_ptr) return Texture;
   --  Load texture from file into GPU memory (VRAM)
   pragma Import (C, LoadTexture, "LoadTexture");

   function LoadTextureFromImage (image_p : Image) return Texture;
   --  Load texture from image data
   pragma Import (C, LoadTextureFromImage, "LoadTextureFromImage");

   function LoadTextureCubemap (image_p : Image; layout : Interfaces.C.int) return Texture;
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

   procedure SetTextureFilter (texture_p : Texture; filter : Interfaces.C.int);
   --  Set texture scaling filter mode
   pragma Import (C, SetTextureFilter, "SetTextureFilter");

   procedure SetTextureWrap (texture_p : Texture; wrap : Interfaces.C.int);
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

   function ColorIsEqual (col1 : Color; col2 : Color) return Interfaces.C.C_bool;
   --  Check if two colors are equal
   pragma Import (C, ColorIsEqual, "ColorIsEqual");

   function Fade (color_p : Color; alpha : Interfaces.C.C_float) return Color;
   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f
   pragma Import (C, Fade, "Fade");

   function ColorToInt (color_p : Color) return Interfaces.C.int;
   --  Get hexadecimal value for a Color (0xRRGGBBAA)
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
   pragma Import (C, GetColor, "GetColor");

   function GetPixelColor (srcPtr : System.Address; format : Interfaces.C.int) return Color;
   --  Get Color from a source pixel pointer of certain format
   pragma Import (C, GetPixelColor, "GetPixelColor");

   procedure SetPixelColor (dstPtr : System.Address; color_p : Color; format : Interfaces.C.int);
   --  Set color formatted into destination pixel pointer
   pragma Import (C, SetPixelColor, "SetPixelColor");

   function GetPixelDataSize (width : Interfaces.C.int; height : Interfaces.C.int; format : Interfaces.C.int) return Interfaces.C.int;
   --  Get pixel data size in bytes for certain format
   pragma Import (C, GetPixelDataSize, "GetPixelDataSize");

   function GetFontDefault return Font;
   --  Get the default Font
   pragma Import (C, GetFontDefault, "GetFontDefault");

   function LoadFont (fileName : Interfaces.C.Strings.chars_ptr) return Font;
   --  Load font from file into GPU memory (VRAM)
   pragma Import (C, LoadFont, "LoadFont");

   function LoadFontEx (fileName : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character setFont
   pragma Import (C, LoadFontEx, "LoadFontEx");

   function LoadFontFromImage (image_p : Image; key : Color; firstChar : Interfaces.C.int) return Font;
   --  Load font from Image (XNA style)
   pragma Import (C, LoadFontFromImage, "LoadFontFromImage");

   function LoadFontFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font;
   --  Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
   pragma Import (C, LoadFontFromMemory, "LoadFontFromMemory");

   function IsFontReady (font_p : Font) return Interfaces.C.C_bool;
   --  Check if a font is ready
   pragma Import (C, IsFontReady, "IsFontReady");

   function LoadFontData (fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int; type_p : Interfaces.C.int) return access GlyphInfo;
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

   procedure DrawFPS (posX : Interfaces.C.int; posY : Interfaces.C.int);
   --  Draw current FPS
   pragma Import (C, DrawFPS, "DrawFPS");

   procedure DrawText (text : Interfaces.C.Strings.chars_ptr; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color);
   --  Draw text (using default font)
   pragma Import (C, DrawText, "DrawText");

   procedure DrawTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using font and additional parameters
   pragma Import (C, DrawTextEx, "DrawTextEx");

   procedure DrawTextPro (font_p : Font; text : Interfaces.C.Strings.chars_ptr; position : Vector2; origin : Vector2; rotation : Interfaces.C.C_float; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color);
   --  Draw text using Font and pro parameters (rotation)
   pragma Import (C, DrawTextPro, "DrawTextPro");

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

   function MeasureTextEx (font_p : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float) return Vector2;
   --  Measure string size for Font
   pragma Import (C, MeasureTextEx, "MeasureTextEx");

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

   function LoadCodepoints (text : Interfaces.C.Strings.chars_ptr; count : access Interfaces.C.int) return access Interfaces.C.int;
   --  Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
   pragma Import (C, LoadCodepoints, "LoadCodepoints");

   procedure UnloadCodepoints (codepoints : access Interfaces.C.int);
   --  Unload codepoints data from memory
   pragma Import (C, UnloadCodepoints, "UnloadCodepoints");

   function GetCodepointCount (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get total number of codepoints in a UTF-8 encoded string
   pragma Import (C, GetCodepointCount, "GetCodepointCount");

   function GetCodepoint (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepoint, "GetCodepoint");

   function GetCodepointNext (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepointNext, "GetCodepointNext");

   function GetCodepointPrevious (text : Interfaces.C.Strings.chars_ptr; codepointSize : access Interfaces.C.int) return Interfaces.C.int;
   --  Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   pragma Import (C, GetCodepointPrevious, "GetCodepointPrevious");

   function CodepointToUTF8 (codepoint : Interfaces.C.int; utf8Size : access Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Encode one codepoint into UTF-8 byte array (array length returned as parameter)
   pragma Import (C, CodepointToUTF8, "CodepointToUTF8");

   function TextCopy (dst : Interfaces.C.Strings.chars_ptr; src : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Copy one string to another, returns bytes copied
   pragma Import (C, TextCopy, "TextCopy");

   function TextIsEqual (text1 : Interfaces.C.Strings.chars_ptr; text2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Check if two text string are equal
   pragma Import (C, TextIsEqual, "TextIsEqual");

   function TextLength (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.unsigned;
   --  Get text length, checks for '\0' ending
   pragma Import (C, TextLength, "TextLength");

   function TextSubtext (text : Interfaces.C.Strings.chars_ptr; position : Interfaces.C.int; length : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Get a piece of a text string
   pragma Import (C, TextSubtext, "TextSubtext");

   function TextReplace (text : Interfaces.C.Strings.chars_ptr; replace : Interfaces.C.Strings.chars_ptr; by : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Replace text string (WARNING: memory must be freed!)
   pragma Import (C, TextReplace, "TextReplace");

   function TextInsert (text : Interfaces.C.Strings.chars_ptr; insert : Interfaces.C.Strings.chars_ptr; position : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   --  Insert text in a position (WARNING: memory must be freed!)
   pragma Import (C, TextInsert, "TextInsert");

   function TextJoin (textList : access Interfaces.C.Strings.chars_ptr; count : Interfaces.C.int; delimiter : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Join text strings with delimiter
   pragma Import (C, TextJoin, "TextJoin");

   function TextSplit (text : Interfaces.C.Strings.chars_ptr; delimiter : Interfaces.C.char; count : access Interfaces.C.int) return access Interfaces.C.Strings.chars_ptr;
   --  Split text into multiple strings
   pragma Import (C, TextSplit, "TextSplit");

   procedure TextAppend (text : Interfaces.C.Strings.chars_ptr; append : Interfaces.C.Strings.chars_ptr; position : access Interfaces.C.int);
   --  Append text at specific position and move cursor!
   pragma Import (C, TextAppend, "TextAppend");

   function TextFindIndex (text : Interfaces.C.Strings.chars_ptr; find : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Find first text occurrence within a string
   pragma Import (C, TextFindIndex, "TextFindIndex");

   function TextToUpper (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get upper case version of provided string
   pragma Import (C, TextToUpper, "TextToUpper");

   function TextToLower (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get lower case version of provided string
   pragma Import (C, TextToLower, "TextToLower");

   function TextToPascal (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   --  Get Pascal case notation version of provided string
   pragma Import (C, TextToPascal, "TextToPascal");

   function TextToInteger (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
   --  Get integer value from text (negative values not supported)
   pragma Import (C, TextToInteger, "TextToInteger");

   function TextToFloat (text : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_float;
   --  Get float value from text (negative values not supported)
   pragma Import (C, TextToFloat, "TextToFloat");

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

   function GetMeshBoundingBox (mesh_p : Mesh) return BoundingBox;
   --  Compute mesh bounding box limits
   pragma Import (C, GetMeshBoundingBox, "GetMeshBoundingBox");

   procedure GenMeshTangents (mesh_p : access Mesh);
   --  Compute mesh tangents
   pragma Import (C, GenMeshTangents, "GenMeshTangents");

   function ExportMesh (mesh_p : Mesh; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export mesh data to file, returns true on success
   pragma Import (C, ExportMesh, "ExportMesh");

   function ExportMeshAsCode (mesh_p : Mesh; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export mesh as code file (.h) defining multiple arrays of vertex attributes
   pragma Import (C, ExportMeshAsCode, "ExportMeshAsCode");

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

   function LoadWaveFromMemory (fileType : Interfaces.C.Strings.chars_ptr; fileData : System.Address; dataSize : Interfaces.C.int) return Wave;
   --  Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
   pragma Import (C, LoadWaveFromMemory, "LoadWaveFromMemory");

   function IsWaveReady (wave_p : Wave) return Interfaces.C.C_bool;
   --  Checks if wave data is ready
   pragma Import (C, IsWaveReady, "IsWaveReady");

   procedure UnloadWave (wave_p : Wave);
   --  Unload wave data
   pragma Import (C, UnloadWave, "UnloadWave");

   function ExportWave (wave_p : Wave; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export wave data to file, returns true on success
   pragma Import (C, ExportWave, "ExportWave");

   function ExportWaveAsCode (wave_p : Wave; fileName : Interfaces.C.Strings.chars_ptr) return Interfaces.C.C_bool;
   --  Export wave sample data to code (.h), returns true on success
   pragma Import (C, ExportWaveAsCode, "ExportWaveAsCode");

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
