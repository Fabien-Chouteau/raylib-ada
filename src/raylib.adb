package body Raylib is
   pragma Style_Checks ("M2000");
   procedure InitWindow (width : Interfaces.C.int; height : Interfaces.C.int; title : String) is
      use Interfaces.C.Strings;
      C_title : Interfaces.C.Strings.chars_ptr := New_String (title);
   begin
      InitWindow (width, height, C_title);
      Free (C_title);
   end InitWindow;

   procedure SetWindowTitle (title : String) is
      use Interfaces.C.Strings;
      C_title : Interfaces.C.Strings.chars_ptr := New_String (title);
   begin
      SetWindowTitle (C_title);
      Free (C_title);
   end SetWindowTitle;

   procedure SetClipboardText (text : String) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      SetClipboardText (C_text);
      Free (C_text);
   end SetClipboardText;

   function GetClipboardText return String is
      use Interfaces.C.Strings;
      Result : constant String := Value (GetClipboardText);
   begin
      return Result;
   end GetClipboardText;

   function LoadShader (vsFileName : String; fsFileName : String) return Shader is
      use Interfaces.C.Strings;
      C_vsFileName : Interfaces.C.Strings.chars_ptr := New_String (vsFileName);
      C_fsFileName : Interfaces.C.Strings.chars_ptr := New_String (fsFileName);
      Result : constant Shader := LoadShader (C_vsFileName, C_fsFileName);
   begin
      Free (C_vsFileName);
      Free (C_fsFileName);
      return Result;
   end LoadShader;

   function LoadShaderFromMemory (vsCode : String; fsCode : String) return Shader is
      use Interfaces.C.Strings;
      C_vsCode : Interfaces.C.Strings.chars_ptr := New_String (vsCode);
      C_fsCode : Interfaces.C.Strings.chars_ptr := New_String (fsCode);
      Result : constant Shader := LoadShaderFromMemory (C_vsCode, C_fsCode);
   begin
      Free (C_vsCode);
      Free (C_fsCode);
      return Result;
   end LoadShaderFromMemory;

   function GetShaderLocation (shader_p : Shader; uniformName : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_uniformName : Interfaces.C.Strings.chars_ptr := New_String (uniformName);
      Result : constant Interfaces.C.int := GetShaderLocation (shader_p, C_uniformName);
   begin
      Free (C_uniformName);
      return Result;
   end GetShaderLocation;

   function GetShaderLocationAttrib (shader_p : Shader; attribName : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_attribName : Interfaces.C.Strings.chars_ptr := New_String (attribName);
      Result : constant Interfaces.C.int := GetShaderLocationAttrib (shader_p, C_attribName);
   begin
      Free (C_attribName);
      return Result;
   end GetShaderLocationAttrib;

   procedure TakeScreenshot (fileName : String) is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
   begin
      TakeScreenshot (C_fileName);
      Free (C_fileName);
   end TakeScreenshot;

   procedure OpenURL (url : String) is
      use Interfaces.C.Strings;
      C_url : Interfaces.C.Strings.chars_ptr := New_String (url);
   begin
      OpenURL (C_url);
      Free (C_url);
   end OpenURL;

   function LoadFileData (fileName : String; dataSize : access Interfaces.C.int) return access Interfaces.C.char is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant access Interfaces.C.char := LoadFileData (C_fileName, dataSize);
   begin
      Free (C_fileName);
      return Result;
   end LoadFileData;

   function SaveFileData (fileName : String; data : System.Address; dataSize : Interfaces.C.int) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := SaveFileData (C_fileName, data, dataSize);
   begin
      Free (C_fileName);
      return Result;
   end SaveFileData;

   function ExportDataAsCode (data : System.Address; dataSize : Interfaces.C.int; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportDataAsCode (data, dataSize, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportDataAsCode;

   function LoadFileText (fileName : String) return String is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant String := Value (LoadFileText (C_fileName));
   begin
      Free (C_fileName);
      return Result;
   end LoadFileText;

   procedure UnloadFileText (text : String) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      UnloadFileText (C_text);
      Free (C_text);
   end UnloadFileText;

   function SaveFileText (fileName : String; text : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.C_bool := SaveFileText (C_fileName, C_text);
   begin
      Free (C_fileName);
      Free (C_text);
      return Result;
   end SaveFileText;

   function FileExists (fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := FileExists (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end FileExists;

   function DirectoryExists (dirPath : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_dirPath : Interfaces.C.Strings.chars_ptr := New_String (dirPath);
      Result : constant Interfaces.C.C_bool := DirectoryExists (C_dirPath);
   begin
      Free (C_dirPath);
      return Result;
   end DirectoryExists;

   function IsFileExtension (fileName : String; ext : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      C_ext : Interfaces.C.Strings.chars_ptr := New_String (ext);
      Result : constant Interfaces.C.C_bool := IsFileExtension (C_fileName, C_ext);
   begin
      Free (C_fileName);
      Free (C_ext);
      return Result;
   end IsFileExtension;

   function GetFileLength (fileName : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.int := GetFileLength (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end GetFileLength;

   function GetFileExtension (fileName : String) return String is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant String := Value (GetFileExtension (C_fileName));
   begin
      Free (C_fileName);
      return Result;
   end GetFileExtension;

   function GetFileName (filePath : String) return String is
      use Interfaces.C.Strings;
      C_filePath : Interfaces.C.Strings.chars_ptr := New_String (filePath);
      Result : constant String := Value (GetFileName (C_filePath));
   begin
      Free (C_filePath);
      return Result;
   end GetFileName;

   function GetFileNameWithoutExt (filePath : String) return String is
      use Interfaces.C.Strings;
      C_filePath : Interfaces.C.Strings.chars_ptr := New_String (filePath);
      Result : constant String := Value (GetFileNameWithoutExt (C_filePath));
   begin
      Free (C_filePath);
      return Result;
   end GetFileNameWithoutExt;

   function GetDirectoryPath (filePath : String) return String is
      use Interfaces.C.Strings;
      C_filePath : Interfaces.C.Strings.chars_ptr := New_String (filePath);
      Result : constant String := Value (GetDirectoryPath (C_filePath));
   begin
      Free (C_filePath);
      return Result;
   end GetDirectoryPath;

   function GetPrevDirectoryPath (dirPath : String) return String is
      use Interfaces.C.Strings;
      C_dirPath : Interfaces.C.Strings.chars_ptr := New_String (dirPath);
      Result : constant String := Value (GetPrevDirectoryPath (C_dirPath));
   begin
      Free (C_dirPath);
      return Result;
   end GetPrevDirectoryPath;

   function GetWorkingDirectory return String is
      use Interfaces.C.Strings;
      Result : constant String := Value (GetWorkingDirectory);
   begin
      return Result;
   end GetWorkingDirectory;

   function GetApplicationDirectory return String is
      use Interfaces.C.Strings;
      Result : constant String := Value (GetApplicationDirectory);
   begin
      return Result;
   end GetApplicationDirectory;

   function ChangeDirectory (dir : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_dir : Interfaces.C.Strings.chars_ptr := New_String (dir);
      Result : constant Interfaces.C.C_bool := ChangeDirectory (C_dir);
   begin
      Free (C_dir);
      return Result;
   end ChangeDirectory;

   function IsPathFile (path : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_path : Interfaces.C.Strings.chars_ptr := New_String (path);
      Result : constant Interfaces.C.C_bool := IsPathFile (C_path);
   begin
      Free (C_path);
      return Result;
   end IsPathFile;

   function LoadDirectoryFiles (dirPath : String) return FilePathList is
      use Interfaces.C.Strings;
      C_dirPath : Interfaces.C.Strings.chars_ptr := New_String (dirPath);
      Result : constant FilePathList := LoadDirectoryFiles (C_dirPath);
   begin
      Free (C_dirPath);
      return Result;
   end LoadDirectoryFiles;

   function LoadDirectoryFilesEx (basePath : String; filter : String; scanSubdirs : Interfaces.C.C_bool) return FilePathList is
      use Interfaces.C.Strings;
      C_basePath : Interfaces.C.Strings.chars_ptr := New_String (basePath);
      C_filter : Interfaces.C.Strings.chars_ptr := New_String (filter);
      Result : constant FilePathList := LoadDirectoryFilesEx (C_basePath, C_filter, scanSubdirs);
   begin
      Free (C_basePath);
      Free (C_filter);
      return Result;
   end LoadDirectoryFilesEx;

   function GetFileModTime (fileName : String) return Interfaces.C.long is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.long := GetFileModTime (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end GetFileModTime;

   function LoadAutomationEventList (fileName : String) return AutomationEventList is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant AutomationEventList := LoadAutomationEventList (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadAutomationEventList;

   function ExportAutomationEventList (list : AutomationEventList; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportAutomationEventList (list, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportAutomationEventList;

   function SetGamepadMappings (mappings : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_mappings : Interfaces.C.Strings.chars_ptr := New_String (mappings);
      Result : constant Interfaces.C.int := SetGamepadMappings (C_mappings);
   begin
      Free (C_mappings);
      return Result;
   end SetGamepadMappings;

   function LoadImage (fileName : String) return Image is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Image := LoadImage (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadImage;

   function LoadImageRaw (fileName : String; width : Interfaces.C.int; height : Interfaces.C.int; format : PixelFormat; headerSize : Interfaces.C.int) return Image is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Image := LoadImageRaw (C_fileName, width, height, format, headerSize);
   begin
      Free (C_fileName);
      return Result;
   end LoadImageRaw;

   function LoadImageSvg (fileNameOrString : String; width : Interfaces.C.int; height : Interfaces.C.int) return Image is
      use Interfaces.C.Strings;
      C_fileNameOrString : Interfaces.C.Strings.chars_ptr := New_String (fileNameOrString);
      Result : constant Image := LoadImageSvg (C_fileNameOrString, width, height);
   begin
      Free (C_fileNameOrString);
      return Result;
   end LoadImageSvg;

   function LoadImageAnim (fileName : String; frames : access Interfaces.C.int) return Image is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Image := LoadImageAnim (C_fileName, frames);
   begin
      Free (C_fileName);
      return Result;
   end LoadImageAnim;

   function LoadImageFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int) return Image is
      use Interfaces.C.Strings;
      C_fileType : Interfaces.C.Strings.chars_ptr := New_String (fileType);
      Result : constant Image := LoadImageFromMemory (C_fileType, fileData, dataSize);
   begin
      Free (C_fileType);
      return Result;
   end LoadImageFromMemory;

   function ExportImage (image_p : Image; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportImage (image_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportImage;

   function ExportImageToMemory (image_p : Image; fileType : String; fileSize : access Interfaces.C.int) return access Interfaces.C.char is
      use Interfaces.C.Strings;
      C_fileType : Interfaces.C.Strings.chars_ptr := New_String (fileType);
      Result : constant access Interfaces.C.char := ExportImageToMemory (image_p, C_fileType, fileSize);
   begin
      Free (C_fileType);
      return Result;
   end ExportImageToMemory;

   function ExportImageAsCode (image_p : Image; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportImageAsCode (image_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportImageAsCode;

   function GenImageText (width : Interfaces.C.int; height : Interfaces.C.int; text : String) return Image is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Image := GenImageText (width, height, C_text);
   begin
      Free (C_text);
      return Result;
   end GenImageText;

   function ImageText (text : String; fontSize : Interfaces.C.int; color_p : Color) return Image is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Image := ImageText (C_text, fontSize, color_p);
   begin
      Free (C_text);
      return Result;
   end ImageText;

   function ImageTextEx (font_p : Font; text : String; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) return Image is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Image := ImageTextEx (font_p, C_text, fontSize, spacing, tint);
   begin
      Free (C_text);
      return Result;
   end ImageTextEx;

   procedure ImageDrawText (dst : access Image; text : String; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      ImageDrawText (dst, C_text, posX, posY, fontSize, color_p);
      Free (C_text);
   end ImageDrawText;

   procedure ImageDrawTextEx (dst : access Image; font_p : Font; text : String; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      ImageDrawTextEx (dst, font_p, C_text, position, fontSize, spacing, tint);
      Free (C_text);
   end ImageDrawTextEx;

   function LoadTexture (fileName : String) return Texture is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Texture := LoadTexture (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadTexture;

   function LoadFont (fileName : String) return Font is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Font := LoadFont (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadFont;

   function LoadFontEx (fileName : String; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Font := LoadFontEx (C_fileName, fontSize, codepoints, codepointCount);
   begin
      Free (C_fileName);
      return Result;
   end LoadFontEx;

   function LoadFontFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int; fontSize : Interfaces.C.int; codepoints : access Interfaces.C.int; codepointCount : Interfaces.C.int) return Font is
      use Interfaces.C.Strings;
      C_fileType : Interfaces.C.Strings.chars_ptr := New_String (fileType);
      Result : constant Font := LoadFontFromMemory (C_fileType, fileData, dataSize, fontSize, codepoints, codepointCount);
   begin
      Free (C_fileType);
      return Result;
   end LoadFontFromMemory;

   function ExportFontAsCode (font_p : Font; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportFontAsCode (font_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportFontAsCode;

   procedure DrawText (text : String; posX : Interfaces.C.int; posY : Interfaces.C.int; fontSize : Interfaces.C.int; color_p : Color) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      DrawText (C_text, posX, posY, fontSize, color_p);
      Free (C_text);
   end DrawText;

   procedure DrawTextEx (font_p : Font; text : String; position : Vector2; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      DrawTextEx (font_p, C_text, position, fontSize, spacing, tint);
      Free (C_text);
   end DrawTextEx;

   procedure DrawTextPro (font_p : Font; text : String; position : Vector2; origin : Vector2; rotation : Interfaces.C.C_float; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float; tint : Color) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      DrawTextPro (font_p, C_text, position, origin, rotation, fontSize, spacing, tint);
      Free (C_text);
   end DrawTextPro;

   function MeasureText (text : String; fontSize : Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := MeasureText (C_text, fontSize);
   begin
      Free (C_text);
      return Result;
   end MeasureText;

   function MeasureTextEx (font_p : Font; text : String; fontSize : Interfaces.C.C_float; spacing : Interfaces.C.C_float) return Vector2 is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Vector2 := MeasureTextEx (font_p, C_text, fontSize, spacing);
   begin
      Free (C_text);
      return Result;
   end MeasureTextEx;

   procedure UnloadUTF8 (text : String) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
   begin
      UnloadUTF8 (C_text);
      Free (C_text);
   end UnloadUTF8;

   function LoadCodepoints (text : String; count : access Interfaces.C.int) return access Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant access Interfaces.C.int := LoadCodepoints (C_text, count);
   begin
      Free (C_text);
      return Result;
   end LoadCodepoints;

   function GetCodepointCount (text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GetCodepointCount (C_text);
   begin
      Free (C_text);
      return Result;
   end GetCodepointCount;

   function GetCodepoint (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GetCodepoint (C_text, codepointSize);
   begin
      Free (C_text);
      return Result;
   end GetCodepoint;

   function GetCodepointNext (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GetCodepointNext (C_text, codepointSize);
   begin
      Free (C_text);
      return Result;
   end GetCodepointNext;

   function GetCodepointPrevious (text : String; codepointSize : access Interfaces.C.int) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := GetCodepointPrevious (C_text, codepointSize);
   begin
      Free (C_text);
      return Result;
   end GetCodepointPrevious;

   function TextCopy (dst : String; src : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_dst : Interfaces.C.Strings.chars_ptr := New_String (dst);
      C_src : Interfaces.C.Strings.chars_ptr := New_String (src);
      Result : constant Interfaces.C.int := TextCopy (C_dst, C_src);
   begin
      Free (C_dst);
      Free (C_src);
      return Result;
   end TextCopy;

   function TextIsEqual (text1 : String; text2 : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_text1 : Interfaces.C.Strings.chars_ptr := New_String (text1);
      C_text2 : Interfaces.C.Strings.chars_ptr := New_String (text2);
      Result : constant Interfaces.C.C_bool := TextIsEqual (C_text1, C_text2);
   begin
      Free (C_text1);
      Free (C_text2);
      return Result;
   end TextIsEqual;

   function TextLength (text : String) return Interfaces.C.unsigned is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.unsigned := TextLength (C_text);
   begin
      Free (C_text);
      return Result;
   end TextLength;

   function TextSubtext (text : String; position : Interfaces.C.int; length : Interfaces.C.int) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant String := Value (TextSubtext (C_text, position, length));
   begin
      Free (C_text);
      return Result;
   end TextSubtext;

   function TextReplace (text : String; replace : String; by : String) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      C_replace : Interfaces.C.Strings.chars_ptr := New_String (replace);
      C_by : Interfaces.C.Strings.chars_ptr := New_String (by);
      Result : constant String := Value (TextReplace (C_text, C_replace, C_by));
   begin
      Free (C_text);
      Free (C_replace);
      Free (C_by);
      return Result;
   end TextReplace;

   function TextInsert (text : String; insert : String; position : Interfaces.C.int) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      C_insert : Interfaces.C.Strings.chars_ptr := New_String (insert);
      Result : constant String := Value (TextInsert (C_text, C_insert, position));
   begin
      Free (C_text);
      Free (C_insert);
      return Result;
   end TextInsert;

   function TextJoin (textList : access Interfaces.C.Strings.chars_ptr; count : Interfaces.C.int; delimiter : String) return String is
      use Interfaces.C.Strings;
      C_delimiter : Interfaces.C.Strings.chars_ptr := New_String (delimiter);
      Result : constant String := Value (TextJoin (textList, count, C_delimiter));
   begin
      Free (C_delimiter);
      return Result;
   end TextJoin;

   function TextSplit (text : String; delimiter : Interfaces.C.char; count : access Interfaces.C.int) return access Interfaces.C.Strings.chars_ptr is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant access Interfaces.C.Strings.chars_ptr := TextSplit (C_text, delimiter, count);
   begin
      Free (C_text);
      return Result;
   end TextSplit;

   procedure TextAppend (text : String; append : String; position : access Interfaces.C.int) is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      C_append : Interfaces.C.Strings.chars_ptr := New_String (append);
   begin
      TextAppend (C_text, C_append, position);
      Free (C_text);
      Free (C_append);
   end TextAppend;

   function TextFindIndex (text : String; find : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      C_find : Interfaces.C.Strings.chars_ptr := New_String (find);
      Result : constant Interfaces.C.int := TextFindIndex (C_text, C_find);
   begin
      Free (C_text);
      Free (C_find);
      return Result;
   end TextFindIndex;

   function TextToUpper (text : String) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant String := Value (TextToUpper (C_text));
   begin
      Free (C_text);
      return Result;
   end TextToUpper;

   function TextToLower (text : String) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant String := Value (TextToLower (C_text));
   begin
      Free (C_text);
      return Result;
   end TextToLower;

   function TextToPascal (text : String) return String is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant String := Value (TextToPascal (C_text));
   begin
      Free (C_text);
      return Result;
   end TextToPascal;

   function TextToInteger (text : String) return Interfaces.C.int is
      use Interfaces.C.Strings;
      C_text : Interfaces.C.Strings.chars_ptr := New_String (text);
      Result : constant Interfaces.C.int := TextToInteger (C_text);
   begin
      Free (C_text);
      return Result;
   end TextToInteger;

   function LoadModel (fileName : String) return Model is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Model := LoadModel (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadModel;

   function ExportMesh (mesh_p : Mesh; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportMesh (mesh_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportMesh;

   function LoadMaterials (fileName : String; materialCount : access Interfaces.C.int) return access Material is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant access Material := LoadMaterials (C_fileName, materialCount);
   begin
      Free (C_fileName);
      return Result;
   end LoadMaterials;

   function LoadModelAnimations (fileName : String; animCount : access Interfaces.C.int) return access ModelAnimation_Array is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant access ModelAnimation_Array := LoadModelAnimations (C_fileName, animCount);
   begin
      Free (C_fileName);
      return Result;
   end LoadModelAnimations;

   function LoadWave (fileName : String) return Wave is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Wave := LoadWave (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadWave;

   function LoadWaveFromMemory (fileType : String; fileData : System.Address; dataSize : Interfaces.C.int) return Wave is
      use Interfaces.C.Strings;
      C_fileType : Interfaces.C.Strings.chars_ptr := New_String (fileType);
      Result : constant Wave := LoadWaveFromMemory (C_fileType, fileData, dataSize);
   begin
      Free (C_fileType);
      return Result;
   end LoadWaveFromMemory;

   function LoadSound (fileName : String) return Sound is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Sound := LoadSound (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadSound;

   function ExportWave (wave_p : Wave; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportWave (wave_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportWave;

   function ExportWaveAsCode (wave_p : Wave; fileName : String) return Interfaces.C.C_bool is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Interfaces.C.C_bool := ExportWaveAsCode (wave_p, C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end ExportWaveAsCode;

   function LoadMusicStream (fileName : String) return Music is
      use Interfaces.C.Strings;
      C_fileName : Interfaces.C.Strings.chars_ptr := New_String (fileName);
      Result : constant Music := LoadMusicStream (C_fileName);
   begin
      Free (C_fileName);
      return Result;
   end LoadMusicStream;

   function LoadMusicStreamFromMemory (fileType : String; data : System.Address; dataSize : Interfaces.C.int) return Music is
      use Interfaces.C.Strings;
      C_fileType : Interfaces.C.Strings.chars_ptr := New_String (fileType);
      Result : constant Music := LoadMusicStreamFromMemory (C_fileType, data, dataSize);
   begin
      Free (C_fileType);
      return Result;
   end LoadMusicStreamFromMemory;

end Raylib;
