with Raylib;
with Resources;
with Examples_Config;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;

procedure Examples is

   package Example_Resources is new Resources (Examples_Config.Crate_Name);
   use Example_Resources;

   screenWidth  : constant := 800;
   screenHeight : constant := 450;

   Cam : aliased Raylib.Camera3D;
   Cube_Position : constant Raylib.Vector3 := (0.0, 0.0, 0.0);
   Cube_Size : constant Raylib.Vector3 := (2.0, 2.0, 2.0);

   Ray : Raylib.Ray := ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0));
   Collision : Raylib.RayCollision :=
     (False, 0.0, (0.0, 0.0, 0.0), (0.0, 0.0, 0.0));

   Model : Raylib.Model;
   Texture : Raylib.Texture;
   Music : Raylib.Music;
   Sound : Raylib.Sound;
begin
   Raylib.InitWindow (screenWidth, screenHeight, "Example window");

   Raylib.InitAudioDevice;

   Model := Raylib.LoadModel (Resource_Path & "/castle.obj");
   Texture := Raylib.LoadTexture (Resource_Path & "/castle_diffuse.png");
   Model.materials.maps (Raylib.MATERIAL_MAP_ALBEDO).texture_f := Texture;

   Music := Raylib.LoadMusicStream
     (Resource_Path & "/S31-The Gears of Progress.ogg");
   Raylib.SetMusicVolume (Music, 0.1);
   Raylib.PlayMusicStream (Music);

   Sound := Raylib.LoadSound (Resource_Path & "/sd_0.wav");

   Cam.position := (10.0, 10.0, 10.0);
   Cam.target := (0.0, 0.0, 0.0);
   Cam.up := (0.0, 1.0, 0.0);
   Cam.fovy := 45.0;
   Cam.projection := Raylib.CAMERA_PERSPECTIVE;

   --  Test LoadDirectoryFiles
   declare
      use Raylib;
      use Interfaces.C.Strings;
      Files : constant FilePathList :=
        LoadDirectoryFiles (Example_Resources.Resource_Path);
   begin
      if Files.count > 0 then
         for X in 0 .. Files.count - 1 loop
            Ada.Text_IO.Put_Line (Value (Files.paths (X)));
         end loop;
      end if;
      UnloadDirectoryFiles (Files);
   end;

   Raylib.DisableCursor;
   Raylib.SetTargetFPS (60);
   while not Raylib.WindowShouldClose loop

      Raylib.UpdateMusicStream (Music);

      if Raylib.IsMouseButtonPressed (Raylib.MOUSE_BUTTON_RIGHT) then
         if Raylib.IsCursorHidden then
            Raylib.EnableCursor;
         else
            Raylib.DisableCursor;
         end if;
      end if;

      if Raylib.IsCursorHidden then
         Raylib.UpdateCamera (Cam'Access, Raylib.CAMERA_FIRST_PERSON);
      else
         if Raylib.IsMouseButtonPressed (Raylib.MOUSE_BUTTON_LEFT) then
            Raylib.PlaySound (Sound);
            if not Collision.hit then
               Ray := Raylib.GetScreenToWorldRay (Raylib.GetMousePosition, Cam);
               Collision := Raylib.GetRayCollisionBox
                 (Ray,
                  ((Cube_Position.x - Cube_Size.x / 2.0,
                   Cube_Position.y - Cube_Size.y / 2.0,
                   Cube_Position.z - Cube_Size.z / 2.0),
                   (Cube_Position.x + Cube_Size.x / 2.0,
                    Cube_Position.y + Cube_Size.y / 2.0,
                    Cube_Position.z + Cube_Size.z / 2.0)));
            else
               Collision.hit := False;
            end if;
         end if;
      end if;

      if Raylib.IsKeyDown (Raylib.KEY_Z) then
         Cam.target := (0.0, 0.0, 0.0);
      end if;

      Raylib.DisableBackfaceCulling;

      Raylib.BeginDrawing;

      Raylib.ClearBackground (Raylib.RAYWHITE);

      Raylib.BeginMode3D (Cam);

      Raylib.DrawModel (Model, (-20.0, 0.0, -20.0), 0.6, Raylib.WHITE);

      Raylib.DrawRay (Ray, Raylib.RED);
      if Collision.hit then
         Raylib.DrawCube (Cube_Position,
                          Cube_Size.x, Cube_Size.y, Cube_Size.z,
                          Raylib.RED);
         Raylib.DrawCubeWires (Cube_Position,
                               Cube_Size.x, Cube_Size.y, Cube_Size.z,
                               Raylib.MAROON);
         Raylib.DrawCubeWires (Cube_Position,
                               Cube_Size.x + 0.2,
                               Cube_Size.y + 0.2,
                               Cube_Size.z + 0.2,
                               Raylib.GREEN);
      else
         Raylib.DrawCube (Cube_Position,
                          Cube_Size.x, Cube_Size.y, Cube_Size.z,
                          Raylib.GRAY);
         Raylib.DrawCubeWires (Cube_Position,
                               Cube_Size.x, Cube_Size.y, Cube_Size.z,
                               Raylib.DARKGRAY);
      end if;

      Raylib.DrawGrid (10, 1.0);

      Raylib.EndMode3D;

      Raylib.DrawText ("Try clicking on the box with your mouse!",
                       240, 10, 20, Raylib.DARKGRAY);

      if Collision.hit then
         Raylib.DrawText
           ("BOX SELECTED",
            (screenWidth - Raylib.MeasureText ("BOX SELECTED", 30)) / 2,
            int (screenHeight * 0.1), 30,
            Raylib.GREEN);
      end if;

      Raylib.DrawText ("Right click mouse to toggle camera controls",
                       10, 430, 10, Raylib.GRAY);

      Raylib.DrawFPS (10, 10);
      Raylib.EndDrawing;
   end loop;

   Raylib.UnloadMusicStream (Music);
   Raylib.UnloadSound (Sound);

   Raylib.CloseWindow;
end Examples;
