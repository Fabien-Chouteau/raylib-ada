with Raylib;
with Resources;
with Examples_Config;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

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

   Str1 : constant chars_ptr :=
     New_String ("Try clicking on the box with your mouse!");
   Str2 : constant chars_ptr :=
     New_String ("Right click mouse to toggle camera controls");
   Selected_Str : constant chars_ptr := New_String ("BOX SELECTED");

   Model : Raylib.Model;
   Texture : Raylib.Texture;
begin

   Raylib.InitWindow (screenWidth, screenHeight,
                      New_String ("Example window"));

   Model := Raylib.LoadModel (New_String (Resource_Path & "/castle.obj"));
   Texture :=
     Raylib.LoadTexture (New_String (Resource_Path & "/castle_diffuse.png"));
   Model.materials.maps.texture_f := Texture;

   Cam.position := (10.0, 10.0, 10.0);
   Cam.target := (0.0, 0.0, 0.0);
   Cam.up := (0.0, 1.0, 0.0);
   Cam.fovy := 45.0;
   Cam.projection := int (Raylib.CAMERA_PERSPECTIVE);

   Raylib.DisableCursor;
   Raylib.SetTargetFPS (60);
   while not Raylib.WindowShouldClose loop

      if Raylib.IsCursorHidden then
         Raylib.UpdateCamera (Cam'Access, int (Raylib.CAMERA_FIRST_PERSON));
      end if;

      if Raylib.IsMouseButtonPressed (int (Raylib.MOUSE_BUTTON_RIGHT)) then
         if Raylib.IsCursorHidden then
            Raylib.EnableCursor;
         else
            Raylib.DisableCursor;
         end if;
      end if;

      if Raylib.IsMouseButtonPressed (int (Raylib.MOUSE_BUTTON_LEFT)) then
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

      if Raylib.IsKeyDown (int (Raylib.KEY_Z)) then
         Cam.target := (0.0, 0.0, 0.0);
      end if;

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

      Raylib.DrawText (Str1, 240, 10, 20, Raylib.DARKGRAY);

      if Collision.hit then
         Raylib.DrawText
           (Selected_Str,
            (screenWidth - Raylib.MeasureText (Selected_Str, 30)) / 2,
            int (screenHeight * 0.1), 30,
            Raylib.GREEN);
      end if;

      Raylib.DrawText (Str2, 10, 430, 10, Raylib.GRAY);

      Raylib.DrawFPS (10, 10);
      Raylib.EndDrawing;
   end loop;

   Raylib.CloseWindow;
end Examples;
