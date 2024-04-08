#!/usr/bin/env python3

import json
import re

with open(
    "/home/chouteau/src/github/raylib-ada/raylib/parser/output/raylib_api.json"
) as file:
    data = json.load(file)

body = ""

ADA_KEYWORD = ["end", "type"]

TYPE_CONVERSION = {
    "char": "Interfaces.C.char",
    "bool": "Interfaces.C.C_bool",
    "float": "Interfaces.C.C_float",
    "double": "Interfaces.C.double",
    "long": "Interfaces.C.long",
    "float *": "access Interfaces.C.C_float",
    "float*": "access Interfaces.C.C_float",
    "int": "Interfaces.C.int",
    "int *": "access Interfaces.C.int",
    "const int *": "access constant Interfaces.C.int",
    "char *": "Interfaces.C.Strings.chars_ptr",
    "const char *": "Interfaces.C.Strings.chars_ptr",
    "unsigned int": "Interfaces.C.unsigned",
    "unsigned int *": "access Interfaces.C.unsigned",
    "unsigned char": "Interfaces.C.unsigned_char",
    "unsigned char *": "access Interfaces.C.char",
    "unsigned short *": "access Interfaces.C.short",
    "void *": "System.Address",
    "const void *": "System.Address",
    "const unsigned char *": "System.Address",
    "Texture2D": "Texture",
    "Camera2D": "Camera2D",
    "Camera3D": "Camera3D",
    "Camera": "Camera3D",
    "RenderTexture2D": "RenderTexture",
    "TextureCubemap": "Texture",
    "float[2]": "Float2",
    "float[4]": "Float4",
    "char[32]": "String32",
    "Matrix[2]": "Matrix2",
    "int[4]": "Int4",
    "const GlyphInfo": "GlyphInfo",
    "const Matrix": "Matrix",
    "void": None,
    "rAudioBuffer *": "System.Address",
    "rAudioProcessor *": "System.Address",
}

TYPE_IDENTITY = [
    "Image",
    "Color",
    "Vector2",
    "Vector3",
    "Vector4",
    "Shader",
    "Texture",
    "Rectangle",
    "Quaternion",
    "Matrix",
    "AudioStream",
    "Transform",
    "RenderTexture",
    "Ray",
    "VrStereoConfig",
    "VrDeviceInfo",
    "AutomationEvent",
    "Font",
    "GlyphInfo",
    "MaterialMap",
    "Material",
    "BoneInfo",
    "NPatchInfo",
    "Mesh",
    "Model",
    "BoundingBox",
    "ModelAnimation",
]


def to_ada_type(c_type, name=None, parent=None):

    # "int" is used for enum values so we have a special handling here where
    # we try to guess if the type should be an enum.
    if name is not None and parent is not None and c_type == "int":
        if name == "mode":
            if "Blend" in parent:
                return "BlendMode"
            elif "Camera" in parent:
                return "CameraMode"
        elif name == "flags" and "Gestures" in parent:
            return "Gesture"
        elif name == "button" and "Mouse" in parent:
            return "MouseButton"
        elif name == "key" and "Key" in parent:
            return "KeyboardKey"
        elif name == "button" and "Gamepad" in parent:
            return "GamepadButton"
        elif name == "projection" and "Camera" in parent:
            return "CameraProjection"
        elif name == "layout":
            if "PatchInfo" in parent:
                return "NPatchLayout"
            elif "Cubemap" in parent:
                return "CubemapLayout"
        elif name == "type_p" and "Font" in parent:
            return "FontType"
        elif name == "wrap" and "Texture" in parent:
            return "TextureWrap"
        elif name == "filter" and "Texture" in parent:
            return "TextureFilter"
        elif name in ["format", "newformat"] and (
            "Image" in parent or "Pixel" in parent
        ):
            return "PixelFormat"
        elif name == "uniformType":
            return "ShaderUniformDataType"
        elif name == "axis" and "Gamepad" in parent:
            return "GamepadAxis"

    if c_type == "int *" and name == "locs":
        return "access ShaderLocationArray"
    elif c_type == "MaterialMap *" and name == "maps":
        return "access MaterialMapArray"
    elif c_type == "char **" and name == "paths":
        return "access constant Interfaces.C.Strings.chars_ptr_array"
    elif c_type == "unsigned int" and parent == "FilePathList":
        return "Interfaces.C.size_t"
    elif c_type == "Transform **" and name == "framePoses":
        return "access Tranform_Array"

    if c_type in TYPE_IDENTITY:
        return c_type
    elif c_type in TYPE_CONVERSION:
        return TYPE_CONVERSION[c_type]
    elif c_type.endswith("*"):
        return "access " + to_ada_type(c_type[:-1].strip())
    else:
        print(TYPE_IDENTITY)
        raise Exception(f"unknown type: '{c_type}'")


def is_type_name(name):
    for type in TYPE_IDENTITY:
        if name.lower() == type.lower():
            return True
    for type in TYPE_CONVERSION.keys():
        if name.lower() == type.lower():
            return True
    return False


def gen_struct(struct):
    print(f"   type {struct['name']} is record")
    for field in struct["fields"]:
        if field["name"] == "type":
            field["name"] = "type_K"

        if is_type_name(field["name"]):
            field["name"] = field["name"] + "_f"
        # print(field)
        print(
            f"      {field['name']} : {to_ada_type(field['type'], field['name'], struct['name'])}; -- {field['description']}"
        )
    print("   end record")
    print("      with Convention => C_Pass_By_Copy;")

    if struct["name"] == "Matrix":
        print("   type Matrix4 is array (0 .. 3) of Matrix;")
        print("   type Matrix2 is array (0 .. 1) of Matrix;")
    elif struct["name"] == "Vector4":
        print("   subtype Quaternion is Vector4;")
    elif struct["name"] == "MaterialMap":
        print("   type MaterialMapArray is array (MaterialMapIndex) of MaterialMap")
        print("     with Convention => C;")
    elif struct["name"] == "Transform":
        print(
            "   type Tranform_Array is array (Interfaces.C.int range <>) of Transform"
        )
        print("     with Convention => C;")

    print()

    TYPE_IDENTITY.append(struct["name"])


def gen_enum(enum):
    if enum["name"].endswith("Flags") or enum["name"] in ["Gesture"]:
        # The enum represents flags that can be combined, so we use a modular type

        print(f"   type {enum['name']} is new Interfaces.C.unsigned;")
        print(f"   --  {enum['description']}")
        print()
        for value in enum["values"]:
            print(
                f"   {value['name']} : constant {enum['name']} := {value['value']}; -- {value['description']}"
            )
    elif enum["name"] in [
        "GamepadAxis",
        "GamepadButton",
        "KeyboardKey",
        "MouseButton",
    ]:
        # In these cases the enum is just used as a list of default/common
        # values for the type.
        print(f"   type {enum['name']} is new Interfaces.C.int;")
        print(f"   --  {enum['description']}")
        print()
        for value in enum["values"]:
            print(
                f"   {value['name']} : constant {enum['name']} := {value['value']}; -- {value['description']}"
            )
    else:
        # Otherwise use regular Ada enums
        print(f"   type {enum['name']} is")
        print("     (")
        sorted_value = sorted(enum["values"], key=lambda d: d["value"])
        first = True
        for value in sorted_value:
            coma = "  " if first else ", "
            print(f"       {coma}{value['name']} -- {value['description']}")
            first = False
        print("     )")
        print("     with Convention => C;")
        print(f"   for {enum['name']} use")
        print("     (")
        first = True
        for value in sorted_value:
            coma = "  " if first else ", "
            print(f"       {coma}{value['name']} => {value['value']}")
            first = False
        print("     );")
    print()


def function_decl(function, spec=True, callback=False):
    out = ""
    param_decls = []
    if function["params"] is not None:
        for p in function["params"]:
            param_decls += [f"{p['name']} : {p['type']}"]

    if len(param_decls) != 0:
        params_str = " (" + "; ".join(param_decls) + ")"
    else:
        params_str = ""

    if callback:
        out += f"   type {function['name']} is access "
        function["name"] = ""

    term = (";" if not callback else "") if spec else " is"
    if function["returnType"] is None:
        out += f"   procedure {function['name']}{params_str}{term}\n"
    else:
        out += f"   function {function['name']}{params_str} return {function['returnType']}{term}\n"
    if spec and not callback:
        out += f"   --  {function['description']}"
    return out


def gen_string_function_body(function):
    out = ""
    out += function_decl(function, spec=False)
    out += "      use Interfaces.C.Strings;\n"
    params = function["params"] if function["params"] is not None else []
    ret_type = function["returnType"]
    ret_str = ret_type == "String"

    call_params = []
    for p in params:
        if p["type"] == "String":
            out += f"      C_{p['name']} : Interfaces.C.Strings.chars_ptr := New_String ({p['name']});\n"
            call_params.append(f"C_{p['name']}")
        else:
            call_params.append(p["name"])

    if len(call_params) > 0:
        call_str = f"{function['name']} ({', '.join(call_params)})"
    else:
        call_str = f"{function['name']}"

    if ret_type:
        if ret_type == "String":
            call_str = f"Value ({call_str})"
        out += f"      Result : constant {ret_type} := {call_str};\n"
    out += "   begin\n"

    if not ret_type:
        out += f"      {call_str};\n"
    for p in params:
        if p["type"] == "String":
            out += f"      Free (C_{p['name']});\n"

    if ret_type:
        out += "      return Result;\n"

    out += f"   end {function['name']};\n\n"

    global body
    body += out


def process_params(params, function_name):
    C_STRING_TYPE = "Interfaces.C.Strings.chars_ptr"
    has_string = False
    for p in params:
        if p["name"] in ADA_KEYWORD or is_type_name(p["name"]):
            p["name"] = p["name"] + "_p"
        p["type"] = to_ada_type(p["type"], p["name"], function_name)
        if p["type"] == C_STRING_TYPE:
            has_string = True
    return params, has_string


def gen_function(function):
    C_STRING_TYPE = "Interfaces.C.Strings.chars_ptr"

    function["returnType"] = to_ada_type(function["returnType"], "RETURNTYPE")

    has_string = function["returnType"] == C_STRING_TYPE
    if "params" in function:
        function["params"], has_string = process_params(
            function["params"], function["name"]
        )
    else:
        function["params"] = None

    print(function_decl(function))
    print(f"   pragma Import (C, {function['name']}, \"{function['name']}\");")
    print()

    if has_string:
        # This sub-program either returns a string or takes a string argument.
        # We write wrapper version that handles conversions between C and Ada String.

        if function["returnType"] == C_STRING_TYPE:
            function["returnType"] = "String"

        if function["params"] is not None:
            for p in function["params"]:
                if p["type"] == C_STRING_TYPE:
                    p["type"] = "String"

        print(function_decl(function))
        gen_string_function_body(function)
        print()


def gen_define(define):
    if define["type"] == "COLOR":
        r = re.compile("CLITERAL\(Color\)\{ ([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+) \}")
        r, g, b, a = r.match(define["value"]).groups()
        print(f"   {define['name']} : constant Color := ({r}, {g}, {b}, {a});")
    elif define["type"] == "STRING":
        print(f"   {define['name']} : constant String := \"{define['value']}\";")
    elif define["type"] == "INT" or define["type"] == "FLOAT":
        print(f"   {define['name']} : constant := {define['value']};")


def gen_callback(callback):
    global TYPE_IDENTITY
    TYPE_IDENTITY.append(callback["name"])

    callback["returnType"] = to_ada_type(callback["returnType"], "RETURNTYPE")
    callback["params"], _ = process_params(callback["params"], callback["name"])
    print(
        function_decl(callback, spec=True, callback=True) + "     with Convention => C;"
    )
    if callback["description"]:
        print(f"   --  {callback['description']}")
    print()


print("with System;")
print("with Interfaces.C;")
print("with Interfaces.C.Strings;")
print("package Raylib")
print("  with Preelaborate")
print("is")
print('   pragma Style_Checks ("M2000");')

print("   type Float2 is array (0 .. 1) of Interfaces.C.C_float;")
print("   type Float4 is array (0 .. 3) of Interfaces.C.C_float;")
print("   type Int4 is array (0 .. 3) of Interfaces.C.int;")
print("   subtype String32 is String (1 .. 32);")

# Do the enums first as the contain definitions used in structs and functions
for enum in data["enums"]:
    gen_enum(enum)

print("   type ShaderLocationArray is array (ShaderLocationIndex) of Interfaces.C.int")
print("     with Convention => C;")
print()

SKIP_CALLBACKS = ["TraceLogCallback"]
for callback in data["callbacks"]:
    if callback["name"] not in SKIP_CALLBACKS:
        gen_callback(callback)

SKIP_STRUCTS = ["ModelAnimation" "Music"]
for struct in data["structs"]:
    if struct["name"] not in SKIP_STRUCTS:
        gen_struct(struct)


SKIP_FUNCTIONS = [
    "LoadAutomationEventList",
    "UnloadAutomationEventList",
    "ExportAutomationEventList",
    "SetAutomationEventList",
    "TextFormat",
    "GenImageFontAtlas",
]

for function in data["functions"]:
    if (
        "VrSteree" not in function["name"]
        and "TraceLog" not in function["name"]
        and "ModelAnimation" not in function["name"]
        and function["name"] not in SKIP_FUNCTIONS
    ):
        gen_function(function)

for define in data["defines"]:
    gen_define(define)

print("end Raylib;")

# print(body)
