#!/usr/bin/env python3

import json
import re

with open(
    "/home/chouteau/src/github/raylib-ada/raylib/parser/output/raylib_api.json"
) as file:
    data = json.load(file)

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


def to_ada_type(c_type):

    if c_type in TYPE_IDENTITY:
        return c_type
    elif c_type in TYPE_CONVERSION:
        return TYPE_CONVERSION[c_type]
    elif c_type.endswith("*"):
        return "access " + to_ada_type(c_type[:-1].strip())
    else:
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
            f"      {field['name']} : {to_ada_type(field['type'])}; -- {field['description']}"
        )
    print("   end record")
    print("      with Convention => C_Pass_By_Copy;")

    if struct["name"] == "Matrix":
        print("   type Matrix4 is array (0 .. 3) of Matrix;")
        print("   type Matrix2 is array (0 .. 1) of Matrix;")
    elif struct["name"] == "Vector4":
        print("   subtype Quaternion is Vector4;")
    print()

    TYPE_IDENTITY.append(struct["name"])


def gen_enum(enum):
    print(f"   type {enum['name']} is new Interfaces.C.unsigned;")
    print(f"   --  {enum['description']}")
    print()
    for value in enum["values"]:
        print(
            f"   {value['name']} : constant {enum['name']} := {value['value']}; -- {value['description']}"
        )
    print()


def gen_function(function):
    return_type = function["returnType"]

    params = []
    if "params" in function:
        for p in function["params"]:
            if p["name"] in ADA_KEYWORD or is_type_name(p["name"]):
                p["name"] = p["name"] + "_p"
            params += [f"{p['name']} : {to_ada_type(p['type'])}"]

    if len(params) != 0:
        params_str = " (" + "; ".join(params) + ")"
    else:
        params_str = ""

    if return_type == "void":
        print(f"   procedure {function['name']}{params_str};")
    else:
        print(
            f"   function {function['name']}{params_str} return {to_ada_type(return_type)};"
        )
    print(f"   --  {function['description']}")
    print(f"   pragma Import (C, {function['name']}, \"{function['name']}\");")
    print()


def gen_define(define):
    if define["type"] == "COLOR":
        r = re.compile("CLITERAL\(Color\)\{ ([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+) \}")
        r, g, b, a = r.match(define["value"]).groups()
        print(f"   {define['name']} : constant Color := ({r}, {g}, {b}, {a});")


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

SKIP_STRUCTS = ["ModelAnimation", "FilePathList", "AudioStream", "Music", "Sound"]
for struct in data["structs"]:
    if struct["name"] not in SKIP_STRUCTS:
        gen_struct(struct)

for enum in data["enums"]:
    gen_enum(enum)

# for callback in data["callbacks"]:
#     print(callback)

SKIP_FUNCTIONS = [
    "TraceLog",
    "LoadDirectoryFiles",
    "LoadDirectoryFilesEx",
    "UnloadDirectoryFiles",
    "LoadDroppedFiles",
    "UnloadDroppedFiles",
    "LoadAutomationEventList",
    "UnloadAutomationEventList",
    "ExportAutomationEventList",
    "SetAutomationEventList",
    "TextFormat",
    "AttachAudioStreamProcessor",
    "DetachAudioStreamProcessor",
    "AttachAudioMixedProcessor",
    "DetachAudioMixedProcessor",
    "GenImageFontAtlas",
]

for function in data["functions"]:
    if (
        "VrSteree" not in function["name"]
        and "Sound" not in function["name"]
        and "Music" not in function["name"]
        and "Callback" not in function["name"]
        and "ModelAnimation" not in function["name"]
        and "AudioStream" not in function["name"]
        and function["name"] not in SKIP_FUNCTIONS
    ):
        gen_function(function)

for define in data["defines"]:
    gen_define(define)

print("end Raylib;")
