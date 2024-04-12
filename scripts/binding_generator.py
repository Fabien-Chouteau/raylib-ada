#!/usr/bin/env python3

import json
import re
import os

import os

CRATE_ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")

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
    "unsigned short": "Interfaces.C.short",
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

TYPE_IDENTITY = ["Quaternion"]


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
        elif parent in ["GuiSetStyle", "GuiGetStyle"] and name == "control":
            return "GuiControl"

    if c_type == "int *" and name == "locs":
        return "access ShaderLocationArray"
    elif c_type == "MaterialMap *" and name == "maps":
        return "access MaterialMapArray"
    elif c_type == "char **" and name == "paths":
        return "access constant C_String_Array"
    elif c_type == "const char **" and name == "text" and parent == "GuiListViewEx":
        return "C_String_Array_Access"
    elif c_type == "Transform **" and name == "framePoses":
        return "access Tranform_Array"
    elif c_type == "AutomationEvent *" and name == "events":
        return "access AutomationEvent_Array"
    elif c_type == "ModelAnimation *" and name in ["animations", "RETURNTYPE"]:
        return "access ModelAnimation_Array"

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

    out = f"   type {struct['name']} is record\n"
    for field in struct["fields"]:
        if field["name"] == "type":
            field["name"] = "type_K"

        if is_type_name(field["name"]):
            field["name"] = field["name"] + "_f"
        desc = field["description"].strip()
        desc = f" -- {desc}" if desc != "" else ""
        out += f"      {field['name']} : {to_ada_type(field['type'], field['name'], struct['name'])};{desc}\n"
    out += "   end record\n"
    out += "      with Convention => C_Pass_By_Copy;\n"

    if struct["name"] == "Matrix":
        out += "   type Matrix4 is array (0 .. 3) of Matrix;\n"
        out += "   type Matrix2 is array (0 .. 1) of Matrix;\n"
    elif struct["name"] == "Vector4":
        out += "   subtype Quaternion is Vector4;\n"
    elif struct["name"] == "MaterialMap":
        out += "   type MaterialMapArray is array (MaterialMapIndex) of MaterialMap\n"
        out += "     with Convention => C;\n"
    elif struct["name"] == "Transform":
        out += "   type Tranform_Array is array (Interfaces.C.int) of Transform\n"
        out += "     with Convention => C;\n"
    elif struct["name"] == "AutomationEvent":
        out += "   type AutomationEvent_Array is array (Interfaces.C.unsigned) of AutomationEvent\n"

        out += "     with Convention => C;\n"
    elif struct["name"] == "ModelAnimation":
        out += "   type ModelAnimation_Array is array (Interfaces.C.unsigned) of ModelAnimation\n"
        out += "     with Convention => C;\n"

    out += "\n"

    TYPE_IDENTITY.append(struct["name"])

    return out


def enum_kind(name):
    if name.endswith("Flags") or name in ["Gesture"]:
        return "unsigned_new_type"
    elif (
        name.startswith("Gui")
        and name.endswith("Property")
        or name in ["GuiTextAlignment", "GuiTextAlignmentVertical", "GuiTextWrapMode"]
    ):
        return "int_subtype"
    elif name in [
        "GamepadAxis",
        "GamepadButton",
        "KeyboardKey",
        "MouseButton",
    ]:
        return "int_new_type"
    else:
        return "ada_enums"


def gen_enum(enum):
    out = ""
    kind = enum_kind(enum["name"])

    if kind == "unsigned_new_type":
        # The enum represents flags that can be combined, so we use a modular type
        out += f"   type {enum['name']} is new Interfaces.C.unsigned;\n"
    elif kind == "int_subtype":
        out += f"   subtype {enum['name']} is Interfaces.C.int;\n"
    elif kind == "int_new_type":
        # In these cases the enum is just used as a list of default/common
        # values for the type.
        out += f"   type {enum['name']} is new Interfaces.C.int;\n"

    if kind in ["unsigned_new_type", "int_subtype", "int_new_type"]:
        if len(enum["description"].strip()) != 0:
            out += f"   --  {enum['description']}\n"
        out += "\n"

        for value in enum["values"]:
            desc = (
                ""
                if len(value["description"].strip()) == 0
                else f" -- {value['description']}"
            )
            out += f"   {value['name']} : constant {enum['name']} := {value['value']};{desc}\n"
    else:
        # Otherwise use regular Ada enums
        out += f"   type {enum['name']} is\n"
        out += "     (\n"
        sorted_value = sorted(enum["values"], key=lambda d: d["value"])
        first = True
        for value in sorted_value:
            coma = "  " if first else ", "
            desc = (
                ""
                if len(value["description"].strip()) == 0
                else f" -- {value['description']}"
            )
            out += f"       {coma}{value['name']}{desc}\n"
            first = False
        out += "     )\n"
        out += "     with Convention => C;\n"
        if len(enum["description"].strip()) != 0:
            out += f"   --  {enum['description']}\n"
        out += "\n"

        out += f"   for {enum['name']} use\n"
        out += "     (\n"
        first = True
        for value in sorted_value:
            coma = "  " if first else ", "
            out += f"       {coma}{value['name']} => {value['value']}\n"
            first = False
        out += "     );\n"
    out += "\n"
    return out


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
        out += f"   --  {function['description']}\n"
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

    return out


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


def GUI_string_exception(param, function):
    """
    Return true is the parameter should not be converted to an Ada String.
    This is required for some GUI function that modify the string.
    """
    return function["name"] == "GuiTextInputBox" and param["name"] == "text"


def gen_function(function):
    spec = ""
    body = ""
    C_STRING_TYPE = "Interfaces.C.Strings.chars_ptr"

    function["returnType"] = to_ada_type(function["returnType"], "RETURNTYPE")

    has_string = function["returnType"] == C_STRING_TYPE
    if "params" in function:
        function["params"], has_string = process_params(
            function["params"], function["name"]
        )
    else:
        function["params"] = None

    spec += function_decl(function)
    if function["name"] == "GetColor":
        # Declare a variant of get color that will work with the output of GuiGetStyle
        spec += "   function GetColor (hexValue : Interfaces.C.int) return Color;\n"
    spec += f"   pragma Import (C, {function['name']}, \"{function['name']}\");\n\n"

    if has_string:
        # This sub-program either returns a string or takes a string argument.
        # We write wrapper version that handles conversions between C and Ada String.

        if function["returnType"] == C_STRING_TYPE:
            function["returnType"] = "String"

        if function["params"] is not None:
            for p in function["params"]:
                if p["type"] == C_STRING_TYPE and not GUI_string_exception(p, function):
                    p["type"] = "String"

        spec += function_decl(function) + "\n"
        body += gen_string_function_body(function)
    return (spec, body)


def gen_define(define):
    out = ""
    if define["type"] == "COLOR":
        r = re.compile("CLITERAL\(Color\)\{ ([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+) \}")
        r, g, b, a = r.match(define["value"]).groups()
        out += f"   {define['name']} : constant Color := ({r}, {g}, {b}, {a});\n"
    elif define["type"] == "STRING":
        out += f"   {define['name']} : constant String := \"{define['value']}\";\n"
    elif define["type"] == "INT" or define["type"] == "FLOAT":
        out += f"   {define['name']} : constant := {define['value']};\n"

    return out


def gen_callback(callback):
    global TYPE_IDENTITY
    TYPE_IDENTITY.append(callback["name"])

    out = ""
    callback["returnType"] = to_ada_type(callback["returnType"], "RETURNTYPE")
    callback["params"], _ = process_params(callback["params"], callback["name"])
    out += function_decl(callback, spec=True, callback=True)
    out += "\n     with Convention => C;\n"

    if callback["description"]:
        out += f"   --  {callback['description']}\n"
    out += "\n"

    return out


def gen_binding(header_file, package_name, package_file, parser_options=""):
    JSON_FILE = os.path.join(CRATE_ROOT, "scripts", f"{package_file}.json")

    SKIP_CALLBACKS = ["TraceLogCallback"]
    SKIP_STRUCTS = []
    SKIP_FUNCTIONS = [
        "TextFormat",  # Var args...
        "GenImageFontAtlas",
    ]

    os.system(
        f"{CRATE_ROOT}/raylib/parser/raylib_parser -input {header_file} -o {JSON_FILE} -f JSON "
        + parser_options
    )

    with open(JSON_FILE, encoding="utf-8") as file:
        data = json.load(file)

    if package_name == "Raylib.GUI":
        SKIP_STRUCTS += [
            "Texture2D",
            "Vector2",
            "Vector3",
            "Color",
            "Rectangle",
            "Image",
            "GlyphInfo",
            "Font",
        ]

    spec = ""
    body = ""

    spec = "with Interfaces.C;\n"
    spec += "with Interfaces.C.Strings;\n"
    if package_name == "Raylib":
        spec += "with System;\n"

    spec += f"package {package_name}\n"
    spec += "  with Preelaborate\n"
    spec += "is\n"
    spec += '   pragma Style_Checks ("M2000");\n'

    if package_name == "Raylib":
        spec += "   type Float2 is array (0 .. 1) of Interfaces.C.C_float;\n"
        spec += "   type Float4 is array (0 .. 3) of Interfaces.C.C_float;\n"
        spec += "   type Int4 is array (0 .. 3) of Interfaces.C.int;\n"
        spec += "   subtype String32 is String (1 .. 32);\n"
        spec += "   type C_String_Array\n"
        spec += "    is array (Interfaces.C.unsigned) of aliased Interfaces.C.Strings.chars_ptr\n"
        spec += "    with Convention => C;\n"
        spec += "   type C_String_Array_Access is access all C_String_Array;\n"
        spec += "\n"

    if package_name == "Raylib.GUI":
        spec += "   function To_C_String_Array_Access (A : aliased Interfaces.C.Strings.chars_ptr_array) return C_String_Array_Access;\n"

    # Do the enums first as the contain definitions used in structs and functions
    for enum in data["enums"]:
        spec += gen_enum(enum)

    if package_name == "Raylib":
        spec += "   type ShaderLocationArray is array (ShaderLocationIndex) of Interfaces.C.int\n"
        spec += "     with Convention => C;\n"
        spec += "\n"

    if package_name == "Raylib.GUI":
        body += "with Ada.Unchecked_Conversion;\n"

    body += f"package body {package_name} is\n"

    body += '   pragma Style_Checks ("M2000");\n'

    if package_name == "Raylib.GUI":
        body += "   function To_C_String_Array_Access (A : aliased Interfaces.C.Strings.chars_ptr_array) return C_String_Array_Access is\n"
        body += "      function Convert\n"
        body += "      is new Ada.Unchecked_Conversion (System.Address, C_String_Array_Access);\n"
        body += "   begin\n"
        body += "      return Convert (A'Address);\n"
        body += "   end To_C_String_Array_Access;\n"

    for callback in data["callbacks"]:
        if callback["name"] not in SKIP_CALLBACKS:
            spec += gen_callback(callback)

    for struct in data["structs"]:
        if struct["name"] not in SKIP_STRUCTS:
            spec += gen_struct(struct)

    for function in data["functions"]:
        if (
            "TraceLog" not in function["name"]  # Var args...
            and function["name"] not in SKIP_FUNCTIONS
        ):
            f_spec, f_body = gen_function(function)
            spec += f_spec
            body += f_body

    for define in data["defines"]:
        spec += gen_define(define)

    spec += f"end {package_name};\n"
    body += f"end {package_name};\n"

    spec_filename = os.path.join(CRATE_ROOT, "src", package_file + ".ads")
    body_filename = os.path.join(CRATE_ROOT, "src", package_file + ".adb")

    with open(spec_filename, "w", encoding="utf-8") as f:
        print(f"Writing {spec_filename}")
        f.write(spec)

    with open(body_filename, "w", encoding="utf-8") as f:
        print(f"Writing {body_filename}")
        f.write(body)


gen_binding(os.path.join(CRATE_ROOT, "raylib", "src", "raylib.h"), "Raylib", "raylib")
gen_binding(
    os.path.join(CRATE_ROOT, "raygui", "src", "raygui.h"),
    "Raylib.GUI",
    "raylib-gui",
    '-d RAYGUIAPI -t "RAYGUI IMPLEMENTATION"',
)
