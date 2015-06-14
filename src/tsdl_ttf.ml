open Ctypes
open Foreign
open Tsdl

module Ttf = struct

let bool =
  let read = function 0 -> false | _ -> true in
  let write = function true -> 1 | false -> 0 in
  view ~read ~write:write int

let byte_swapped_unicode =
  foreign "TTF_ByteSwappedUNICODE" (int @-> returning void)

let surface =
  view ~read:Sdl.unsafe_surface_of_ptr ~write:Sdl.unsafe_ptr_of_surface nativeint
let surface_opt =
  let read v = if Nativeint.(compare v zero) = 0 then None else Some (Sdl.unsafe_surface_of_ptr v) in
  let write = function | None -> raw_address_of_ptr @@ null | Some s -> Sdl.unsafe_ptr_of_surface s in
  view ~read ~write nativeint

let rw_ops =
  view ~read:Sdl.unsafe_rw_ops_of_ptr ~write:Sdl.unsafe_ptr_of_rw_ops nativeint

type _font
type font = _font structure ptr
let font_struct : _font structure typ = structure "TTF_Font"
let font : _font structure ptr typ = ptr font_struct
let font_opt : _font structure ptr option typ = ptr_opt font_struct

let init = foreign "TTF_Init" (void @-> returning int)

let open_font = foreign "TTF_OpenFont" (string @-> int @-> returning font_opt)
let open_font_index = foreign "TTF_OpenFontIndex" (string @-> int @-> long @-> returning font_opt)
let open_font_rw = foreign "TTF_OpenFontRW" (rw_ops @-> int @-> int @-> returning font_opt)
let open_font_index_rw = foreign "TTF_OpenFontIndexRW" (rw_ops @-> int @-> int @-> long @-> returning font_opt)

module Style = struct
  type t = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let normal = i 0
  let bold = i 1
  let italic = i 2
  let underline = i 4
  let strikethrough = i 8
end
let get_font_style = foreign "TTF_GetFontStyle" (font @-> returning uint32_t)
let set_font_style = foreign "TTF_SetFontStyle" (font @-> uint32_t @-> returning void)

let get_font_outline = foreign "TTF_GetFontOutline" (font @-> returning int)
let set_font_outline = foreign "TTF_SetFontOutline" (font @-> int @-> returning void)

module Hinting = struct
  type t = Normal | Light | Mono | None
  let t =
    let read = function 0 -> Normal | 1 -> Light | 2 -> Mono | 3 -> None | _ -> failwith "Unexpected value" in
    let write = function Normal -> 0 | Light -> 1 | Mono -> 2 | None -> 3 in
    view ~read ~write int
end
let get_font_hinting = foreign "TTF_GetFontHinting" (font @-> returning Hinting.t)
let set_font_hinting = foreign "TTF_SetFontHinting" (font @-> Hinting.t @-> returning void)

let font_height = foreign "TTF_FontHeight" (font @-> returning int)
let font_ascent = foreign "TTF_FontAscent" (font @-> returning int)
let font_descent = foreign "TTF_FontDescent" (font @-> returning int)

let font_line_skip = foreign "TTF_FontLineSkip" (font @-> returning int)

let get_font_kerning = foreign "TTF_GetFontKerning" (font @-> returning bool)
let set_font_kerning = foreign "TTF_SetFontKerning" (font @-> bool @-> returning void)

let font_faces = foreign "TTF_FontFaces" (font @-> returning long)

let font_face_is_fixed_width = foreign "TTF_FontFaceIsFixedWidth" (font @-> returning int)
let font_face_family_name = foreign "TTF_FontFaceFamilyName" (font @-> returning string)
let font_face_style_name = foreign "TTF_FontFaceStyleName" (font @-> returning string)

let glyph_is_provided = foreign "TTF_GlyphIsProvided" (font @-> uint16_t @-> returning bool)

let glyph_metrics = foreign "TTF_GlyphMetrics" (font @-> uint16_t @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> returning int)

let size_text = foreign "TTF_SizeText" (font @-> string @-> ptr int @-> ptr int @-> returning int)
let size_utf8 = foreign "TTF_SizeUTF8" (font @-> string @-> ptr int @-> ptr int @-> returning int)
let size_unicode = foreign "TTF_SizeUNICODE" (font @-> ptr uint16_t @-> ptr int @-> ptr int @-> returning int)

type _color
type color = _color structure
let color : color typ = structure "SDL_Color"
let color_r = field color "r" uint8_t
let color_g = field color "g" uint8_t
let color_b = field color "b" uint8_t
let color_a = field color "a" uint8_t
let () = seal color

let render_text_solid = foreign "TTF_RenderText_Solid" (font @-> string @-> color @-> returning surface_opt)
let render_utf8_solid = foreign "TTF_RenderUTF8_Solid" (font @-> string @-> color @-> returning surface_opt)
let render_unicode_solid = foreign "TTF_RenderUNICODE_Solid" (font @-> ptr uint16_t @-> color @-> returning surface_opt)

let render_glyph_solid = foreign "TTF_RenderGlyph_Solid" (font @-> uint16_t @-> color @-> returning surface_opt)

let render_text_shaded = foreign "TTF_RenderText_Shaded" (font @-> string @-> color @-> color @-> returning surface_opt)
let render_utf8_shaded = foreign "TTF_RenderUTF8_Shaded" (font @-> string @-> color @-> color @-> returning surface_opt)
let render_unicode_shaded = foreign "TTF_RenderUNICODE_Shaded" (font @-> ptr uint16_t @-> color @-> color @-> returning surface_opt)

let render_glyph_shaded = foreign "TTF_RenderGlyph_Shaded" (font @-> uint16_t @-> color @-> color @-> returning surface_opt)

let render_text_blended = foreign "TTF_RenderText_Blended" (font @-> string @-> color @-> returning surface_opt)
let render_utf8_blended = foreign "TTF_RenderUTF8_Blended" (font @-> string @-> color @-> returning surface_opt)
let render_unicode_blended = foreign "TTF_RenderUNICODE_Blended" (font @-> ptr uint16_t @-> color @-> returning surface_opt)

let render_text_blended_wrapped = foreign "TTF_RenderText_Blended_Wrapped" (font @-> string @-> color @-> uint32_t @-> returning surface_opt)
let render_utf8_blended_wrapped = foreign "TTF_RenderUTF8_Blended_Wrapped" (font @-> string @-> color @-> uint32_t @-> returning surface_opt)
let render_unicode_blended_wrapped = foreign "TTF_RenderUNICODE_Blended_Wrapped" (font @-> ptr uint16_t @-> color @-> uint32_t @-> returning surface_opt)

let render_glyph_blended = foreign "TTF_RenderGlyph_Blended" (font @-> uint16_t @-> color @-> returning surface_opt)

let close_font = foreign "TTF_CloseFont" (font @-> returning void)

let quit = foreign "TTF_Quit" (void @-> returning void)

let was_init = foreign "TTF_WasInit" (void @-> returning bool)

let get_font_kerning_size = foreign "TTF_GetFontKerningSize" (font @-> int @-> int @-> returning int)

end
