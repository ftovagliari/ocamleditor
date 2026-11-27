(** Application logo.

    These are the only raster images loaded by the application: a font glyph cannot be used as
    window icon or as the logo of the "About" dialog. Every other icon is a glyph of the icon font,
    see [Icon_set]. *)

let (//) = Filename.concat

module Light = struct
  let path = App_config.application_icons // "light"
  let oe = GdkPixbuf.from_file (path // "oe.png")
  let logo = GdkPixbuf.from_file (path // "logo.png")
end

module Dark = struct
  let path = App_config.application_icons // "dark"
  let oe = GdkPixbuf.from_file (path // "oe.png")
  let logo = GdkPixbuf.from_file (path // "logo.png")
end

(** (light, dark) variants, to be passed to [Preferences.Icon.get_themed_icon]. *)
let oe = Light.oe, Dark.oe

let logo = Light.logo, Dark.logo
